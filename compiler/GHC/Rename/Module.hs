{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Main pass of renamer
-}

module GHC.Rename.Module (
        rnSrcDecls, addTcgDUs, findSplice
    ) where

#include "HsVersions.h"

import GHC.Prelude

import {-# SOURCE #-} GHC.Rename.Expr( rnLExpr )
import {-# SOURCE #-} GHC.Rename.Splice ( rnSpliceDecl, rnTopSpliceDecls )

import GHC.Hs
import GHC.Types.FieldLabel
import GHC.Types.Name.Reader
import GHC.Rename.HsType
import GHC.Rename.Bind
import GHC.Rename.Env
import GHC.Rename.Utils ( HsDocContext(..), mapFvRn, bindLocalNames
                        , checkDupRdrNames, bindLocalNamesFV
                        , checkShadowedRdrNames, warnUnusedTypePatterns
                        , extendTyVarEnvFVRn, newLocalBndrsRn
                        , withHsDocContext, noNestedForallsContextsErr
                        , addNoNestedForallsContextsErr, checkInferredVars )
import GHC.Rename.Unbound ( mkUnboundName, notInScopeErr )
import GHC.Rename.Names
import GHC.Tc.Gen.Annotation ( annCtxt )
import GHC.Tc.Utils.Monad

import GHC.Types.ForeignCall ( CCallTarget(..) )
import GHC.Unit
import GHC.Unit.Module.Warnings
import GHC.Builtin.Names( applicativeClassName, pureAName, thenAName
                        , monadClassName, returnMName, thenMName
                        , semigroupClassName, sappendName
                        , monoidClassName, mappendName
                        )
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.Avail
import GHC.Utils.Outputable
import GHC.Data.Bag
import GHC.Types.Basic  ( pprRuleName, TypeOrKind(..) )
import GHC.Data.FastString
import GHC.Types.SrcLoc as SrcLoc
import GHC.Driver.Session
import GHC.Utils.Misc   ( debugIsOn, lengthExceeds, partitionWith )
import GHC.Utils.Panic
import GHC.Driver.Env ( HscEnv(..), hsc_home_unit)
import GHC.Data.List.SetOps ( findDupsEq, removeDups, equivClasses )
import GHC.Data.Graph.Directed ( SCC, flattenSCC, flattenSCCs, Node(..)
                               , stronglyConnCompFromEdgedVerticesUniq )
import GHC.Types.Unique.Set
import GHC.Data.OrdList
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Control.Arrow ( first )
import Data.List ( mapAccumL )
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe ( isNothing, fromMaybe, mapMaybe )
import qualified Data.Set as Set ( difference, fromList, toList, null )
import Data.Function ( on )

{- | @rnSourceDecl@ "renames" declarations.
It simultaneously performs dependency analysis and precedence parsing.
It also does the following error checks:

* Checks that tyvars are used properly. This includes checking
  for undefined tyvars, and tyvars in contexts that are ambiguous.
  (Some of this checking has now been moved to module @TcMonoType@,
  since we don't have functional dependency information at this point.)

* Checks that all variable occurrences are defined.

* Checks the @(..)@ etc constraints in the export list.

Brings the binders of the group into scope in the appropriate places;
does NOT assume that anything is in scope already
-}
rnSrcDecls :: HsGroup GhcPs -> RnM (TcGblEnv, HsGroup GhcRn)
-- Rename a top-level HsGroup; used for normal source files *and* hs-boot files
rnSrcDecls group@(HsGroup { hs_valds   = val_decls,
                            hs_splcds  = splice_decls,
                            hs_tyclds  = tycl_decls,
                            hs_derivds = deriv_decls,
                            hs_fixds   = fix_decls,
                            hs_warnds  = warn_decls,
                            hs_annds   = ann_decls,
                            hs_fords   = foreign_decls,
                            hs_defds   = default_decls,
                            hs_ruleds  = rule_decls,
                            hs_docs    = docs })
 = do {
   -- (A) Process the top-level fixity declarations, creating a mapping from
   --     FastStrings to FixItems. Also checks for duplicates.
   --     See Note [Top-level fixity signatures in an HsGroup] in GHC.Hs.Decls
   local_fix_env <- makeMiniFixityEnv $ hsGroupTopLevelFixitySigs group ;

   -- (B) Bring top level binders (and their fixities) into scope,
   --     *except* for the value bindings, which get done in step (D)
   --     with collectHsIdBinders. However *do* include
   --
   --        * Class ops, data constructors, and record fields,
   --          because they do not have value declarations.
   --
   --        * For hs-boot files, include the value signatures
   --          Again, they have no value declarations
   --
   (tc_envs, tc_bndrs) <- getLocalNonValBinders local_fix_env group ;


   setEnvs tc_envs $ do {

   failIfErrsM ; -- No point in continuing if (say) we have duplicate declarations

   -- (D1) Bring pattern synonyms into scope.
   --      Need to do this before (D2) because rnTopBindsLHS
   --      looks up those pattern synonyms (#9889)

   extendPatSynEnv val_decls local_fix_env $ \pat_syn_bndrs -> do {

   -- (D2) Rename the left-hand sides of the value bindings.
   --     This depends on everything from (B) being in scope.
   --     It uses the fixity env from (A) to bind fixities for view patterns.
   new_lhs <- rnTopBindsLHS local_fix_env val_decls ;

   -- Bind the LHSes (and their fixities) in the global rdr environment
   let { id_bndrs = collectHsIdBinders new_lhs } ;  -- Excludes pattern-synonym binders
                                                    -- They are already in scope
   traceRn "rnSrcDecls" (ppr id_bndrs) ;
   tc_envs <- extendGlobalRdrEnvRn (map avail id_bndrs) local_fix_env ;
   setEnvs tc_envs $ do {

   --  Now everything is in scope, as the remaining renaming assumes.

   -- (E) Rename type and class decls
   --     (note that value LHSes need to be in scope for default methods)
   --
   -- You might think that we could build proper def/use information
   -- for type and class declarations, but they can be involved
   -- in mutual recursion across modules, and we only do the SCC
   -- analysis for them in the type checker.
   -- So we content ourselves with gathering uses only; that
   -- means we'll only report a declaration as unused if it isn't
   -- mentioned at all.  Ah well.
   traceRn "Start rnTyClDecls" (ppr tycl_decls) ;
   (rn_tycl_decls, src_fvs1) <- rnTyClDecls tycl_decls ;

   -- (F) Rename Value declarations right-hand sides
   traceRn "Start rnmono" empty ;
   let { val_bndr_set = mkNameSet id_bndrs `unionNameSet` mkNameSet pat_syn_bndrs } ;
   is_boot <- tcIsHsBootOrSig ;
   (rn_val_decls, bind_dus) <- if is_boot
    -- For an hs-boot, use tc_bndrs (which collects how we're renamed
    -- signatures), since val_bndr_set is empty (there are no x = ...
    -- bindings in an hs-boot.)
    then rnTopBindsBoot tc_bndrs new_lhs
    else rnValBindsRHS (TopSigCtxt val_bndr_set) new_lhs ;
   traceRn "finish rnmono" (ppr rn_val_decls) ;

   -- (G) Rename Fixity and deprecations

   -- Rename fixity declarations and error if we try to
   -- fix something from another module (duplicates were checked in (A))
   let { all_bndrs = tc_bndrs `unionNameSet` val_bndr_set } ;
   rn_fix_decls <- mapM (mapM (rnSrcFixityDecl (TopSigCtxt all_bndrs)))
                        fix_decls ;

   -- Rename deprec decls;
   -- check for duplicates and ensure that deprecated things are defined locally
   -- at the moment, we don't keep these around past renaming
   rn_warns <- rnSrcWarnDecls all_bndrs warn_decls ;

   -- (H) Rename Everything else

   (rn_rule_decls,    src_fvs2) <- setXOptM LangExt.ScopedTypeVariables $
                                   rnList rnHsRuleDecls rule_decls ;
                           -- Inside RULES, scoped type variables are on
   (rn_foreign_decls, src_fvs3) <- rnList rnHsForeignDecl foreign_decls ;
   (rn_ann_decls,     src_fvs4) <- rnList rnAnnDecl       ann_decls ;
   (rn_default_decls, src_fvs5) <- rnList rnDefaultDecl   default_decls ;
   (rn_deriv_decls,   src_fvs6) <- rnList rnSrcDerivDecl  deriv_decls ;
   (rn_splice_decls,  src_fvs7) <- rnList rnSpliceDecl    splice_decls ;

   last_tcg_env <- getGblEnv ;
   -- (I) Compute the results and return
   let {rn_group = HsGroup { hs_ext     = noExtField,
                             hs_valds   = rn_val_decls,
                             hs_splcds  = rn_splice_decls,
                             hs_tyclds  = rn_tycl_decls,
                             hs_derivds = rn_deriv_decls,
                             hs_fixds   = rn_fix_decls,
                             hs_warnds  = [], -- warns are returned in the tcg_env
                                             -- (see below) not in the HsGroup
                             hs_fords  = rn_foreign_decls,
                             hs_annds  = rn_ann_decls,
                             hs_defds  = rn_default_decls,
                             hs_ruleds = rn_rule_decls,
                             hs_docs   = docs } ;

        tcf_bndrs = hsTyClForeignBinders rn_tycl_decls rn_foreign_decls ;
        other_def  = (Just (mkNameSet tcf_bndrs), emptyNameSet) ;
        other_fvs  = plusFVs [src_fvs1, src_fvs2, src_fvs3, src_fvs4,
                              src_fvs5, src_fvs6, src_fvs7] ;
                -- It is tiresome to gather the binders from type and class decls

        src_dus = unitOL other_def `plusDU` bind_dus `plusDU` usesOnly other_fvs ;
                -- Instance decls may have occurrences of things bound in bind_dus
                -- so we must put other_fvs last

        final_tcg_env = let tcg_env' = (last_tcg_env `addTcgDUs` src_dus)
                        in -- we return the deprecs in the env, not in the HsGroup above
                        tcg_env' { tcg_warns = tcg_warns tcg_env' `plusWarns` rn_warns };
       } ;
   traceRn "finish rnSrc" (ppr rn_group) ;
   traceRn "finish Dus" (ppr src_dus ) ;
   return (final_tcg_env, rn_group)
                    }}}}

addTcgDUs :: TcGblEnv -> DefUses -> TcGblEnv
-- This function could be defined lower down in the module hierarchy,
-- but there doesn't seem anywhere very logical to put it.
addTcgDUs tcg_env dus = tcg_env { tcg_dus = tcg_dus tcg_env `plusDU` dus }

rnList :: (a -> RnM (b, FreeVars)) -> [Located a] -> RnM ([Located b], FreeVars)
rnList f xs = mapFvRn (wrapLocFstM f) xs

{-
*********************************************************
*                                                       *
        Source-code deprecations declarations
*                                                       *
*********************************************************

Check that the deprecated names are defined, are defined locally, and
that there are no duplicate deprecations.

It's only imported deprecations, dealt with in RnIfaces, that we
gather them together.
-}

-- checks that the deprecations are defined locally, and that there are no duplicates
rnSrcWarnDecls :: NameSet -> [LWarnDecls GhcPs] -> RnM Warnings
rnSrcWarnDecls _ []
  = return NoWarnings

rnSrcWarnDecls bndr_set decls'
  = do { -- check for duplicates
       ; mapM_ (\ dups -> let ((L loc rdr) :| (lrdr':_)) = dups
                          in addErrAt loc (dupWarnDecl lrdr' rdr))
               warn_rdr_dups
       ; pairs_s <- mapM (addLocM rn_deprec) decls
       ; return (WarnSome ((concat pairs_s))) }
 where
   decls = concatMap (wd_warnings . unLoc) decls'

   sig_ctxt = TopSigCtxt bndr_set

   rn_deprec (Warning _ rdr_names txt)
       -- ensures that the names are defined locally
     = do { names <- concatMapM (lookupLocalTcNames sig_ctxt what . unLoc)
                                rdr_names
          ; return [(rdrNameOcc rdr, txt) | (rdr, _) <- names] }

   what = text "deprecation"

   warn_rdr_dups = findDupRdrNames
                   $ concatMap (\(L _ (Warning _ ns _)) -> ns) decls

findDupRdrNames :: [Located RdrName] -> [NonEmpty (Located RdrName)]
findDupRdrNames = findDupsEq (\ x -> \ y -> rdrNameOcc (unLoc x) == rdrNameOcc (unLoc y))

-- look for duplicates among the OccNames;
-- we check that the names are defined above
-- invt: the lists returned by findDupsEq always have at least two elements

dupWarnDecl :: Located RdrName -> RdrName -> SDoc
-- Located RdrName -> DeprecDecl RdrName -> SDoc
dupWarnDecl d rdr_name
  = vcat [text "Multiple warning declarations for" <+> quotes (ppr rdr_name),
          text "also at " <+> ppr (getLoc d)]

{-
*********************************************************
*                                                      *
\subsection{Annotation declarations}
*                                                      *
*********************************************************
-}

rnAnnDecl :: AnnDecl GhcPs -> RnM (AnnDecl GhcRn, FreeVars)
rnAnnDecl ann@(HsAnnotation _ s provenance expr)
  = addErrCtxt (annCtxt ann) $
    do { (provenance', provenance_fvs) <- rnAnnProvenance provenance
       ; (expr', expr_fvs) <- setStage (Splice Untyped) $
                              rnLExpr expr
       ; return (HsAnnotation noExtField s provenance' expr',
                 provenance_fvs `plusFV` expr_fvs) }

rnAnnProvenance :: AnnProvenance RdrName
                -> RnM (AnnProvenance Name, FreeVars)
rnAnnProvenance provenance = do
    provenance' <- traverse lookupTopBndrRn provenance
    return (provenance', maybe emptyFVs unitFV (annProvenanceName_maybe provenance'))

{-
*********************************************************
*                                                      *
\subsection{Default declarations}
*                                                      *
*********************************************************
-}

rnDefaultDecl :: DefaultDecl GhcPs -> RnM (DefaultDecl GhcRn, FreeVars)
rnDefaultDecl (DefaultDecl _ tys)
  = do { (tys', fvs) <- rnLHsTypes doc_str tys
       ; return (DefaultDecl noExtField tys', fvs) }
  where
    doc_str = DefaultDeclCtx

{-
*********************************************************
*                                                      *
\subsection{Foreign declarations}
*                                                      *
*********************************************************
-}

rnHsForeignDecl :: ForeignDecl GhcPs -> RnM (ForeignDecl GhcRn, FreeVars)
rnHsForeignDecl (ForeignImport { fd_name = name, fd_sig_ty = ty, fd_fi = spec })
  = do { topEnv :: HscEnv <- getTopEnv
       ; name' <- lookupLocatedTopBndrRn name
       ; (ty', fvs) <- rnHsSigType (ForeignDeclCtx name) TypeLevel ty

        -- Mark any PackageTarget style imports as coming from the current package
       ; let home_unit = hsc_home_unit topEnv
             spec'  = patchForeignImport (homeUnitAsUnit home_unit) spec

       ; return (ForeignImport { fd_i_ext = noExtField
                               , fd_name = name', fd_sig_ty = ty'
                               , fd_fi = spec' }, fvs) }

rnHsForeignDecl (ForeignExport { fd_name = name, fd_sig_ty = ty, fd_fe = spec })
  = do { name' <- lookupLocatedOccRn name
       ; (ty', fvs) <- rnHsSigType (ForeignDeclCtx name) TypeLevel ty
       ; return (ForeignExport { fd_e_ext = noExtField
                               , fd_name = name', fd_sig_ty = ty'
                               , fd_fe = spec }
                , fvs `addOneFV` unLoc name') }
        -- NB: a foreign export is an *occurrence site* for name, so
        --     we add it to the free-variable list.  It might, for example,
        --     be imported from another module

-- | For Windows DLLs we need to know what packages imported symbols are from
--      to generate correct calls. Imported symbols are tagged with the current
--      package, so if they get inlined across a package boundary we'll still
--      know where they're from.
--
patchForeignImport :: Unit -> ForeignImport -> ForeignImport
patchForeignImport unit (CImport cconv safety fs spec src)
        = CImport cconv safety fs (patchCImportSpec unit spec) src

patchCImportSpec :: Unit -> CImportSpec -> CImportSpec
patchCImportSpec unit spec
 = case spec of
        CFunction callTarget    -> CFunction $ patchCCallTarget unit callTarget
        _                       -> spec

patchCCallTarget :: Unit -> CCallTarget -> CCallTarget
patchCCallTarget unit callTarget =
  case callTarget of
  StaticTarget src label Nothing isFun
                              -> StaticTarget src label (Just unit) isFun
  _                           -> callTarget

{-
*********************************************************
*                                                      *
\subsection{Instance declarations}
*                                                      *
*********************************************************
-}

rnSrcInstDecl :: InstDecl GhcPs -> RnM (InstDecl GhcRn, FreeVars)
rnSrcInstDecl (TyFamInstD { tfid_inst = tfi })
  = do { (tfi', fvs) <- rnTyFamInstDecl (NonAssocTyFamEqn NotClosedTyFam) tfi
       ; return (TyFamInstD { tfid_ext = noExtField, tfid_inst = tfi' }, fvs) }

rnSrcInstDecl (DataFamInstD { dfid_inst = dfi })
  = do { (dfi', fvs) <- rnDataFamInstDecl (NonAssocTyFamEqn NotClosedTyFam) dfi
       ; return (DataFamInstD { dfid_ext = noExtField, dfid_inst = dfi' }, fvs) }

rnSrcInstDecl (ClsInstD { cid_inst = cid })
  = do { traceRn "rnSrcIstDecl {" (ppr cid)
       ; (cid', fvs) <- rnClsInstDecl cid
       ; traceRn "rnSrcIstDecl end }" empty
       ; return (ClsInstD { cid_d_ext = noExtField, cid_inst = cid' }, fvs) }

-- | Warn about non-canonical typeclass instance declarations
--
-- A "non-canonical" instance definition can occur for instances of a
-- class which redundantly defines an operation its superclass
-- provides as well (c.f. `return`/`pure`). In such cases, a canonical
-- instance is one where the subclass inherits its method
-- implementation from its superclass instance (usually the subclass
-- has a default method implementation to that effect). Consequently,
-- a non-canonical instance occurs when this is not the case.
--
-- See also descriptions of 'checkCanonicalMonadInstances' and
-- 'checkCanonicalMonoidInstances'
checkCanonicalInstances :: Name -> LHsSigType GhcRn -> LHsBinds GhcRn -> RnM ()
checkCanonicalInstances cls poly_ty mbinds = do
    whenWOptM Opt_WarnNonCanonicalMonadInstances
        $ checkCanonicalMonadInstances
        "https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return"

    whenWOptM Opt_WarnNonCanonicalMonoidInstances
        $ checkCanonicalMonoidInstances
        "https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/semigroup-monoid"

  where
    -- | Warn about unsound/non-canonical 'Applicative'/'Monad' instance
    -- declarations. Specifically, the following conditions are verified:
    --
    -- In 'Monad' instances declarations:
    --
    --  * If 'return' is overridden it must be canonical (i.e. @return = pure@)
    --  * If '(>>)' is overridden it must be canonical (i.e. @(>>) = (*>)@)
    --
    -- In 'Applicative' instance declarations:
    --
    --  * Warn if 'pure' is defined backwards (i.e. @pure = return@).
    --  * Warn if '(*>)' is defined backwards (i.e. @(*>) = (>>)@).
    --
    checkCanonicalMonadInstances refURL
      | cls == applicativeClassName =
          forM_ (bagToList mbinds) $ \(L loc mbind) -> setSrcSpan loc $
              case mbind of
                  FunBind { fun_id = L _ name
                          , fun_matches = mg }
                      | name == pureAName, isAliasMG mg == Just returnMName
                      -> addWarnNonCanonicalMethod1 refURL
                            Opt_WarnNonCanonicalMonadInstances "pure" "return"

                      | name == thenAName, isAliasMG mg == Just thenMName
                      -> addWarnNonCanonicalMethod1 refURL
                            Opt_WarnNonCanonicalMonadInstances "(*>)" "(>>)"

                  _ -> return ()

      | cls == monadClassName =
          forM_ (bagToList mbinds) $ \(L loc mbind) -> setSrcSpan loc $
              case mbind of
                  FunBind { fun_id = L _ name
                          , fun_matches = mg }
                      | name == returnMName, isAliasMG mg /= Just pureAName
                      -> addWarnNonCanonicalMethod2 refURL
                            Opt_WarnNonCanonicalMonadInstances "return" "pure"

                      | name == thenMName, isAliasMG mg /= Just thenAName
                      -> addWarnNonCanonicalMethod2 refURL
                            Opt_WarnNonCanonicalMonadInstances "(>>)" "(*>)"

                  _ -> return ()

      | otherwise = return ()

    -- | Check whether Monoid(mappend) is defined in terms of
    -- Semigroup((<>)) (and not the other way round). Specifically,
    -- the following conditions are verified:
    --
    -- In 'Monoid' instances declarations:
    --
    --  * If 'mappend' is overridden it must be canonical
    --    (i.e. @mappend = (<>)@)
    --
    -- In 'Semigroup' instance declarations:
    --
    --  * Warn if '(<>)' is defined backwards (i.e. @(<>) = mappend@).
    --
    checkCanonicalMonoidInstances refURL
      | cls == semigroupClassName =
          forM_ (bagToList mbinds) $ \(L loc mbind) -> setSrcSpan loc $
              case mbind of
                  FunBind { fun_id      = L _ name
                          , fun_matches = mg }
                      | name == sappendName, isAliasMG mg == Just mappendName
                      -> addWarnNonCanonicalMethod1 refURL
                            Opt_WarnNonCanonicalMonoidInstances "(<>)" "mappend"

                  _ -> return ()

      | cls == monoidClassName =
          forM_ (bagToList mbinds) $ \(L loc mbind) -> setSrcSpan loc $
              case mbind of
                  FunBind { fun_id = L _ name
                          , fun_matches = mg }
                      | name == mappendName, isAliasMG mg /= Just sappendName
                      -> addWarnNonCanonicalMethod2 refURL
                            Opt_WarnNonCanonicalMonoidInstances
                            "mappend" "(<>)"

                  _ -> return ()

      | otherwise = return ()

    -- | test whether MatchGroup represents a trivial \"lhsName = rhsName\"
    -- binding, and return @Just rhsName@ if this is the case
    isAliasMG :: MatchGroup GhcRn (LHsExpr GhcRn) -> Maybe Name
    isAliasMG MG {mg_alts = (L _ [L _ (Match { m_pats = []
                                             , m_grhss = grhss })])}
        | GRHSs _ [L _ (GRHS _ [] body)] lbinds <- grhss
        , EmptyLocalBinds _ <- unLoc lbinds
        , HsVar _ lrhsName  <- unLoc body  = Just (unLoc lrhsName)
    isAliasMG _ = Nothing

    -- got "lhs = rhs" but expected something different
    addWarnNonCanonicalMethod1 refURL flag lhs rhs =
        addWarn (Reason flag) $ vcat
                       [ text "Noncanonical" <+>
                         quotes (text (lhs ++ " = " ++ rhs)) <+>
                         text "definition detected"
                       , instDeclCtxt1 poly_ty
                       , text "Move definition from" <+>
                         quotes (text rhs) <+>
                         text "to" <+> quotes (text lhs)
                       , text "See also:" <+>
                         text refURL
                       ]

    -- expected "lhs = rhs" but got something else
    addWarnNonCanonicalMethod2 refURL flag lhs rhs =
        addWarn (Reason flag) $ vcat
                       [ text "Noncanonical" <+>
                         quotes (text lhs) <+>
                         text "definition detected"
                       , instDeclCtxt1 poly_ty
                       , quotes (text lhs) <+>
                         text "will eventually be removed in favour of" <+>
                         quotes (text rhs)
                       , text "Either remove definition for" <+>
                         quotes (text lhs) <+> text "(recommended)" <+>
                         text "or define as" <+>
                         quotes (text (lhs ++ " = " ++ rhs))
                       , text "See also:" <+>
                         text refURL
                       ]

    -- stolen from GHC.Tc.TyCl.Instance
    instDeclCtxt1 :: LHsSigType GhcRn -> SDoc
    instDeclCtxt1 hs_inst_ty
      = inst_decl_ctxt (ppr (getLHsInstDeclHead hs_inst_ty))

    inst_decl_ctxt :: SDoc -> SDoc
    inst_decl_ctxt doc = hang (text "in the instance declaration for")
                         2 (quotes doc <> text ".")


rnClsInstDecl :: ClsInstDecl GhcPs -> RnM (ClsInstDecl GhcRn, FreeVars)
rnClsInstDecl (ClsInstDecl { cid_poly_ty = inst_ty, cid_binds = mbinds
                           , cid_sigs = uprags, cid_tyfam_insts = ats
                           , cid_overlap_mode = oflag
                           , cid_datafam_insts = adts })
  = do { checkInferredVars ctxt inf_err inst_ty
       ; (inst_ty', inst_fvs) <- rnHsSigType ctxt TypeLevel inst_ty
       ; let (ktv_names, _, head_ty') = splitLHsInstDeclTy inst_ty'
             -- Check if there are any nested `forall`s or contexts, which are
             -- illegal in the type of an instance declaration (see
             -- Note [No nested foralls or contexts in instance types] in
             -- GHC.Hs.Type)...
             mb_nested_msg = noNestedForallsContextsErr
                               (text "Instance head") head_ty'
             -- ...then check if the instance head is actually headed by a
             -- class type constructor...
             eith_cls = case hsTyGetAppHead_maybe head_ty' of
               Just (L _ cls) -> Right cls
               Nothing        -> Left
                 ( getLoc head_ty'
                 , hang (text "Illegal head of an instance declaration:"
                           <+> quotes (ppr head_ty'))
                      2 (vcat [ text "Instance heads must be of the form"
                              , nest 2 $ text "C ty_1 ... ty_n"
                              , text "where" <+> quotes (char 'C')
                                <+> text "is a class"
                              ])
                 )
         -- ...finally, attempt to retrieve the class type constructor, failing
         -- with an error message if there isn't one. To avoid excessive
         -- amounts of error messages, we will only report one of the errors
         -- from mb_nested_msg or eith_cls at a time.
       ; cls <- case (mb_nested_msg, eith_cls) of
           (Nothing,   Right cls) -> pure cls
           (Just err1, _)         -> bail_out err1
           (_,         Left err2) -> bail_out err2

          -- Rename the bindings
          -- The typechecker (not the renamer) checks that all
          -- the bindings are for the right class
          -- (Slightly strangely) when scoped type variables are on, the
          -- forall-d tyvars scope over the method bindings too
       ; (mbinds', uprags', meth_fvs) <- rnMethodBinds False cls ktv_names mbinds uprags

       ; checkCanonicalInstances cls inst_ty' mbinds'

       -- Rename the associated types, and type signatures
       -- Both need to have the instance type variables in scope
       ; traceRn "rnSrcInstDecl" (ppr inst_ty' $$ ppr ktv_names)
       ; ((ats', adts'), more_fvs)
             <- extendTyVarEnvFVRn ktv_names $
                do { (ats',  at_fvs)  <- rnATInstDecls rnTyFamInstDecl cls ktv_names ats
                   ; (adts', adt_fvs) <- rnATInstDecls rnDataFamInstDecl cls ktv_names adts
                   ; return ( (ats', adts'), at_fvs `plusFV` adt_fvs) }

       ; let all_fvs = meth_fvs `plusFV` more_fvs
                                `plusFV` inst_fvs
       ; return (ClsInstDecl { cid_ext = noExtField
                             , cid_poly_ty = inst_ty', cid_binds = mbinds'
                             , cid_sigs = uprags', cid_tyfam_insts = ats'
                             , cid_overlap_mode = oflag
                             , cid_datafam_insts = adts' },
                 all_fvs) }
             -- We return the renamed associated data type declarations so
             -- that they can be entered into the list of type declarations
             -- for the binding group, but we also keep a copy in the instance.
             -- The latter is needed for well-formedness checks in the type
             -- checker (eg, to ensure that all ATs of the instance actually
             -- receive a declaration).
             -- NB: Even the copies in the instance declaration carry copies of
             --     the instance context after renaming.  This is a bit
             --     strange, but should not matter (and it would be more work
             --     to remove the context).
  where
    ctxt    = GenericCtx $ text "an instance declaration"
    inf_err = Just (text "Inferred type variables are not allowed")

    -- The instance is malformed. We'd still like to make *some* progress
    -- (rather than failing outright), so we report an error and continue for
    -- as long as we can. Importantly, this error should be thrown before we
    -- reach the typechecker, lest we encounter different errors that are
    -- hopelessly confusing (such as the one in #16114).
    bail_out (l, err_msg) = do
      addErrAt l $ withHsDocContext ctxt err_msg
      pure $ mkUnboundName (mkTcOccFS (fsLit "<class>"))

rnFamEqn :: HsDocContext
         -> AssocTyFamInfo
         -> FreeKiTyVars
         -- ^ Additional kind variables to implicitly bind if there is no
         --   explicit forall. (See the comments on @all_imp_vars@ below for a
         --   more detailed explanation.)
         -> FamEqn GhcPs rhs
         -> (HsDocContext -> rhs -> RnM (rhs', FreeVars))
         -> RnM (FamEqn GhcRn rhs', FreeVars)
rnFamEqn doc atfi extra_kvars
    (FamEqn { feqn_tycon  = tycon
            , feqn_bndrs  = outer_bndrs
            , feqn_pats   = pats
            , feqn_fixity = fixity
            , feqn_rhs    = payload }) rn_payload
  = do { tycon' <- lookupFamInstName mb_cls tycon

         -- all_imp_vars represent the implicitly bound type variables. This is
         -- empty if we have an explicit `forall` (see
         -- Note [forall-or-nothing rule] in GHC.Hs.Type), which means
         -- ignoring:
         --
         -- - pat_kity_vars, the free variables mentioned in the type patterns
         --   on the LHS of the equation, and
         -- - extra_kvars, which is one of the following:
         --   * For type family instances, extra_kvars are the free kind
         --     variables mentioned in an outermost kind signature on the RHS
         --     of the equation.
         --     (See Note [Implicit quantification in type synonyms] in
         --     GHC.Rename.HsType.)
         --   * For data family instances, extra_kvars are the free kind
         --     variables mentioned in the explicit return kind, if one is
         --     provided. (e.g., the `k` in `data instance T :: k -> Type`).
         --
         -- Some examples:
         --
         -- @
         -- type family F a b
         -- type instance forall a b c. F [(a, b)] c = a -> b -> c
         --   -- all_imp_vars = []
         -- type instance F [(a, b)] c = a -> b -> c
         --   -- all_imp_vars = [a, b, c]
         --
         -- type family G :: Maybe a
         -- type instance forall a. G = (Nothing :: Maybe a)
         --   -- all_imp_vars = []
         -- type instance G = (Nothing :: Maybe a)
         --   -- all_imp_vars = [a]
         --
         -- data family H :: k -> Type
         -- data instance forall k. H :: k -> Type where ...
         --   -- all_imp_vars = []
         -- data instance H :: k -> Type where ...
         --   -- all_imp_vars = [k]
         -- @
       ; let all_imp_vars = pat_kity_vars ++ extra_kvars

       ; bindHsOuterTyVarBndrs doc mb_cls all_imp_vars outer_bndrs $ \rn_outer_bndrs ->
    do { (pats', pat_fvs) <- rnLHsTypeArgs (FamPatCtx tycon) pats
       ; (payload', rhs_fvs) <- rn_payload doc payload

          -- Report unused binders on the LHS
          -- See Note [Unused type variables in family instances]
       ; let -- The SrcSpan that bindHsOuterFamEqnTyVarBndrs will attach to each
             -- implicitly bound type variable Name in outer_bndrs' will
             -- span the entire type family instance, which will be reflected in
             -- -Wunused-type-patterns warnings. We can be a little more precise
             -- than that by pointing to the LHS of the instance instead, which
             -- is what lhs_loc corresponds to.
             rn_outer_bndrs' = mapHsOuterImplicit (map (`setNameLoc` lhs_loc))
                                                  rn_outer_bndrs

             groups :: [NonEmpty (Located RdrName)]
             groups = equivClasses cmpLocated pat_kity_vars
       ; nms_dups <- mapM (lookupOccRn . unLoc) $
                        [ tv | (tv :| (_:_)) <- groups ]
             -- Add to the used variables
             --  a) any variables that appear *more than once* on the LHS
             --     e.g.   F a Int a = Bool
             --  b) for associated instances, the variables
             --     of the instance decl.  See
             --     Note [Unused type variables in family instances]
       ; let nms_used = extendNameSetList rhs_fvs $
                           nms_dups {- (a) -} ++ inst_head_tvs {- (b) -}
             all_nms = hsOuterTyVarNames rn_outer_bndrs'
       ; warnUnusedTypePatterns all_nms nms_used

         -- For associated family instances, if a type variable from the
         -- parent instance declaration is mentioned on the RHS of the
         -- associated family instance but not bound on the LHS, then reject
         -- that type variable as being out of scope.
         -- See Note [Renaming associated types]
       ; let lhs_bound_vars = extendNameSetList pat_fvs all_nms
             improperly_scoped cls_tkv =
                  cls_tkv `elemNameSet` rhs_fvs
                    -- Mentioned on the RHS...
               && not (cls_tkv `elemNameSet` lhs_bound_vars)
                    -- ...but not bound on the LHS.
             bad_tvs = filter improperly_scoped inst_head_tvs
       ; unless (null bad_tvs) (badAssocRhs bad_tvs)

       ; let eqn_fvs = rhs_fvs `plusFV` pat_fvs
             -- See Note [Type family equations and occurrences]
             all_fvs = case atfi of
                         NonAssocTyFamEqn ClosedTyFam
                           -> eqn_fvs
                         _ -> eqn_fvs `addOneFV` unLoc tycon'

       ; return (FamEqn { feqn_ext    = noExtField
                        , feqn_tycon  = tycon'
                          -- Note [Wildcards in family instances]
                        , feqn_bndrs  = rn_outer_bndrs'
                        , feqn_pats   = pats'
                        , feqn_fixity = fixity
                        , feqn_rhs    = payload' },
                 all_fvs) } }
  where
    -- The parent class, if we are dealing with an associated type family
    -- instance.
    mb_cls = case atfi of
      NonAssocTyFamEqn _   -> Nothing
      AssocTyFamDeflt cls  -> Just cls
      AssocTyFamInst cls _ -> Just cls

    -- The type variables from the instance head, if we are dealing with an
    -- associated type family instance.
    inst_head_tvs = case atfi of
      NonAssocTyFamEqn _             -> []
      AssocTyFamDeflt _              -> []
      AssocTyFamInst _ inst_head_tvs -> inst_head_tvs

    pat_kity_vars = extractHsTyArgRdrKiTyVars pats
             -- It is crucial that extractHsTyArgRdrKiTyVars return
             -- duplicate occurrences, since they're needed to help
             -- determine unused binders on the LHS.

    -- The SrcSpan of the LHS of the instance. For example, lhs_loc would be
    -- the highlighted part in the example below:
    --
    --   type instance F a b c = Either a b
    --                   ^^^^^
    lhs_loc = case map lhsTypeArgSrcSpan pats ++ map getLoc extra_kvars of
      []         -> panic "rnFamEqn.lhs_loc"
      [loc]      -> loc
      (loc:locs) -> loc `combineSrcSpans` last locs

    badAssocRhs :: [Name] -> RnM ()
    badAssocRhs ns
      = addErr (hang (text "The RHS of an associated type declaration mentions"
                      <+> text "out-of-scope variable" <> plural ns
                      <+> pprWithCommas (quotes . ppr) ns)
                   2 (text "All such variables must be bound on the LHS"))

rnTyFamInstDecl :: AssocTyFamInfo
                -> TyFamInstDecl GhcPs
                -> RnM (TyFamInstDecl GhcRn, FreeVars)
rnTyFamInstDecl atfi (TyFamInstDecl { tfid_eqn = eqn })
  = do { (eqn', fvs) <- rnTyFamInstEqn atfi eqn
       ; return (TyFamInstDecl { tfid_eqn = eqn' }, fvs) }

-- | Tracks whether we are renaming:
--
-- 1. A type family equation that is not associated
--    with a parent type class ('NonAssocTyFamEqn'). Examples:
--
--    @
--    type family F a
--    type instance F Int = Bool  -- NonAssocTyFamEqn NotClosed
--
--    type family G a where
--       G Int = Bool             -- NonAssocTyFamEqn Closed
--    @
--
-- 2. An associated type family default declaration ('AssocTyFamDeflt').
--    Example:
--
--    @
--    class C a where
--      type A a
--      type instance A a = a -> a  -- AssocTyFamDeflt C
--    @
--
-- 3. An associated type family instance declaration ('AssocTyFamInst').
--    Example:
--
--    @
--    instance C a => C [a] where
--      type A [a] = Bool  -- AssocTyFamInst C [a]
--    @
data AssocTyFamInfo
  = NonAssocTyFamEqn
      ClosedTyFamInfo -- Is this a closed type family?
  | AssocTyFamDeflt
      Name            -- Name of the parent class
  | AssocTyFamInst
      Name            -- Name of the parent class
      [Name]          -- Names of the tyvars of the parent instance decl

-- | Tracks whether we are renaming an equation in a closed type family
-- equation ('ClosedTyFam') or not ('NotClosedTyFam').
data ClosedTyFamInfo
  = NotClosedTyFam
  | ClosedTyFam

rnTyFamInstEqn :: AssocTyFamInfo
               -> TyFamInstEqn GhcPs
               -> RnM (TyFamInstEqn GhcRn, FreeVars)
rnTyFamInstEqn atfi eqn@(FamEqn { feqn_tycon = tycon, feqn_rhs = rhs })
  = rnFamEqn (TySynCtx tycon) atfi extra_kvs eqn rnTySyn
  where
    extra_kvs = extractHsTyRdrTyVarsKindVars rhs

rnTyFamDefltDecl :: Name
                 -> TyFamDefltDecl GhcPs
                 -> RnM (TyFamDefltDecl GhcRn, FreeVars)
rnTyFamDefltDecl cls = rnTyFamInstDecl (AssocTyFamDeflt cls)

rnDataFamInstDecl :: AssocTyFamInfo
                  -> DataFamInstDecl GhcPs
                  -> RnM (DataFamInstDecl GhcRn, FreeVars)
rnDataFamInstDecl atfi (DataFamInstDecl { dfid_eqn =
                    eqn@(FamEqn { feqn_tycon = tycon
                                , feqn_rhs   = rhs })})
  = do { let extra_kvs = extractDataDefnKindVars rhs
       ; (eqn', fvs) <-
           rnFamEqn (TyDataCtx tycon) atfi extra_kvs eqn rnDataDefn
       ; return (DataFamInstDecl { dfid_eqn = eqn' }, fvs) }

-- Renaming of the associated types in instances.

-- Rename associated type family decl in class
rnATDecls :: Name      -- Class
          -> [LFamilyDecl GhcPs]
          -> RnM ([LFamilyDecl GhcRn], FreeVars)
rnATDecls cls at_decls
  = rnList (rnFamDecl (Just cls)) at_decls

rnATInstDecls :: (AssocTyFamInfo ->           -- The function that renames
                  decl GhcPs ->               -- an instance. rnTyFamInstDecl
                  RnM (decl GhcRn, FreeVars)) -- or rnDataFamInstDecl
              -> Name      -- Class
              -> [Name]
              -> [Located (decl GhcPs)]
              -> RnM ([Located (decl GhcRn)], FreeVars)
-- Used for data and type family defaults in a class decl
-- and the family instance declarations in an instance
--
-- NB: We allow duplicate associated-type decls;
--     See Note [Associated type instances] in GHC.Tc.TyCl.Instance
rnATInstDecls rnFun cls tv_ns at_insts
  = rnList (rnFun (AssocTyFamInst cls tv_ns)) at_insts
    -- See Note [Renaming associated types]

{- Note [Wildcards in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Wild cards can be used in type/data family instance declarations to indicate
that the name of a type variable doesn't matter. Each wild card will be
replaced with a new unique type variable. For instance:

    type family F a b :: *
    type instance F Int _ = Int

is the same as

    type family F a b :: *
    type instance F Int b = Int

This is implemented as follows: Unnamed wildcards remain unchanged after
the renamer, and then given fresh meta-variables during typechecking, and
it is handled pretty much the same way as the ones in partial type signatures.
We however don't want to emit hole constraints on wildcards in family
instances, so we turn on PartialTypeSignatures and turn off warning flag to
let typechecker know this.
See related Note [Wildcards in visible kind application] in GHC.Tc.Gen.HsType

Note [Unused type variables in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the flag -fwarn-unused-type-patterns is on, the compiler reports
warnings about unused type variables in type-family instances. A
tpye variable is considered used (i.e. cannot be turned into a wildcard)
when

 * it occurs on the RHS of the family instance
   e.g.   type instance F a b = a    -- a is used on the RHS

 * it occurs multiple times in the patterns on the LHS
   e.g.   type instance F a a = Int  -- a appears more than once on LHS

 * it is one of the instance-decl variables, for associated types
   e.g.   instance C (a,b) where
            type T (a,b) = a
   Here the type pattern in the type instance must be the same as that
   for the class instance, so
            type T (a,_) = a
   would be rejected.  So we should not complain about an unused variable b

As usual, the warnings are not reported for type variables with names
beginning with an underscore.

Extra-constraints wild cards are not supported in type/data family
instance declarations.

Relevant tickets: #3699, #10586, #10982 and #11451.

Note [Renaming associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When renaming a type/data family instance, be it top-level or associated with
a class, we must check that all of the type variables mentioned on the RHS are
properly scoped. Specifically, the rule is this:

  Every variable mentioned on the RHS of a type instance declaration
  (whether associated or not) must be either
  * Mentioned on the LHS, or
  * Mentioned in an outermost kind signature on the RHS
    (see Note [Implicit quantification in type synonyms])

Here is a simple example of something we should reject:

  class C a b where
    type F a x
  instance C Int Bool where
    type F Int x = z

Here, `z` is mentioned on the RHS of the associated instance without being
mentioned on the LHS, nor is `z` mentioned in an outermost kind signature. The
renamer will reject `z` as being out of scope without much fuss.

Things get slightly trickier when the instance header itself binds type
variables. Consider this example (adapted from #5515):

   instance C (p,q) z where
      type F (p,q) x = (x, z)

According to the rule above, this instance is improperly scoped. However, due
to the way GHC's renamer works, `z` is /technically/ in scope, as GHC will
always bring type variables from an instance header into scope over the
associated type family instances. As a result, the renamer won't simply reject
the `z` as being out of scope (like it would for the `type F Int x = z`
example) unless further action is taken. It is important to reject this sort of
thing in the renamer, because if it is allowed to make it through to the
typechecker, unexpected shenanigans can occur (see #18021 for examples).

To prevent these sorts of shenanigans, we reject programs like the one above
with an extra validity check in rnFamEqn. For each type variable bound in the
parent instance head, we check if it is mentioned on the RHS of the associated
family instance but not bound on the LHS. If any of the instance-head-bound
variables meet these criteria, we throw an error.
(See rnFamEqn.improperly_scoped for how this is implemented.)

Some additional wrinkles:

* This Note only applies to *instance* declarations.  In *class* declarations
  there is no RHS to worry about, and the class variables can all be in scope
  (#5862):

    class Category (x :: k -> k -> *) where
      type Ob x :: k -> Constraint
      id :: Ob x a => x a a
      (.) :: (Ob x a, Ob x b, Ob x c) => x b c -> x a b -> x a c

  Here 'k' is in scope in the kind signature, just like 'x'.

* Although type family equations can bind type variables with explicit foralls,
  it need not be the case that all variables that appear on the RHS must be
  bound by a forall. For instance, the following is acceptable:

    class C4 a where
      type T4 a b
    instance C4 (Maybe a) where
      type forall b. T4 (Maybe a) b = Either a b

  Even though `a` is not bound by the forall, this is still accepted because `a`
  was previously bound by the `instance C4 (Maybe a)` part. (see #16116).

* In addition to the validity check in rnFamEqn.improperly_scoped, there is an
  additional check in GHC.Tc.Validity.checkFamPatBinders that checks each family
  instance equation for type variables used on the RHS but not bound on the
  LHS. This is not made redundant by rmFamEqn.improperly_scoped, as there are
  programs that each check will reject that the other check will not catch:

  - checkValidFamPats is used on all forms of family instances, whereas
    rmFamEqn.improperly_scoped only checks associated family instances. Since
    checkFamPatBinders occurs after typechecking, it can catch programs that
    introduce dodgy scoping by way of type synonyms (see #7536), which is
    impractical to accomplish in the renamer.
  - rnFamEqn.improperly_scoped catches some programs that, if allowed to escape
    the renamer, would accidentally be accepted by the typechecker. Here is one
    such program (#18021):

      class C5 a where
        data family D a

      instance forall a. C5 Int where
        data instance D Int = MkD a

    If this is not rejected in the renamer, the typechecker would treat this
    program as though the `a` were existentially quantified, like so:

      data instance D Int = forall a. MkD a

    This is likely not what the user intended!

    Here is another such program (#9574):

      class Funct f where
        type Codomain f
      instance Funct ('KProxy :: KProxy o) where
        type Codomain 'KProxy = NatTr (Proxy :: o -> Type)

    Where:

      data Proxy (a :: k) = Proxy
      data KProxy (t :: Type) = KProxy
      data NatTr (c :: o -> Type)

    Note that the `o` in the `Codomain 'KProxy` instance should be considered
    improperly scoped. It does not meet the criteria for being explicitly
    quantified, as it is not mentioned by name on the LHS, nor does it meet the
    criteria for being implicitly quantified, as it is used in a RHS kind
    signature that is not outermost (see Note [Implicit quantification in type
    synonyms]). However, `o` /is/ bound by the instance header, so if this
    program is not rejected by the renamer, the typechecker would treat it as
    though you had written this:

      instance Funct ('KProxy :: KProxy o) where
        type Codomain ('KProxy @o) = NatTr (Proxy :: o -> Type)

    Although this is a valid program, it's probably a stretch too far to turn
    `type Codomain 'KProxy = ...` into `type Codomain ('KProxy @o) = ...` here.
    If the user really wants the latter, it is simple enough to communicate
    their intent by mentioning `o` on the LHS by name.

Note [Type family equations and occurrences]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In most data/type family equations, the type family name used in the equation
is treated as an occurrence. For example:

  module A where
    type family F a

  module B () where
    import B (F)
    type instance F Int = Bool

We do not want to warn about `F` being unused in the module `B`, as the
instance constitutes a use site for `F`. The exception to this rule is closed
type families, whose equations constitute a definition, not occurrences. For
example:

  module C () where
    type family CF a where
      CF Char = Float

Here, we /do/ want to warn that `CF` is unused in the module `C`, as it is
defined but not used (#18470).

GHC accomplishes this in rnFamInstEqn when determining the set of free
variables to return at the end. If renaming a data family or open type family
equation, we add the name of the type family constructor to the set of returned
free variables to ensure that the name is marked as an occurrence. If renaming
a closed type family equation, we avoid adding the type family constructor name
to the free variables. This is quite simple, but it is not a perfect solution.
Consider this example:

  module X () where
    type family F a where
      F Int = Bool
      F Double = F Int

At present, GHC will treat any use of a type family constructor on the RHS of a
type family equation as an occurrence. Since `F` is used on the RHS of the
second equation of `F`, it is treated as an occurrence, causing `F` not to be
warned about. This is not ideal, since `F` isn't exported—it really /should/
cause a warning to be emitted. There is some discussion in #10089/#12920 about
how this limitation might be overcome, but until then, we stick to the
simplistic solution above, as it fixes the egregious bug in #18470.
-}


{-
*********************************************************
*                                                      *
\subsection{Stand-alone deriving declarations}
*                                                      *
*********************************************************
-}

rnSrcDerivDecl :: DerivDecl GhcPs -> RnM (DerivDecl GhcRn, FreeVars)
rnSrcDerivDecl (DerivDecl _ ty mds overlap)
  = do { standalone_deriv_ok <- xoptM LangExt.StandaloneDeriving
       ; unless standalone_deriv_ok (addErr standaloneDerivErr)
       ; checkInferredVars ctxt inf_err nowc_ty
       ; (mds', ty', fvs) <- rnLDerivStrategy ctxt mds $ rnHsSigWcType ctxt ty
         -- Check if there are any nested `forall`s or contexts, which are
         -- illegal in the type of an instance declaration (see
         -- Note [No nested foralls or contexts in instance types] in
         -- GHC.Hs.Type).
       ; addNoNestedForallsContextsErr ctxt
           (text "Standalone-derived instance head")
           (getLHsInstDeclHead $ dropWildCards ty')
       ; warnNoDerivStrat mds' loc
       ; return (DerivDecl noExtField ty' mds' overlap, fvs) }
  where
    ctxt    = DerivDeclCtx
    inf_err = Just (text "Inferred type variables are not allowed")
    loc = getLoc nowc_ty
    nowc_ty = dropWildCards ty

standaloneDerivErr :: SDoc
standaloneDerivErr
  = hang (text "Illegal standalone deriving declaration")
       2 (text "Use StandaloneDeriving to enable this extension")

{-
*********************************************************
*                                                      *
\subsection{Rules}
*                                                      *
*********************************************************
-}

rnHsRuleDecls :: RuleDecls GhcPs -> RnM (RuleDecls GhcRn, FreeVars)
rnHsRuleDecls (HsRules { rds_src = src
                       , rds_rules = rules })
  = do { (rn_rules,fvs) <- rnList rnHsRuleDecl rules
       ; return (HsRules { rds_ext = noExtField
                         , rds_src = src
                         , rds_rules = rn_rules }, fvs) }

rnHsRuleDecl :: RuleDecl GhcPs -> RnM (RuleDecl GhcRn, FreeVars)
rnHsRuleDecl (HsRule { rd_name = rule_name
                     , rd_act  = act
                     , rd_tyvs = tyvs
                     , rd_tmvs = tmvs
                     , rd_lhs  = lhs
                     , rd_rhs  = rhs })
  = do { let rdr_names_w_loc = map (get_var . unLoc) tmvs
       ; checkDupRdrNames rdr_names_w_loc
       ; checkShadowedRdrNames rdr_names_w_loc
       ; names <- newLocalBndrsRn rdr_names_w_loc
       ; let doc = RuleCtx (snd $ unLoc rule_name)
       ; bindRuleTyVars doc tyvs $ \ tyvs' ->
         bindRuleTmVars doc tyvs' tmvs names $ \ tmvs' ->
    do { (lhs', fv_lhs') <- rnLExpr lhs
       ; (rhs', fv_rhs') <- rnLExpr rhs
       ; checkValidRule (snd $ unLoc rule_name) names lhs' fv_lhs'
       ; return (HsRule { rd_ext  = HsRuleRn fv_lhs' fv_rhs'
                        , rd_name = rule_name
                        , rd_act  = act
                        , rd_tyvs = tyvs'
                        , rd_tmvs = tmvs'
                        , rd_lhs  = lhs'
                        , rd_rhs  = rhs' }, fv_lhs' `plusFV` fv_rhs') } }
  where
    get_var :: RuleBndr GhcPs -> Located RdrName
    get_var (RuleBndrSig _ v _) = v
    get_var (RuleBndr _ v)      = v

bindRuleTmVars :: HsDocContext -> Maybe ty_bndrs
               -> [LRuleBndr GhcPs] -> [Name]
               -> ([LRuleBndr GhcRn] -> RnM (a, FreeVars))
               -> RnM (a, FreeVars)
bindRuleTmVars doc tyvs vars names thing_inside
  = go vars names $ \ vars' ->
    bindLocalNamesFV names (thing_inside vars')
  where
    go ((L l (RuleBndr _ (L loc _))) : vars) (n : ns) thing_inside
      = go vars ns $ \ vars' ->
        thing_inside (L l (RuleBndr noExtField (L loc n)) : vars')

    go ((L l (RuleBndrSig _ (L loc _) bsig)) : vars)
       (n : ns) thing_inside
      = rnHsPatSigType bind_free_tvs doc bsig $ \ bsig' ->
        go vars ns $ \ vars' ->
        thing_inside (L l (RuleBndrSig noExtField (L loc n) bsig') : vars')

    go [] [] thing_inside = thing_inside []
    go vars names _ = pprPanic "bindRuleVars" (ppr vars $$ ppr names)

    bind_free_tvs = case tyvs of Nothing -> AlwaysBind
                                 Just _  -> NeverBind

bindRuleTyVars :: HsDocContext -> Maybe [LHsTyVarBndr () GhcPs]
               -> (Maybe [LHsTyVarBndr () GhcRn]  -> RnM (b, FreeVars))
               -> RnM (b, FreeVars)
bindRuleTyVars doc (Just bndrs) thing_inside
  = bindLHsTyVarBndrs doc WarnUnusedForalls Nothing bndrs (thing_inside . Just)
bindRuleTyVars _ _ thing_inside = thing_inside Nothing

{-
Note [Rule LHS validity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check the shape of a rewrite rule LHS.  Currently we only allow
LHSs of the form @(f e1 .. en)@, where @f@ is not one of the
@forall@'d variables.

We used restrict the form of the 'ei' to prevent you writing rules
with LHSs with a complicated desugaring (and hence unlikely to match);
(e.g. a case expression is not allowed: too elaborate.)

But there are legitimate non-trivial args ei, like sections and
lambdas.  So it seems simmpler not to check at all, and that is why
check_e is commented out.
-}

checkValidRule :: FastString -> [Name] -> LHsExpr GhcRn -> NameSet -> RnM ()
checkValidRule rule_name ids lhs' fv_lhs'
  = do  {       -- Check for the form of the LHS
          case (validRuleLhs ids lhs') of
                Nothing  -> return ()
                Just bad -> failWithTc (badRuleLhsErr rule_name lhs' bad)

                -- Check that LHS vars are all bound
        ; let bad_vars = [var | var <- ids, not (var `elemNameSet` fv_lhs')]
        ; mapM_ (addErr . badRuleVar rule_name) bad_vars }

validRuleLhs :: [Name] -> LHsExpr GhcRn -> Maybe (HsExpr GhcRn)
-- Nothing => OK
-- Just e  => Not ok, and e is the offending sub-expression
validRuleLhs foralls lhs
  = checkl lhs
  where
    checkl = check . unLoc

    check (OpApp _ e1 op e2)              = checkl op `mplus` checkl_e e1
                                                      `mplus` checkl_e e2
    check (HsApp _ e1 e2)                 = checkl e1 `mplus` checkl_e e2
    check (HsAppType _ e _)               = checkl e
    check (HsVar _ lv)
      | (unLoc lv) `notElem` foralls      = Nothing
    check other                           = Just other  -- Failure

        -- Check an argument
    checkl_e _ = Nothing
    -- Was (check_e e); see Note [Rule LHS validity checking]

{-      Commented out; see Note [Rule LHS validity checking] above
    check_e (HsVar v)     = Nothing
    check_e (HsPar e)     = checkl_e e
    check_e (HsLit e)     = Nothing
    check_e (HsOverLit e) = Nothing

    check_e (OpApp e1 op _ e2)   = checkl_e e1 `mplus` checkl_e op `mplus` checkl_e e2
    check_e (HsApp e1 e2)        = checkl_e e1 `mplus` checkl_e e2
    check_e (NegApp e _)         = checkl_e e
    check_e (ExplicitList _ es)  = checkl_es es
    check_e other                = Just other   -- Fails

    checkl_es es = foldr (mplus . checkl_e) Nothing es
-}

badRuleVar :: FastString -> Name -> SDoc
badRuleVar name var
  = sep [text "Rule" <+> doubleQuotes (ftext name) <> colon,
         text "Forall'd variable" <+> quotes (ppr var) <+>
                text "does not appear on left hand side"]

badRuleLhsErr :: FastString -> LHsExpr GhcRn -> HsExpr GhcRn -> SDoc
badRuleLhsErr name lhs bad_e
  = sep [text "Rule" <+> pprRuleName name <> colon,
         nest 2 (vcat [err,
                       text "in left-hand side:" <+> ppr lhs])]
    $$
    text "LHS must be of form (f e1 .. en) where f is not forall'd"
  where
    err = case bad_e of
            HsUnboundVar _ uv -> notInScopeErr (mkRdrUnqual uv)
            _                 -> text "Illegal expression:" <+> ppr bad_e

{- **************************************************************
         *                                                      *
      Renaming type, class, instance and role declarations
*                                                               *
*****************************************************************

@rnTyDecl@ uses the `global name function' to create a new type
declaration in which local names have been replaced by their original
names, reporting any unknown names.

Renaming type variables is a pain. Because they now contain uniques,
it is necessary to pass in an association list which maps a parsed
tyvar to its @Name@ representation.
In some cases (type signatures of values),
it is even necessary to go over the type first
in order to get the set of tyvars used by it, make an assoc list,
and then go over it again to rename the tyvars!
However, we can also do some scoping checks at the same time.

Note [Dependency analysis of type, class, and instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A TyClGroup represents a strongly connected components of
type/class/instance decls, together with the role annotations for the
type/class declarations.  The renamer uses strongly connected
comoponent analysis to build these groups.  We do this for a number of
reasons:

* Improve kind error messages. Consider

     data T f a = MkT f a
     data S f a = MkS f (T f a)

  This has a kind error, but the error message is better if you
  check T first, (fixing its kind) and *then* S.  If you do kind
  inference together, you might get an error reported in S, which
  is jolly confusing.  See #4875


* Increase kind polymorphism.  See GHC.Tc.TyCl
  Note [Grouping of type and class declarations]

Why do the instance declarations participate?  At least two reasons

* Consider (#11348)

     type family F a
     type instance F Int = Bool

     data R = MkR (F Int)

     type Foo = 'MkR 'True

  For Foo to kind-check we need to know that (F Int) ~ Bool.  But we won't
  know that unless we've looked at the type instance declaration for F
  before kind-checking Foo.

* Another example is this (#3990).

     data family Complex a
     data instance Complex Double = CD {-# UNPACK #-} !Double
                                       {-# UNPACK #-} !Double

     data T = T {-# UNPACK #-} !(Complex Double)

  Here, to generate the right kind of unpacked implementation for T,
  we must have access to the 'data instance' declaration.

* Things become more complicated when we introduce transitive
  dependencies through imported definitions, like in this scenario:

      A.hs
        type family Closed (t :: Type) :: Type where
          Closed t = Open t

        type family Open (t :: Type) :: Type

      B.hs
        data Q where
          Q :: Closed Bool -> Q

        type instance Open Int = Bool

        type S = 'Q 'True

  Somehow, we must ensure that the instance Open Int = Bool is checked before
  the type synonym S. While we know that S depends upon 'Q depends upon Closed,
  we have no idea that Closed depends upon Open!

  To accommodate for these situations, we ensure that an instance is checked
  before every @TyClDecl@ on which it does not depend. That's to say, instances
  are checked as early as possible in @tcTyAndClassDecls@.

------------------------------------
So much for WHY.  What about HOW?  It's pretty easy:

(1) Rename the type/class, instance, and role declarations
    individually

(2) Do strongly-connected component analysis of the type/class decls,
    We'll make a TyClGroup for each SCC

    In this step we treat a reference to a (promoted) data constructor
    K as a dependency on its parent type.  Thus
        data T = K1 | K2
        data S = MkS (Proxy 'K1)
    Here S depends on 'K1 and hence on its parent T.

    In this step we ignore instances; see
    Note [No dependencies on data instances]

(3) Attach roles to the appropriate SCC

(4) Attach instances to the appropriate SCC.
    We add an instance decl to SCC when:
      all its free types/classes are bound in this SCC or earlier ones

(5) We make an initial TyClGroup, with empty group_tyclds, for any
    (orphan) instances that affect only imported types/classes

Steps (3) and (4) are done by the (mapAccumL mk_group) call.

Note [No dependencies on data instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
   data family D a
   data instance D Int = D1
   data S = MkS (Proxy 'D1)

Here the declaration of S depends on the /data instance/ declaration
for 'D Int'.  That makes things a lot more complicated, especially
if the data instance is an associated type of an enclosing class instance.
(And the class instance might have several associated type instances
with different dependency structure!)

Ugh.  For now we simply don't allow promotion of data constructors for
data instances.  See Note [AFamDataCon: not promoting data family
constructors] in GHC.Tc.Utils.Env
-}


rnTyClDecls :: [TyClGroup GhcPs]
            -> RnM ([TyClGroup GhcRn], FreeVars)
-- Rename the declarations and do dependency analysis on them
rnTyClDecls tycl_ds
  = do { -- Rename the type/class, instance, and role declaraations
       ; tycls_w_fvs <- mapM (wrapLocFstM rnTyClDecl) (tyClGroupTyClDecls tycl_ds)
       ; let tc_names = mkNameSet (map (tcdName . unLoc . fst) tycls_w_fvs)
       ; kisigs_w_fvs <- rnStandaloneKindSignatures tc_names (tyClGroupKindSigs tycl_ds)
       ; instds_w_fvs <- mapM (wrapLocFstM rnSrcInstDecl) (tyClGroupInstDecls tycl_ds)
       ; role_annots  <- rnRoleAnnots tc_names (tyClGroupRoleDecls tycl_ds)

       -- Do SCC analysis on the type/class decls
       ; rdr_env <- getGlobalRdrEnv
       ; let tycl_sccs = depAnalTyClDecls rdr_env kisig_fv_env tycls_w_fvs
             role_annot_env = mkRoleAnnotEnv role_annots
             (kisig_env, kisig_fv_env) = mkKindSig_fv_env kisigs_w_fvs

             inst_ds_map = mkInstDeclFreeVarsMap rdr_env tc_names instds_w_fvs
             (init_inst_ds, rest_inst_ds) = getInsts [] inst_ds_map

             first_group
               | null init_inst_ds = []
               | otherwise = [TyClGroup { group_ext    = noExtField
                                        , group_tyclds = []
                                        , group_kisigs = []
                                        , group_roles  = []
                                        , group_instds = init_inst_ds }]

             (final_inst_ds, groups)
                = mapAccumL (mk_group role_annot_env kisig_env) rest_inst_ds tycl_sccs

             all_fvs = foldr (plusFV . snd) emptyFVs tycls_w_fvs  `plusFV`
                       foldr (plusFV . snd) emptyFVs instds_w_fvs `plusFV`
                       foldr (plusFV . snd) emptyFVs kisigs_w_fvs

             all_groups = first_group ++ groups

       ; MASSERT2( null final_inst_ds,  ppr instds_w_fvs $$ ppr inst_ds_map
                                       $$ ppr (flattenSCCs tycl_sccs) $$ ppr final_inst_ds  )

       ; traceRn "rnTycl dependency analysis made groups" (ppr all_groups)
       ; return (all_groups, all_fvs) }
  where
    mk_group :: RoleAnnotEnv
             -> KindSigEnv
             -> InstDeclFreeVarsMap
             -> SCC (LTyClDecl GhcRn)
             -> (InstDeclFreeVarsMap, TyClGroup GhcRn)
    mk_group role_env kisig_env inst_map scc
      = (inst_map', group)
      where
        tycl_ds              = flattenSCC scc
        bndrs                = map (tcdName . unLoc) tycl_ds
        roles                = getRoleAnnots bndrs role_env
        kisigs               = getKindSigs   bndrs kisig_env
        (inst_ds, inst_map') = getInsts      bndrs inst_map
        group = TyClGroup { group_ext    = noExtField
                          , group_tyclds = tycl_ds
                          , group_kisigs = kisigs
                          , group_roles  = roles
                          , group_instds = inst_ds }

-- | Free variables of standalone kind signatures.
newtype KindSig_FV_Env = KindSig_FV_Env (NameEnv FreeVars)

lookupKindSig_FV_Env :: KindSig_FV_Env -> Name -> FreeVars
lookupKindSig_FV_Env (KindSig_FV_Env e) name
  = fromMaybe emptyFVs (lookupNameEnv e name)

-- | Standalone kind signatures.
type KindSigEnv = NameEnv (LStandaloneKindSig GhcRn)

mkKindSig_fv_env :: [(LStandaloneKindSig GhcRn, FreeVars)] -> (KindSigEnv, KindSig_FV_Env)
mkKindSig_fv_env kisigs_w_fvs = (kisig_env, kisig_fv_env)
  where
    kisig_env = mapNameEnv fst compound_env
    kisig_fv_env = KindSig_FV_Env (mapNameEnv snd compound_env)
    compound_env :: NameEnv (LStandaloneKindSig GhcRn, FreeVars)
      = mkNameEnvWith (standaloneKindSigName . unLoc . fst) kisigs_w_fvs

getKindSigs :: [Name] -> KindSigEnv -> [LStandaloneKindSig GhcRn]
getKindSigs bndrs kisig_env = mapMaybe (lookupNameEnv kisig_env) bndrs

rnStandaloneKindSignatures
  :: NameSet  -- names of types and classes in the current TyClGroup
  -> [LStandaloneKindSig GhcPs]
  -> RnM [(LStandaloneKindSig GhcRn, FreeVars)]
rnStandaloneKindSignatures tc_names kisigs
  = do { let (no_dups, dup_kisigs) = removeDups (compare `on` get_name) kisigs
             get_name = standaloneKindSigName . unLoc
       ; mapM_ dupKindSig_Err dup_kisigs
       ; mapM (wrapLocFstM (rnStandaloneKindSignature tc_names)) no_dups
       }

rnStandaloneKindSignature
  :: NameSet  -- names of types and classes in the current TyClGroup
  -> StandaloneKindSig GhcPs
  -> RnM (StandaloneKindSig GhcRn, FreeVars)
rnStandaloneKindSignature tc_names (StandaloneKindSig _ v ki)
  = do  { standalone_ki_sig_ok <- xoptM LangExt.StandaloneKindSignatures
        ; unless standalone_ki_sig_ok $ addErr standaloneKiSigErr
        ; new_v <- lookupSigCtxtOccRn (TopSigCtxt tc_names) (text "standalone kind signature") v
        ; let doc = StandaloneKindSigCtx (ppr v)
        ; (new_ki, fvs) <- rnHsSigType doc KindLevel ki
        ; return (StandaloneKindSig noExtField new_v new_ki, fvs)
        }
  where
    standaloneKiSigErr :: SDoc
    standaloneKiSigErr =
      hang (text "Illegal standalone kind signature")
         2 (text "Did you mean to enable StandaloneKindSignatures?")

depAnalTyClDecls :: GlobalRdrEnv
                 -> KindSig_FV_Env
                 -> [(LTyClDecl GhcRn, FreeVars)]
                 -> [SCC (LTyClDecl GhcRn)]
-- See Note [Dependency analysis of type, class, and instance decls]
depAnalTyClDecls rdr_env kisig_fv_env ds_w_fvs
  = stronglyConnCompFromEdgedVerticesUniq edges
  where
    edges :: [ Node Name (LTyClDecl GhcRn) ]
    edges = [ DigraphNode d name (map (getParent rdr_env) (nonDetEltsUniqSet deps))
            | (d, fvs) <- ds_w_fvs,
              let { name = tcdName (unLoc d)
                  ; kisig_fvs = lookupKindSig_FV_Env kisig_fv_env name
                  ; deps = fvs `plusFV` kisig_fvs
                  }
            ]
            -- It's OK to use nonDetEltsUFM here as
            -- stronglyConnCompFromEdgedVertices is still deterministic
            -- even if the edges are in nondeterministic order as explained
            -- in Note [Deterministic SCC] in GHC.Data.Graph.Directed.

toParents :: GlobalRdrEnv -> NameSet -> NameSet
toParents rdr_env ns
  = nonDetStrictFoldUniqSet add emptyNameSet ns
  -- It's OK to use a non-deterministic fold because we immediately forget the
  -- ordering by creating a set
  where
    add n s = extendNameSet s (getParent rdr_env n)

getParent :: GlobalRdrEnv -> Name -> Name
getParent rdr_env n
  = case lookupGRE_Name rdr_env n of
      Just gre -> case gre_par gre of
                    ParentIs  { par_is = p } -> p
                    _                        -> n
      Nothing -> n


{- ******************************************************
*                                                       *
       Role annotations
*                                                       *
****************************************************** -}

-- | Renames role annotations, returning them as the values in a NameEnv
-- and checks for duplicate role annotations.
-- It is quite convenient to do both of these in the same place.
-- See also Note [Role annotations in the renamer]
rnRoleAnnots :: NameSet
             -> [LRoleAnnotDecl GhcPs]
             -> RnM [LRoleAnnotDecl GhcRn]
rnRoleAnnots tc_names role_annots
  = do {  -- Check for duplicates *before* renaming, to avoid
          -- lumping together all the unboundNames
         let (no_dups, dup_annots) = removeDups (compare `on` get_name) role_annots
             get_name = roleAnnotDeclName . unLoc
       ; mapM_ dupRoleAnnotErr dup_annots
       ; mapM (wrapLocM rn_role_annot1) no_dups }
  where
    rn_role_annot1 (RoleAnnotDecl _ tycon roles)
      = do {  -- the name is an *occurrence*, but look it up only in the
              -- decls defined in this group (see #10263)
             tycon' <- lookupSigCtxtOccRn (RoleAnnotCtxt tc_names)
                                          (text "role annotation")
                                          tycon
           ; return $ RoleAnnotDecl noExtField tycon' roles }

dupRoleAnnotErr :: NonEmpty (LRoleAnnotDecl GhcPs) -> RnM ()
dupRoleAnnotErr list
  = addErrAt loc $
    hang (text "Duplicate role annotations for" <+>
          quotes (ppr $ roleAnnotDeclName first_decl) <> colon)
       2 (vcat $ map pp_role_annot $ NE.toList sorted_list)
    where
      sorted_list = NE.sortBy cmp_loc list
      ((L loc first_decl) :| _) = sorted_list

      pp_role_annot (L loc decl) = hang (ppr decl)
                                      4 (text "-- written at" <+> ppr loc)

      cmp_loc = SrcLoc.leftmost_smallest `on` getLoc

dupKindSig_Err :: NonEmpty (LStandaloneKindSig GhcPs) -> RnM ()
dupKindSig_Err list
  = addErrAt loc $
    hang (text "Duplicate standalone kind signatures for" <+>
          quotes (ppr $ standaloneKindSigName first_decl) <> colon)
       2 (vcat $ map pp_kisig $ NE.toList sorted_list)
    where
      sorted_list = NE.sortBy cmp_loc list
      ((L loc first_decl) :| _) = sorted_list

      pp_kisig (L loc decl) =
        hang (ppr decl) 4 (text "-- written at" <+> ppr loc)

      cmp_loc = SrcLoc.leftmost_smallest `on` getLoc

{- Note [Role annotations in the renamer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must ensure that a type's role annotation is put in the same group as the
proper type declaration. This is because role annotations are needed during
type-checking when creating the type's TyCon. So, rnRoleAnnots builds a
NameEnv (LRoleAnnotDecl Name) that maps a name to a role annotation for that
type, if any. Then, this map can be used to add the role annotations to the
groups after dependency analysis.

This process checks for duplicate role annotations, where we must be careful
to do the check *before* renaming to avoid calling all unbound names duplicates
of one another.

The renaming process, as usual, might identify and report errors for unbound
names. This is done by using lookupSigCtxtOccRn in rnRoleAnnots (using
lookupGlobalOccRn led to #8485).
-}


{- ******************************************************
*                                                       *
       Dependency info for instances
*                                                       *
****************************************************** -}

----------------------------------------------------------
-- | 'InstDeclFreeVarsMap is an association of an
--   @InstDecl@ with @FreeVars@. The @FreeVars@ are
--   the tycon names that are both
--     a) free in the instance declaration
--     b) bound by this group of type/class/instance decls
type InstDeclFreeVarsMap = [(LInstDecl GhcRn, FreeVars)]

-- | Construct an @InstDeclFreeVarsMap@ by eliminating any @Name@s from the
--   @FreeVars@ which are *not* the binders of a @TyClDecl@.
mkInstDeclFreeVarsMap :: GlobalRdrEnv
                      -> NameSet
                      -> [(LInstDecl GhcRn, FreeVars)]
                      -> InstDeclFreeVarsMap
mkInstDeclFreeVarsMap rdr_env tycl_bndrs inst_ds_fvs
  = [ (inst_decl, toParents rdr_env fvs `intersectFVs` tycl_bndrs)
    | (inst_decl, fvs) <- inst_ds_fvs ]

-- | Get the @LInstDecl@s which have empty @FreeVars@ sets, and the
--   @InstDeclFreeVarsMap@ with these entries removed.
-- We call (getInsts tcs instd_map) when we've completed the declarations
-- for 'tcs'.  The call returns (inst_decls, instd_map'), where
--   inst_decls are the instance declarations all of
--              whose free vars are now defined
--   instd_map' is the inst-decl map with 'tcs' removed from
--               the free-var set
getInsts :: [Name] -> InstDeclFreeVarsMap
         -> ([LInstDecl GhcRn], InstDeclFreeVarsMap)
getInsts bndrs inst_decl_map
  = partitionWith pick_me inst_decl_map
  where
    pick_me :: (LInstDecl GhcRn, FreeVars)
            -> Either (LInstDecl GhcRn) (LInstDecl GhcRn, FreeVars)
    pick_me (decl, fvs)
      | isEmptyNameSet depleted_fvs = Left decl
      | otherwise                   = Right (decl, depleted_fvs)
      where
        depleted_fvs = delFVs bndrs fvs

{- ******************************************************
*                                                       *
         Renaming a type or class declaration
*                                                       *
****************************************************** -}

rnTyClDecl :: TyClDecl GhcPs
           -> RnM (TyClDecl GhcRn, FreeVars)

-- All flavours of top-level type family declarations ("type family", "newtype
-- family", and "data family")
rnTyClDecl (FamDecl { tcdFam = fam })
  = do { (fam', fvs) <- rnFamDecl Nothing fam
       ; return (FamDecl noExtField fam', fvs) }

rnTyClDecl (SynDecl { tcdLName = tycon, tcdTyVars = tyvars,
                      tcdFixity = fixity, tcdRhs = rhs })
  = do { tycon' <- lookupLocatedTopBndrRn tycon
       ; let kvs = extractHsTyRdrTyVarsKindVars rhs
             doc = TySynCtx tycon
       ; traceRn "rntycl-ty" (ppr tycon <+> ppr kvs)
       ; bindHsQTyVars doc Nothing kvs tyvars $ \ tyvars' _ ->
    do { (rhs', fvs) <- rnTySyn doc rhs
       ; return (SynDecl { tcdLName = tycon', tcdTyVars = tyvars'
                         , tcdFixity = fixity
                         , tcdRhs = rhs', tcdSExt = fvs }, fvs) } }

-- "data", "newtype" declarations
rnTyClDecl (DataDecl
    { tcdLName = tycon, tcdTyVars = tyvars,
      tcdFixity = fixity,
      tcdDataDefn = defn@HsDataDefn{ dd_ND = new_or_data
                                   , dd_kindSig = kind_sig} })
  = do { tycon' <- lookupLocatedTopBndrRn tycon
       ; let kvs = extractDataDefnKindVars defn
             doc = TyDataCtx tycon
       ; traceRn "rntycl-data" (ppr tycon <+> ppr kvs)
       ; bindHsQTyVars doc Nothing kvs tyvars $ \ tyvars' no_rhs_kvs ->
    do { (defn', fvs) <- rnDataDefn doc defn
       ; cusk <- data_decl_has_cusk tyvars' new_or_data no_rhs_kvs kind_sig
       ; let rn_info = DataDeclRn { tcdDataCusk = cusk
                                  , tcdFVs      = fvs }
       ; traceRn "rndata" (ppr tycon <+> ppr cusk <+> ppr no_rhs_kvs)
       ; return (DataDecl { tcdLName    = tycon'
                          , tcdTyVars   = tyvars'
                          , tcdFixity   = fixity
                          , tcdDataDefn = defn'
                          , tcdDExt     = rn_info }, fvs) } }

rnTyClDecl (ClassDecl { tcdCtxt = context, tcdLName = lcls,
                        tcdTyVars = tyvars, tcdFixity = fixity,
                        tcdFDs = fds, tcdSigs = sigs,
                        tcdMeths = mbinds, tcdATs = ats, tcdATDefs = at_defs,
                        tcdDocs = docs})
  = do  { lcls' <- lookupLocatedTopBndrRn lcls
        ; let cls' = unLoc lcls'
              kvs = []  -- No scoped kind vars except those in
                        -- kind signatures on the tyvars

        -- Tyvars scope over superclass context and method signatures
        ; ((tyvars', context', fds', ats'), stuff_fvs)
            <- bindHsQTyVars cls_doc Nothing kvs tyvars $ \ tyvars' _ -> do
                  -- Checks for distinct tyvars
             { (context', cxt_fvs) <- rnContext cls_doc context
             ; fds'  <- rnFds fds
                         -- The fundeps have no free variables
             ; (ats', fv_ats) <- rnATDecls cls' ats
             ; let fvs = cxt_fvs     `plusFV`
                         fv_ats
             ; return ((tyvars', context', fds', ats'), fvs) }

        ; (at_defs', fv_at_defs) <- rnList (rnTyFamDefltDecl cls') at_defs

        -- No need to check for duplicate associated type decls
        -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn

        -- Check the signatures
        -- First process the class op sigs (op_sigs), then the fixity sigs (non_op_sigs).
        ; let sig_rdr_names_w_locs =
                [op | L _ (ClassOpSig _ False ops _) <- sigs
                    , op <- ops]
        ; checkDupRdrNames sig_rdr_names_w_locs
                -- Typechecker is responsible for checking that we only
                -- give default-method bindings for things in this class.
                -- The renamer *could* check this for class decls, but can't
                -- for instance decls.

        -- The newLocals call is tiresome: given a generic class decl
        --      class C a where
        --        op :: a -> a
        --        op {| x+y |} (Inl a) = ...
        --        op {| x+y |} (Inr b) = ...
        --        op {| a*b |} (a*b)   = ...
        -- we want to name both "x" tyvars with the same unique, so that they are
        -- easy to group together in the typechecker.
        ; (mbinds', sigs', meth_fvs)
            <- rnMethodBinds True cls' (hsAllLTyVarNames tyvars') mbinds sigs
                -- No need to check for duplicate method signatures
                -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn
                -- and the methods are already in scope

        ; let all_fvs = meth_fvs `plusFV` stuff_fvs `plusFV` fv_at_defs
        ; return (ClassDecl { tcdCtxt = context', tcdLName = lcls',
                              tcdTyVars = tyvars', tcdFixity = fixity,
                              tcdFDs = fds', tcdSigs = sigs',
                              tcdMeths = mbinds', tcdATs = ats', tcdATDefs = at_defs',
                              tcdDocs = docs, tcdCExt = all_fvs },
                  all_fvs ) }
  where
    cls_doc  = ClassDeclCtx lcls

-- Does the data type declaration include a CUSK?
data_decl_has_cusk :: LHsQTyVars (GhcPass p) -> NewOrData -> Bool -> Maybe (LHsKind (GhcPass p')) -> RnM Bool
data_decl_has_cusk tyvars new_or_data no_rhs_kvs kind_sig = do
  { -- See Note [Unlifted Newtypes and CUSKs], and for a broader
    -- picture, see Note [Implementation of UnliftedNewtypes].
  ; unlifted_newtypes <- xoptM LangExt.UnliftedNewtypes
  ; let non_cusk_newtype
          | NewType <- new_or_data =
              unlifted_newtypes && isNothing kind_sig
          | otherwise = False
    -- See Note [CUSKs: complete user-supplied kind signatures] in GHC.Hs.Decls
  ; return $ hsTvbAllKinded tyvars && no_rhs_kvs && not non_cusk_newtype
  }

{- Note [Unlifted Newtypes and CUSKs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When unlifted newtypes are enabled, a newtype must have a kind signature
in order to be considered have a CUSK. This is because the flow of
kind inference works differently. Consider:

  newtype Foo = FooC Int

When UnliftedNewtypes is disabled, we decide that Foo has kind
`TYPE 'LiftedRep` without looking inside the data constructor. So, we
can say that Foo has a CUSK. However, when UnliftedNewtypes is enabled,
we fill in the kind of Foo as a metavar that gets solved by unification
with the kind of the field inside FooC (that is, Int, whose kind is
`TYPE 'LiftedRep`). But since we have to look inside the data constructors
to figure out the kind signature of Foo, it does not have a CUSK.

See Note [Implementation of UnliftedNewtypes] for where this fits in to
the broader picture of UnliftedNewtypes.
-}

-- "type" and "type instance" declarations
rnTySyn :: HsDocContext -> LHsType GhcPs -> RnM (LHsType GhcRn, FreeVars)
rnTySyn doc rhs = rnLHsType doc rhs

rnDataDefn :: HsDocContext -> HsDataDefn GhcPs
           -> RnM (HsDataDefn GhcRn, FreeVars)
rnDataDefn doc (HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                           , dd_ctxt = context, dd_cons = condecls
                           , dd_kindSig = m_sig, dd_derivs = derivs })
  = do  { checkTc (h98_style || null (unLoc context))
                  (badGadtStupidTheta doc)

        ; (m_sig', sig_fvs) <- case m_sig of
             Just sig -> first Just <$> rnLHsKind doc sig
             Nothing  -> return (Nothing, emptyFVs)
        ; (context', fvs1) <- rnContext doc context
        ; (derivs',  fvs3) <- rn_derivs derivs

        -- For the constructor declarations, drop the LocalRdrEnv
        -- in the GADT case, where the type variables in the declaration
        -- do not scope over the constructor signatures
        -- data T a where { T1 :: forall b. b-> b }
        ; let { zap_lcl_env | h98_style = \ thing -> thing
                            | otherwise = setLocalRdrEnv emptyLocalRdrEnv }
        ; (condecls', con_fvs) <- zap_lcl_env $ rnConDecls condecls
           -- No need to check for duplicate constructor decls
           -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn

        ; let all_fvs = fvs1 `plusFV` fvs3 `plusFV`
                        con_fvs `plusFV` sig_fvs
        ; return ( HsDataDefn { dd_ext = noExtField
                              , dd_ND = new_or_data, dd_cType = cType
                              , dd_ctxt = context', dd_kindSig = m_sig'
                              , dd_cons = condecls'
                              , dd_derivs = derivs' }
                 , all_fvs )
        }
  where
    h98_style = case condecls of  -- Note [Stupid theta]
                     (L _ (ConDeclGADT {}))                    : _ -> False
                     _                                             -> True

    rn_derivs (L loc ds)
      = do { deriv_strats_ok <- xoptM LangExt.DerivingStrategies
           ; failIfTc (lengthExceeds ds 1 && not deriv_strats_ok)
               multipleDerivClausesErr
           ; (ds', fvs) <- mapFvRn (rnLHsDerivingClause doc) ds
           ; return (L loc ds', fvs) }

warnNoDerivStrat :: Maybe (LDerivStrategy GhcRn)
                 -> SrcSpan
                 -> RnM ()
warnNoDerivStrat mds loc
  = do { dyn_flags <- getDynFlags
       ; when (wopt Opt_WarnMissingDerivingStrategies dyn_flags) $
           case mds of
             Nothing -> addWarnAt
               (Reason Opt_WarnMissingDerivingStrategies)
               loc
               (if xopt LangExt.DerivingStrategies dyn_flags
                 then no_strat_warning
                 else no_strat_warning $+$ deriv_strat_nenabled
               )
             _ -> pure ()
       }
  where
    no_strat_warning :: SDoc
    no_strat_warning = text "No deriving strategy specified. Did you want stock"
                       <> text ", newtype, or anyclass?"
    deriv_strat_nenabled :: SDoc
    deriv_strat_nenabled = text "Use DerivingStrategies to specify a strategy."

rnLHsDerivingClause :: HsDocContext -> LHsDerivingClause GhcPs
                    -> RnM (LHsDerivingClause GhcRn, FreeVars)
rnLHsDerivingClause doc
                (L loc (HsDerivingClause
                              { deriv_clause_ext = noExtField
                              , deriv_clause_strategy = dcs
                              , deriv_clause_tys = dct }))
  = do { (dcs', dct', fvs)
           <- rnLDerivStrategy doc dcs $ rn_deriv_clause_tys dct
       ; warnNoDerivStrat dcs' loc
       ; pure ( L loc (HsDerivingClause { deriv_clause_ext = noExtField
                                        , deriv_clause_strategy = dcs'
                                        , deriv_clause_tys = dct' })
              , fvs ) }
  where
    rn_deriv_clause_tys :: LDerivClauseTys GhcPs
                        -> RnM (LDerivClauseTys GhcRn, FreeVars)
    rn_deriv_clause_tys (L l dct) = case dct of
      DctSingle x ty -> do
        (ty', fvs) <- rn_clause_pred ty
        pure (L l (DctSingle x ty'), fvs)
      DctMulti x tys -> do
        (tys', fvs) <- mapFvRn rn_clause_pred tys
        pure (L l (DctMulti x tys'), fvs)

    rn_clause_pred :: LHsSigType GhcPs -> RnM (LHsSigType GhcRn, FreeVars)
    rn_clause_pred pred_ty = do
      let inf_err = Just (text "Inferred type variables are not allowed")
      checkInferredVars doc inf_err pred_ty
      ret@(pred_ty', _) <- rnHsSigType doc TypeLevel pred_ty
      -- Check if there are any nested `forall`s, which are illegal in a
      -- `deriving` clause.
      -- See Note [No nested foralls or contexts in instance types]
      -- (Wrinkle: Derived instances) in GHC.Hs.Type.
      addNoNestedForallsContextsErr doc (text "Derived class type")
        (getLHsInstDeclHead pred_ty')
      pure ret

rnLDerivStrategy :: forall a.
                    HsDocContext
                 -> Maybe (LDerivStrategy GhcPs)
                 -> RnM (a, FreeVars)
                 -> RnM (Maybe (LDerivStrategy GhcRn), a, FreeVars)
rnLDerivStrategy doc mds thing_inside
  = case mds of
      Nothing -> boring_case Nothing
      Just (L loc ds) ->
        setSrcSpan loc $ do
          (ds', thing, fvs) <- rn_deriv_strat ds
          pure (Just (L loc ds'), thing, fvs)
  where
    rn_deriv_strat :: DerivStrategy GhcPs
                   -> RnM (DerivStrategy GhcRn, a, FreeVars)
    rn_deriv_strat ds = do
      let extNeeded :: LangExt.Extension
          extNeeded
            | ViaStrategy{} <- ds
            = LangExt.DerivingVia
            | otherwise
            = LangExt.DerivingStrategies

      unlessXOptM extNeeded $
        failWith $ illegalDerivStrategyErr ds

      case ds of
        StockStrategy    -> boring_case StockStrategy
        AnyclassStrategy -> boring_case AnyclassStrategy
        NewtypeStrategy  -> boring_case NewtypeStrategy
        ViaStrategy via_ty ->
          do checkInferredVars doc inf_err via_ty
             (via_ty', fvs1) <- rnHsSigType doc TypeLevel via_ty
             let HsSig { sig_bndrs = via_outer_bndrs
                       , sig_body  = via_body } = unLoc via_ty'
                 via_tvs = hsOuterTyVarNames via_outer_bndrs
             -- Check if there are any nested `forall`s, which are illegal in a
             -- `via` type.
             -- See Note [No nested foralls or contexts in instance types]
             -- (Wrinkle: Derived instances) in GHC.Hs.Type.
             addNoNestedForallsContextsErr doc
               (quotes (text "via") <+> text "type") via_body
             (thing, fvs2) <- extendTyVarEnvFVRn via_tvs thing_inside
             pure (ViaStrategy via_ty', thing, fvs1 `plusFV` fvs2)

    inf_err = Just (text "Inferred type variables are not allowed")

    boring_case :: ds -> RnM (ds, a, FreeVars)
    boring_case ds = do
      (thing, fvs) <- thing_inside
      pure (ds, thing, fvs)

badGadtStupidTheta :: HsDocContext -> SDoc
badGadtStupidTheta _
  = vcat [text "No context is allowed on a GADT-style data declaration",
          text "(You can put a context on each constructor, though.)"]

illegalDerivStrategyErr :: DerivStrategy GhcPs -> SDoc
illegalDerivStrategyErr ds
  = vcat [ text "Illegal deriving strategy" <> colon <+> derivStrategyName ds
         , text enableStrategy ]

  where
    enableStrategy :: String
    enableStrategy
      | ViaStrategy{} <- ds
      = "Use DerivingVia to enable this extension"
      | otherwise
      = "Use DerivingStrategies to enable this extension"

multipleDerivClausesErr :: SDoc
multipleDerivClausesErr
  = vcat [ text "Illegal use of multiple, consecutive deriving clauses"
         , text "Use DerivingStrategies to allow this" ]

rnFamDecl :: Maybe Name -- Just cls => this FamilyDecl is nested
                        --             inside an *class decl* for cls
                        --             used for associated types
          -> FamilyDecl GhcPs
          -> RnM (FamilyDecl GhcRn, FreeVars)
rnFamDecl mb_cls (FamilyDecl { fdLName = tycon, fdTyVars = tyvars
                             , fdFixity = fixity
                             , fdInfo = info, fdResultSig = res_sig
                             , fdInjectivityAnn = injectivity })
  = do { tycon' <- lookupLocatedTopBndrRn tycon
       ; ((tyvars', res_sig', injectivity'), fv1) <-
            bindHsQTyVars doc mb_cls kvs tyvars $ \ tyvars' _ ->
            do { let rn_sig = rnFamResultSig doc
               ; (res_sig', fv_kind) <- wrapLocFstM rn_sig res_sig
               ; injectivity' <- traverse (rnInjectivityAnn tyvars' res_sig')
                                          injectivity
               ; return ( (tyvars', res_sig', injectivity') , fv_kind ) }
       ; (info', fv2) <- rn_info info
       ; return (FamilyDecl { fdExt = noExtField
                            , fdLName = tycon', fdTyVars = tyvars'
                            , fdFixity = fixity
                            , fdInfo = info', fdResultSig = res_sig'
                            , fdInjectivityAnn = injectivity' }
                , fv1 `plusFV` fv2) }
  where
     doc = TyFamilyCtx tycon
     kvs = extractRdrKindSigVars res_sig

     ----------------------
     rn_info :: FamilyInfo GhcPs -> RnM (FamilyInfo GhcRn, FreeVars)
     rn_info (ClosedTypeFamily (Just eqns))
       = do { (eqns', fvs)
                <- rnList (rnTyFamInstEqn (NonAssocTyFamEqn ClosedTyFam)) eqns
                                          -- no class context
            ; return (ClosedTypeFamily (Just eqns'), fvs) }
     rn_info (ClosedTypeFamily Nothing)
       = return (ClosedTypeFamily Nothing, emptyFVs)
     rn_info OpenTypeFamily = return (OpenTypeFamily, emptyFVs)
     rn_info DataFamily     = return (DataFamily, emptyFVs)

rnFamResultSig :: HsDocContext
               -> FamilyResultSig GhcPs
               -> RnM (FamilyResultSig GhcRn, FreeVars)
rnFamResultSig _ (NoSig _)
   = return (NoSig noExtField, emptyFVs)
rnFamResultSig doc (KindSig _ kind)
   = do { (rndKind, ftvs) <- rnLHsKind doc kind
        ;  return (KindSig noExtField rndKind, ftvs) }
rnFamResultSig doc (TyVarSig _ tvbndr)
   = do { -- `TyVarSig` tells us that user named the result of a type family by
          -- writing `= tyvar` or `= (tyvar :: kind)`. In such case we want to
          -- be sure that the supplied result name is not identical to an
          -- already in-scope type variable from an enclosing class.
          --
          --  Example of disallowed declaration:
          --         class C a b where
          --            type F b = a | a -> b
          rdr_env <- getLocalRdrEnv
       ;  let resName = hsLTyVarName tvbndr
       ;  when (resName `elemLocalRdrEnv` rdr_env) $
          addErrAt (getLoc tvbndr) $
                     (hsep [ text "Type variable", quotes (ppr resName) <> comma
                           , text "naming a type family result,"
                           ] $$
                      text "shadows an already bound type variable")

       ; bindLHsTyVarBndr doc Nothing -- This might be a lie, but it's used for
                                      -- scoping checks that are irrelevant here
                          tvbndr $ \ tvbndr' ->
         return (TyVarSig noExtField tvbndr', unitFV (hsLTyVarName tvbndr')) }

-- Note [Renaming injectivity annotation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- During renaming of injectivity annotation we have to make several checks to
-- make sure that it is well-formed.  At the moment injectivity annotation
-- consists of a single injectivity condition, so the terms "injectivity
-- annotation" and "injectivity condition" might be used interchangeably.  See
-- Note [Injectivity annotation] for a detailed discussion of currently allowed
-- injectivity annotations.
--
-- Checking LHS is simple because the only type variable allowed on the LHS of
-- injectivity condition is the variable naming the result in type family head.
-- Example of disallowed annotation:
--
--     type family Foo a b = r | b -> a
--
-- Verifying RHS of injectivity consists of checking that:
--
--  1. only variables defined in type family head appear on the RHS (kind
--     variables are also allowed).  Example of disallowed annotation:
--
--        type family Foo a = r | r -> b
--
--  2. for associated types the result variable does not shadow any of type
--     class variables. Example of disallowed annotation:
--
--        class Foo a b where
--           type F a = b | b -> a
--
-- Breaking any of these assumptions results in an error.

-- | Rename injectivity annotation. Note that injectivity annotation is just the
-- part after the "|".  Everything that appears before it is renamed in
-- rnFamDecl.
rnInjectivityAnn :: LHsQTyVars GhcRn           -- ^ Type variables declared in
                                               --   type family head
                 -> LFamilyResultSig GhcRn     -- ^ Result signature
                 -> LInjectivityAnn GhcPs      -- ^ Injectivity annotation
                 -> RnM (LInjectivityAnn GhcRn)
rnInjectivityAnn tvBndrs (L _ (TyVarSig _ resTv))
                 (L srcSpan (InjectivityAnn injFrom injTo))
 = do
   { (injDecl'@(L _ (InjectivityAnn injFrom' injTo')), noRnErrors)
          <- askNoErrs $
             bindLocalNames [hsLTyVarName resTv] $
             -- The return type variable scopes over the injectivity annotation
             -- e.g.   type family F a = (r::*) | r -> a
             do { injFrom' <- rnLTyVar injFrom
                ; injTo'   <- mapM rnLTyVar injTo
                ; return $ L srcSpan (InjectivityAnn injFrom' injTo') }

   ; let tvNames  = Set.fromList $ hsAllLTyVarNames tvBndrs
         resName  = hsLTyVarName resTv
         -- See Note [Renaming injectivity annotation]
         lhsValid = EQ == (stableNameCmp resName (unLoc injFrom'))
         rhsValid = Set.fromList (map unLoc injTo') `Set.difference` tvNames

   -- if renaming of type variables ended with errors (eg. there were
   -- not-in-scope variables) don't check the validity of injectivity
   -- annotation. This gives better error messages.
   ; when (noRnErrors && not lhsValid) $
        addErrAt (getLoc injFrom)
              ( vcat [ text $ "Incorrect type variable on the LHS of "
                           ++ "injectivity condition"
              , nest 5
              ( vcat [ text "Expected :" <+> ppr resName
                     , text "Actual   :" <+> ppr injFrom ])])

   ; when (noRnErrors && not (Set.null rhsValid)) $
      do { let errorVars = Set.toList rhsValid
         ; addErrAt srcSpan $ ( hsep
                        [ text "Unknown type variable" <> plural errorVars
                        , text "on the RHS of injectivity condition:"
                        , interpp'SP errorVars ] ) }

   ; return injDecl' }

-- We can only hit this case when the user writes injectivity annotation without
-- naming the result:
--
--   type family F a | result -> a
--   type family F a :: * | result -> a
--
-- So we rename injectivity annotation like we normally would except that
-- this time we expect "result" to be reported not in scope by rnLTyVar.
rnInjectivityAnn _ _ (L srcSpan (InjectivityAnn injFrom injTo)) =
   setSrcSpan srcSpan $ do
   (injDecl', _) <- askNoErrs $ do
     injFrom' <- rnLTyVar injFrom
     injTo'   <- mapM rnLTyVar injTo
     return $ L srcSpan (InjectivityAnn injFrom' injTo')
   return $ injDecl'

{-
Note [Stupid theta]
~~~~~~~~~~~~~~~~~~~
#3850 complains about a regression wrt 6.10 for
     data Show a => T a
There is no reason not to allow the stupid theta if there are no data
constructors.  It's still stupid, but does no harm, and I don't want
to cause programs to break unnecessarily (notably HList).  So if there
are no data constructors we allow h98_style = True
-}


{- *****************************************************
*                                                      *
     Support code for type/data declarations
*                                                      *
***************************************************** -}

-----------------
rnConDecls :: [LConDecl GhcPs] -> RnM ([LConDecl GhcRn], FreeVars)
rnConDecls = mapFvRn (wrapLocFstM rnConDecl)

rnConDecl :: ConDecl GhcPs -> RnM (ConDecl GhcRn, FreeVars)
rnConDecl decl@(ConDeclH98 { con_name = name, con_ex_tvs = ex_tvs
                           , con_mb_cxt = mcxt, con_args = args
                           , con_doc = mb_doc, con_forall = forall })
  = do  { _        <- addLocM checkConName name
        ; new_name <- lookupLocatedTopBndrRn name

        -- We bind no implicit binders here; this is just like
        -- a nested HsForAllTy.  E.g. consider
        --         data T a = forall (b::k). MkT (...)
        -- The 'k' will already be in scope from the bindHsQTyVars
        -- for the data decl itself. So we'll get
        --         data T {k} a = ...
        -- And indeed we may later discover (a::k).  But that's the
        -- scoping we get.  So no implicit binders at the existential forall

        ; let ctxt = ConDeclCtx [new_name]
        ; bindLHsTyVarBndrs ctxt WarnUnusedForalls
                            Nothing ex_tvs $ \ new_ex_tvs ->
    do  { (new_context, fvs1) <- rnMbContext ctxt mcxt
        ; (new_args,    fvs2) <- rnConDeclH98Details (unLoc new_name) ctxt args
        ; let all_fvs  = fvs1 `plusFV` fvs2
        ; traceRn "rnConDecl (ConDeclH98)" (ppr name <+> vcat
             [ text "ex_tvs:" <+> ppr ex_tvs
             , text "new_ex_dqtvs':" <+> ppr new_ex_tvs ])

        ; return (decl { con_ext = noExtField
                       , con_name = new_name, con_ex_tvs = new_ex_tvs
                       , con_mb_cxt = new_context, con_args = new_args
                       , con_doc = mb_doc
                       , con_forall = forall }, -- Remove when #18311 is fixed
                  all_fvs) }}

rnConDecl (ConDeclGADT { con_names   = names
                       , con_bndrs   = L l outer_bndrs
                       , con_mb_cxt  = mcxt
                       , con_g_args  = args
                       , con_res_ty  = res_ty
                       , con_doc     = mb_doc })
  = do  { mapM_ (addLocM checkConName) names
        ; new_names <- mapM lookupLocatedTopBndrRn names

        ; let -- We must ensure that we extract the free tkvs in left-to-right
              -- order of their appearance in the constructor type.
              -- That order governs the order the implicitly-quantified type
              -- variable, and hence the order needed for visible type application
              -- See #14808.
              implicit_bndrs =
                extractHsOuterTvBndrs outer_bndrs           $
                extractHsTysRdrTyVars (hsConDeclTheta mcxt) $
                extractConDeclGADTDetailsTyVars args        $
                extractHsTysRdrTyVars [res_ty] []

        ; let ctxt = ConDeclCtx new_names

        ; bindHsOuterTyVarBndrs ctxt Nothing implicit_bndrs outer_bndrs $ \outer_bndrs' ->
    do  { (new_cxt, fvs1)    <- rnMbContext ctxt mcxt
        ; (new_args, fvs2)   <- rnConDeclGADTDetails (unLoc (head new_names)) ctxt args
        ; (new_res_ty, fvs3) <- rnLHsType ctxt res_ty

         -- Ensure that there are no nested `forall`s or contexts, per
         -- Note [GADT abstract syntax] (Wrinkle: No nested foralls or contexts)
         -- in GHC.Hs.Type.
       ; addNoNestedForallsContextsErr ctxt
           (text "GADT constructor type signature") new_res_ty

        ; let all_fvs = fvs1 `plusFV` fvs2 `plusFV` fvs3

        ; traceRn "rnConDecl (ConDeclGADT)"
            (ppr names $$ ppr outer_bndrs')
        ; return (ConDeclGADT { con_g_ext = noExtField, con_names = new_names
                              , con_bndrs = L l outer_bndrs', con_mb_cxt = new_cxt
                              , con_g_args = new_args, con_res_ty = new_res_ty
                              , con_doc = mb_doc },
                  all_fvs) } }

rnMbContext :: HsDocContext -> Maybe (LHsContext GhcPs)
            -> RnM (Maybe (LHsContext GhcRn), FreeVars)
rnMbContext _    Nothing    = return (Nothing, emptyFVs)
rnMbContext doc (Just cxt) = do { (ctx',fvs) <- rnContext doc cxt
                                ; return (Just ctx',fvs) }

rnConDeclH98Details ::
      Name
   -> HsDocContext
   -> HsConDeclH98Details GhcPs
   -> RnM (HsConDeclH98Details GhcRn, FreeVars)
rnConDeclH98Details _ doc (PrefixCon _ tys)
  = do { (new_tys, fvs) <- mapFvRn (rnScaledLHsType doc) tys
       ; return (PrefixCon noTypeArgs new_tys, fvs) }
rnConDeclH98Details _ doc (InfixCon ty1 ty2)
  = do { (new_ty1, fvs1) <- rnScaledLHsType doc ty1
       ; (new_ty2, fvs2) <- rnScaledLHsType doc ty2
       ; return (InfixCon new_ty1 new_ty2, fvs1 `plusFV` fvs2) }
rnConDeclH98Details con doc (RecCon flds)
  = do { (new_flds, fvs) <- rnRecConDeclFields con doc flds
       ; return (RecCon new_flds, fvs) }

rnConDeclGADTDetails ::
      Name
   -> HsDocContext
   -> HsConDeclGADTDetails GhcPs
   -> RnM (HsConDeclGADTDetails GhcRn, FreeVars)
rnConDeclGADTDetails _ doc (PrefixConGADT tys)
  = do { (new_tys, fvs) <- mapFvRn (rnScaledLHsType doc) tys
       ; return (PrefixConGADT new_tys, fvs) }
rnConDeclGADTDetails con doc (RecConGADT flds)
  = do { (new_flds, fvs) <- rnRecConDeclFields con doc flds
       ; return (RecConGADT new_flds, fvs) }

rnRecConDeclFields ::
     Name
  -> HsDocContext
  -> Located [LConDeclField GhcPs]
  -> RnM (Located [LConDeclField GhcRn], FreeVars)
rnRecConDeclFields con doc (L l fields)
  = do  { fls <- lookupConstructorFields con
        ; (new_fields, fvs) <- rnConDeclFields doc fls fields
                -- No need to check for duplicate fields
                -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn
        ; pure (L l new_fields, fvs) }

-------------------------------------------------

-- | Brings pattern synonym names and also pattern synonym selectors
-- from record pattern synonyms into scope.
extendPatSynEnv :: HsValBinds GhcPs -> MiniFixityEnv
                -> ([Name] -> TcRnIf TcGblEnv TcLclEnv a) -> TcM a
extendPatSynEnv val_decls local_fix_env thing = do {
     names_with_fls <- new_ps val_decls
   ; let pat_syn_bndrs = concat [ name: map flSelector fields
                                | (name, fields) <- names_with_fls ]
   ; let avails = map avail (map fst names_with_fls)
               ++ map availField (concatMap snd names_with_fls)
   ; (gbl_env, lcl_env) <- extendGlobalRdrEnvRn avails local_fix_env

   ; let field_env' = extendNameEnvList (tcg_field_env gbl_env) names_with_fls
         final_gbl_env = gbl_env { tcg_field_env = field_env' }
   ; setEnvs (final_gbl_env, lcl_env) (thing pat_syn_bndrs) }
  where
    new_ps :: HsValBinds GhcPs -> TcM [(Name, [FieldLabel])]
    new_ps (ValBinds _ binds _) = foldrM new_ps' [] binds
    new_ps _ = panic "new_ps"

    new_ps' :: LHsBindLR GhcPs GhcPs
            -> [(Name, [FieldLabel])]
            -> TcM [(Name, [FieldLabel])]
    new_ps' bind names
      | (L bind_loc (PatSynBind _ (PSB { psb_id = L _ n
                                       , psb_args = RecCon as }))) <- bind
      = do
          bnd_name <- newTopSrcBinder (L bind_loc n)
          let field_occs = map ((\ f -> L (getLoc (rdrNameFieldOcc f)) f) . recordPatSynField) as
          overload_ok <- xoptM LangExt.DuplicateRecordFields
          flds <- mapM (newRecordSelector overload_ok [bnd_name]) field_occs
          return ((bnd_name, flds): names)
      | L bind_loc (PatSynBind _ (PSB { psb_id = L _ n})) <- bind
      = do
        bnd_name <- newTopSrcBinder (L bind_loc n)
        return ((bnd_name, []): names)
      | otherwise
      = return names

{-
*********************************************************
*                                                      *
\subsection{Support code to rename types}
*                                                      *
*********************************************************
-}

rnFds :: [LHsFunDep GhcPs] -> RnM [LHsFunDep GhcRn]
rnFds fds
  = mapM (wrapLocM rn_fds) fds
  where
    rn_fds (tys1, tys2)
      = do { tys1' <- rnHsTyVars tys1
           ; tys2' <- rnHsTyVars tys2
           ; return (tys1', tys2') }

rnHsTyVars :: [Located RdrName] -> RnM [Located Name]
rnHsTyVars tvs  = mapM rnHsTyVar tvs

rnHsTyVar :: Located RdrName -> RnM (Located Name)
rnHsTyVar (L l tyvar) = do
  tyvar' <- lookupOccRn tyvar
  return (L l tyvar')

{-
*********************************************************
*                                                      *
        findSplice
*                                                      *
*********************************************************

This code marches down the declarations, looking for the first
Template Haskell splice.  As it does so it
        a) groups the declarations into a HsGroup
        b) runs any top-level quasi-quotes
-}

findSplice :: [LHsDecl GhcPs]
           -> RnM (HsGroup GhcPs, Maybe (SpliceDecl GhcPs, [LHsDecl GhcPs]))
findSplice ds = addl emptyRdrGroup ds

addl :: HsGroup GhcPs -> [LHsDecl GhcPs]
     -> RnM (HsGroup GhcPs, Maybe (SpliceDecl GhcPs, [LHsDecl GhcPs]))
-- This stuff reverses the declarations (again) but it doesn't matter
addl gp []           = return (gp, Nothing)
addl gp (L l d : ds) = add gp l d ds


add :: HsGroup GhcPs -> SrcSpan -> HsDecl GhcPs -> [LHsDecl GhcPs]
    -> RnM (HsGroup GhcPs, Maybe (SpliceDecl GhcPs, [LHsDecl GhcPs]))

-- #10047: Declaration QuasiQuoters are expanded immediately, without
--         causing a group split
add gp _ (SpliceD _ (SpliceDecl _ (L _ qq@HsQuasiQuote{}) _)) ds
  = do { (ds', _) <- rnTopSpliceDecls qq
       ; addl gp (ds' ++ ds)
       }

add gp loc (SpliceD _ splice@(SpliceDecl _ _ flag)) ds
  = do { -- We've found a top-level splice.  If it is an *implicit* one
         -- (i.e. a naked top level expression)
         case flag of
           ExplicitSplice -> return ()
           ImplicitSplice -> do { th_on <- xoptM LangExt.TemplateHaskell
                                ; unless th_on $ setSrcSpan loc $
                                  failWith badImplicitSplice }

       ; return (gp, Just (splice, ds)) }
  where
    badImplicitSplice = text "Parse error: module header, import declaration"
                     $$ text "or top-level declaration expected."
                     -- The compiler should suggest the above, and not using
                     -- TemplateHaskell since the former suggestion is more
                     -- relevant to the larger base of users.
                     -- See #12146 for discussion.

-- Class declarations: added to the TyClGroup
add gp@(HsGroup {hs_tyclds = ts}) l (TyClD _ d) ds
  = addl (gp { hs_tyclds = add_tycld (L l d) ts }) ds

-- Signatures: fixity sigs go a different place than all others
add gp@(HsGroup {hs_fixds = ts}) l (SigD _ (FixSig _ f)) ds
  = addl (gp {hs_fixds = L l f : ts}) ds

-- Standalone kind signatures: added to the TyClGroup
add gp@(HsGroup {hs_tyclds = ts}) l (KindSigD _ s) ds
  = addl (gp {hs_tyclds = add_kisig (L l s) ts}) ds

add gp@(HsGroup {hs_valds = ts}) l (SigD _ d) ds
  = addl (gp {hs_valds = add_sig (L l d) ts}) ds

-- Value declarations: use add_bind
add gp@(HsGroup {hs_valds  = ts}) l (ValD _ d) ds
  = addl (gp { hs_valds = add_bind (L l d) ts }) ds

-- Role annotations: added to the TyClGroup
add gp@(HsGroup {hs_tyclds = ts}) l (RoleAnnotD _ d) ds
  = addl (gp { hs_tyclds = add_role_annot (L l d) ts }) ds

-- NB instance declarations go into TyClGroups. We throw them into the first
-- group, just as we do for the TyClD case. The renamer will go on to group
-- and order them later.
add gp@(HsGroup {hs_tyclds = ts})  l (InstD _ d) ds
  = addl (gp { hs_tyclds = add_instd (L l d) ts }) ds

-- The rest are routine
add gp@(HsGroup {hs_derivds = ts})  l (DerivD _ d) ds
  = addl (gp { hs_derivds = L l d : ts }) ds
add gp@(HsGroup {hs_defds  = ts})  l (DefD _ d) ds
  = addl (gp { hs_defds = L l d : ts }) ds
add gp@(HsGroup {hs_fords  = ts}) l (ForD _ d) ds
  = addl (gp { hs_fords = L l d : ts }) ds
add gp@(HsGroup {hs_warnds  = ts})  l (WarningD _ d) ds
  = addl (gp { hs_warnds = L l d : ts }) ds
add gp@(HsGroup {hs_annds  = ts}) l (AnnD _ d) ds
  = addl (gp { hs_annds = L l d : ts }) ds
add gp@(HsGroup {hs_ruleds  = ts}) l (RuleD _ d) ds
  = addl (gp { hs_ruleds = L l d : ts }) ds
add gp l (DocD _ d) ds
  = addl (gp { hs_docs = (L l d) : (hs_docs gp) })  ds

add_tycld :: LTyClDecl (GhcPass p) -> [TyClGroup (GhcPass p)]
          -> [TyClGroup (GhcPass p)]
add_tycld d []       = [TyClGroup { group_ext    = noExtField
                                  , group_tyclds = [d]
                                  , group_kisigs = []
                                  , group_roles  = []
                                  , group_instds = []
                                  }
                       ]
add_tycld d (ds@(TyClGroup { group_tyclds = tyclds }):dss)
  = ds { group_tyclds = d : tyclds } : dss

add_instd :: LInstDecl (GhcPass p) -> [TyClGroup (GhcPass p)]
          -> [TyClGroup (GhcPass p)]
add_instd d []       = [TyClGroup { group_ext    = noExtField
                                  , group_tyclds = []
                                  , group_kisigs = []
                                  , group_roles  = []
                                  , group_instds = [d]
                                  }
                       ]
add_instd d (ds@(TyClGroup { group_instds = instds }):dss)
  = ds { group_instds = d : instds } : dss

add_role_annot :: LRoleAnnotDecl (GhcPass p) -> [TyClGroup (GhcPass p)]
               -> [TyClGroup (GhcPass p)]
add_role_annot d [] = [TyClGroup { group_ext    = noExtField
                                 , group_tyclds = []
                                 , group_kisigs = []
                                 , group_roles  = [d]
                                 , group_instds = []
                                 }
                      ]
add_role_annot d (tycls@(TyClGroup { group_roles = roles }) : rest)
  = tycls { group_roles = d : roles } : rest

add_kisig :: LStandaloneKindSig (GhcPass p)
         -> [TyClGroup (GhcPass p)] -> [TyClGroup (GhcPass p)]
add_kisig d [] = [TyClGroup { group_ext    = noExtField
                            , group_tyclds = []
                            , group_kisigs = [d]
                            , group_roles  = []
                            , group_instds = []
                            }
                 ]
add_kisig d (tycls@(TyClGroup { group_kisigs = kisigs }) : rest)
  = tycls { group_kisigs = d : kisigs } : rest

add_bind :: LHsBind a -> HsValBinds a -> HsValBinds a
add_bind b (ValBinds x bs sigs) = ValBinds x (bs `snocBag` b) sigs
add_bind _ (XValBindsLR {})     = panic "GHC.Rename.Module.add_bind"

add_sig :: LSig (GhcPass a) -> HsValBinds (GhcPass a) -> HsValBinds (GhcPass a)
add_sig s (ValBinds x bs sigs) = ValBinds x bs (s:sigs)
add_sig _ (XValBindsLR {})     = panic "GHC.Rename.Module.add_sig"

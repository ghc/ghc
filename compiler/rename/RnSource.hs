{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[RnSource]{Main pass of renamer}
-}

{-# LANGUAGE CPP, ScopedTypeVariables #-}

module RnSource (
        rnSrcDecls, addTcgDUs, findSplice
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} RnExpr( rnLExpr )
import {-# SOURCE #-} RnSplice ( rnSpliceDecl, rnTopSpliceDecls )

import HsSyn
import FieldLabel
import RdrName
import RnTypes
import RnBinds
import RnEnv
import RnNames
import RnHsDoc          ( rnHsDoc, rnMbLHsDoc )
import TcAnnotations    ( annCtxt )
import TcRnMonad

import ForeignCall      ( CCallTarget(..) )
import Module
import HscTypes         ( Warnings(..), plusWarns )
import Class            ( FunDep )
import PrelNames        ( applicativeClassName, pureAName, thenAName
                        , monadClassName, returnMName, thenMName
                        , monadFailClassName, failMName, failMName_preMFP
                        , semigroupClassName, sappendName
                        , monoidClassName, mappendName
                        )
import Name
import NameSet
import NameEnv
import Avail
import Outputable
import Bag
import BasicTypes       ( DerivStrategy, RuleName, pprRuleName )
import FastString
import SrcLoc
import DynFlags
import Util             ( debugIsOn, lengthExceeds, partitionWith )
import HscTypes         ( HscEnv, hsc_dflags )
import ListSetOps       ( findDupsEq, removeDups, equivClasses )
import Digraph          ( SCC, flattenSCC, flattenSCCs
                        , stronglyConnCompFromEdgedVerticesUniq )
import UniqFM
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Control.Arrow ( first )
import Data.List ( sortBy, mapAccumL )
import Data.Maybe ( isJust )
import qualified Data.Set as Set ( difference, fromList, toList, null )

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
rnSrcDecls :: HsGroup RdrName -> RnM (TcGblEnv, HsGroup Name)
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
                            hs_vects   = vect_decls,
                            hs_docs    = docs })
 = do {
   -- (A) Process the fixity declarations, creating a mapping from
   --     FastStrings to FixItems.
   --     Also checks for duplicates.
   local_fix_env <- makeMiniFixityEnv fix_decls ;

   -- (B) Bring top level binders (and their fixities) into scope,
   --     *except* for the value bindings, which get done in step (D)
   --     with collectHsIdBinders. However *do* include
   --
   --        * Class ops, data constructors, and record fields,
   --          because they do not have value declarations.
   --          Aso step (C) depends on datacons and record fields
   --
   --        * For hs-boot files, include the value signatures
   --          Again, they have no value declarations
   --
   (tc_envs, tc_bndrs) <- getLocalNonValBinders local_fix_env group ;


   setEnvs tc_envs $ do {

   failIfErrsM ; -- No point in continuing if (say) we have duplicate declarations

   -- (D1) Bring pattern synonyms into scope.
   --      Need to do this before (D2) because rnTopBindsLHS
   --      looks up those pattern synonyms (Trac #9889)

   extendPatSynEnv val_decls local_fix_env $ \pat_syn_bndrs -> do {

   -- (D2) Rename the left-hand sides of the value bindings.
   --     This depends on everything from (B) being in scope,
   --     and on (C) for resolving record wild cards.
   --     It uses the fixity env from (A) to bind fixities for view patterns.
   new_lhs <- rnTopBindsLHS local_fix_env val_decls ;

   -- Bind the LHSes (and their fixities) in the global rdr environment
   let { id_bndrs = collectHsIdBinders new_lhs } ;  -- Excludes pattern-synonym binders
                                                    -- They are already in scope
   traceRn "rnSrcDecls" (ppr id_bndrs) ;
   tc_envs <- extendGlobalRdrEnvRn (map avail id_bndrs) local_fix_env ;
   traceRn "D2" (ppr (tcg_rdr_env (fst tc_envs)));
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
   rn_fix_decls <- rnSrcFixityDecls all_bndrs fix_decls ;

   -- Rename deprec decls;
   -- check for duplicates and ensure that deprecated things are defined locally
   -- at the moment, we don't keep these around past renaming
   rn_warns <- rnSrcWarnDecls all_bndrs warn_decls ;

   -- (H) Rename Everything else

   (rn_rule_decls,    src_fvs2) <- setXOptM LangExt.ScopedTypeVariables $
                                   rnList rnHsRuleDecls rule_decls ;
                           -- Inside RULES, scoped type variables are on
   (rn_vect_decls,    src_fvs3) <- rnList rnHsVectDecl    vect_decls ;
   (rn_foreign_decls, src_fvs4) <- rnList rnHsForeignDecl foreign_decls ;
   (rn_ann_decls,     src_fvs5) <- rnList rnAnnDecl       ann_decls ;
   (rn_default_decls, src_fvs6) <- rnList rnDefaultDecl   default_decls ;
   (rn_deriv_decls,   src_fvs7) <- rnList rnSrcDerivDecl  deriv_decls ;
   (rn_splice_decls,  src_fvs8) <- rnList rnSpliceDecl    splice_decls ;
      -- Haddock docs; no free vars
   rn_docs <- mapM (wrapLocM rnDocDecl) docs ;

   last_tcg_env <- getGblEnv ;
   -- (I) Compute the results and return
   let {rn_group = HsGroup { hs_valds   = rn_val_decls,
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
                             hs_vects  = rn_vect_decls,
                             hs_docs   = rn_docs } ;

        tcf_bndrs = hsTyClForeignBinders rn_tycl_decls rn_foreign_decls ;
        other_def  = (Just (mkNameSet tcf_bndrs), emptyNameSet) ;
        other_fvs  = plusFVs [src_fvs1, src_fvs2, src_fvs3, src_fvs4, src_fvs5,
                              src_fvs6, src_fvs7, src_fvs8] ;
                -- It is tiresome to gather the binders from type and class decls

        src_dus = [other_def] `plusDU` bind_dus `plusDU` usesOnly other_fvs ;
                -- Instance decls may have occurrences of things bound in bind_dus
                -- so we must put other_fvs last

        final_tcg_env = let tcg_env' = (last_tcg_env `addTcgDUs` src_dus)
                        in -- we return the deprecs in the env, not in the HsGroup above
                        tcg_env' { tcg_warns = tcg_warns tcg_env' `plusWarns` rn_warns };
       } ;
   traceRn "last" (ppr (tcg_rdr_env final_tcg_env)) ;
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
        HsDoc stuff
*                                                       *
*********************************************************
-}

rnDocDecl :: DocDecl -> RnM DocDecl
rnDocDecl (DocCommentNext doc) = do
  rn_doc <- rnHsDoc doc
  return (DocCommentNext rn_doc)
rnDocDecl (DocCommentPrev doc) = do
  rn_doc <- rnHsDoc doc
  return (DocCommentPrev rn_doc)
rnDocDecl (DocCommentNamed str doc) = do
  rn_doc <- rnHsDoc doc
  return (DocCommentNamed str rn_doc)
rnDocDecl (DocGroup lev doc) = do
  rn_doc <- rnHsDoc doc
  return (DocGroup lev rn_doc)

{-
*********************************************************
*                                                       *
        Source-code fixity declarations
*                                                       *
*********************************************************
-}

rnSrcFixityDecls :: NameSet -> [LFixitySig RdrName] -> RnM [LFixitySig Name]
-- Rename the fixity decls, so we can put
-- the renamed decls in the renamed syntax tree
-- Errors if the thing being fixed is not defined locally.
--
-- The returned FixitySigs are not actually used for anything,
-- except perhaps the GHCi API
rnSrcFixityDecls bndr_set fix_decls
  = do fix_decls <- mapM rn_decl fix_decls
       return (concat fix_decls)
  where
    sig_ctxt = TopSigCtxt bndr_set

    rn_decl :: LFixitySig RdrName -> RnM [LFixitySig Name]
        -- GHC extension: look up both the tycon and data con
        -- for con-like things; hence returning a list
        -- If neither are in scope, report an error; otherwise
        -- return a fixity sig for each (slightly odd)
    rn_decl (L loc (FixitySig fnames fixity))
      = do names <- mapM lookup_one fnames
           return [ L loc (FixitySig name fixity)
                  | name <- names ]

    lookup_one :: Located RdrName -> RnM [Located Name]
    lookup_one (L name_loc rdr_name)
      = setSrcSpan name_loc $
                    -- this lookup will fail if the definition isn't local
        do names <- lookupLocalTcNames sig_ctxt what rdr_name
           return [ L name_loc name | (_, name) <- names ]
    what = text "fixity signature"

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
rnSrcWarnDecls :: NameSet -> [LWarnDecls RdrName] -> RnM Warnings
rnSrcWarnDecls _ []
  = return NoWarnings

rnSrcWarnDecls bndr_set decls'
  = do { -- check for duplicates
       ; mapM_ (\ dups -> let (L loc rdr:lrdr':_) = dups
                          in addErrAt loc (dupWarnDecl lrdr' rdr))
               warn_rdr_dups
       ; pairs_s <- mapM (addLocM rn_deprec) decls
       ; return (WarnSome ((concat pairs_s))) }
 where
   decls = concatMap (\(L _ d) -> wd_warnings d) decls'

   sig_ctxt = TopSigCtxt bndr_set

   rn_deprec (Warning rdr_names txt)
       -- ensures that the names are defined locally
     = do { names <- concatMapM (lookupLocalTcNames sig_ctxt what . unLoc)
                                rdr_names
          ; return [(rdrNameOcc rdr, txt) | (rdr, _) <- names] }

   what = text "deprecation"

   warn_rdr_dups = findDupRdrNames $ concatMap (\(L _ (Warning ns _)) -> ns)
                                               decls

findDupRdrNames :: [Located RdrName] -> [[Located RdrName]]
findDupRdrNames = findDupsEq (\ x -> \ y -> rdrNameOcc (unLoc x) == rdrNameOcc (unLoc y))

-- look for duplicates among the OccNames;
-- we check that the names are defined above
-- invt: the lists returned by findDupsEq always have at least two elements

dupWarnDecl :: Located RdrName -> RdrName -> SDoc
-- Located RdrName -> DeprecDecl RdrName -> SDoc
dupWarnDecl (L loc _) rdr_name
  = vcat [text "Multiple warning declarations for" <+> quotes (ppr rdr_name),
          text "also at " <+> ppr loc]

{-
*********************************************************
*                                                      *
\subsection{Annotation declarations}
*                                                      *
*********************************************************
-}

rnAnnDecl :: AnnDecl RdrName -> RnM (AnnDecl Name, FreeVars)
rnAnnDecl ann@(HsAnnotation s provenance expr)
  = addErrCtxt (annCtxt ann) $
    do { (provenance', provenance_fvs) <- rnAnnProvenance provenance
       ; (expr', expr_fvs) <- setStage (Splice Untyped) $
                              rnLExpr expr
       ; return (HsAnnotation s provenance' expr',
                 provenance_fvs `plusFV` expr_fvs) }

rnAnnProvenance :: AnnProvenance RdrName -> RnM (AnnProvenance Name, FreeVars)
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

rnDefaultDecl :: DefaultDecl RdrName -> RnM (DefaultDecl Name, FreeVars)
rnDefaultDecl (DefaultDecl tys)
  = do { (tys', fvs) <- rnLHsTypes doc_str tys
       ; return (DefaultDecl tys', fvs) }
  where
    doc_str = DefaultDeclCtx

{-
*********************************************************
*                                                      *
\subsection{Foreign declarations}
*                                                      *
*********************************************************
-}

rnHsForeignDecl :: ForeignDecl RdrName -> RnM (ForeignDecl Name, FreeVars)
rnHsForeignDecl (ForeignImport { fd_name = name, fd_sig_ty = ty, fd_fi = spec })
  = do { topEnv :: HscEnv <- getTopEnv
       ; name' <- lookupLocatedTopBndrRn name
       ; (ty', fvs) <- rnHsSigType (ForeignDeclCtx name) ty

        -- Mark any PackageTarget style imports as coming from the current package
       ; let unitId = thisPackage $ hsc_dflags topEnv
             spec'      = patchForeignImport unitId spec

       ; return (ForeignImport { fd_name = name', fd_sig_ty = ty'
                               , fd_co = noForeignImportCoercionYet
                               , fd_fi = spec' }, fvs) }

rnHsForeignDecl (ForeignExport { fd_name = name, fd_sig_ty = ty, fd_fe = spec })
  = do { name' <- lookupLocatedOccRn name
       ; (ty', fvs) <- rnHsSigType (ForeignDeclCtx name) ty
       ; return (ForeignExport { fd_name = name', fd_sig_ty = ty'
                               , fd_co = noForeignExportCoercionYet
                               , fd_fe = spec }
                , fvs `addOneFV` unLoc name') }
        -- NB: a foreign export is an *occurrence site* for name, so
        --     we add it to the free-variable list.  It might, for example,
        --     be imported from another module

-- | For Windows DLLs we need to know what packages imported symbols are from
--      to generate correct calls. Imported symbols are tagged with the current
--      package, so if they get inlined across a package boundry we'll still
--      know where they're from.
--
patchForeignImport :: UnitId -> ForeignImport -> ForeignImport
patchForeignImport unitId (CImport cconv safety fs spec src)
        = CImport cconv safety fs (patchCImportSpec unitId spec) src

patchCImportSpec :: UnitId -> CImportSpec -> CImportSpec
patchCImportSpec unitId spec
 = case spec of
        CFunction callTarget    -> CFunction $ patchCCallTarget unitId callTarget
        _                       -> spec

patchCCallTarget :: UnitId -> CCallTarget -> CCallTarget
patchCCallTarget unitId callTarget =
  case callTarget of
  StaticTarget src label Nothing isFun
                              -> StaticTarget src label (Just unitId) isFun
  _                           -> callTarget

{-
*********************************************************
*                                                      *
\subsection{Instance declarations}
*                                                      *
*********************************************************
-}

rnSrcInstDecl :: InstDecl RdrName -> RnM (InstDecl Name, FreeVars)
rnSrcInstDecl (TyFamInstD { tfid_inst = tfi })
  = do { (tfi', fvs) <- rnTyFamInstDecl Nothing tfi
       ; return (TyFamInstD { tfid_inst = tfi' }, fvs) }

rnSrcInstDecl (DataFamInstD { dfid_inst = dfi })
  = do { (dfi', fvs) <- rnDataFamInstDecl Nothing dfi
       ; return (DataFamInstD { dfid_inst = dfi' }, fvs) }

rnSrcInstDecl (ClsInstD { cid_inst = cid })
  = do { (cid', fvs) <- rnClsInstDecl cid
       ; return (ClsInstD { cid_inst = cid' }, fvs) }

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
checkCanonicalInstances :: Name -> LHsSigType Name -> LHsBinds Name -> RnM ()
checkCanonicalInstances cls poly_ty mbinds = do
    whenWOptM Opt_WarnNonCanonicalMonadInstances
        checkCanonicalMonadInstances

    whenWOptM Opt_WarnNonCanonicalMonadFailInstances
        checkCanonicalMonadFailInstances

    whenWOptM Opt_WarnNonCanonicalMonoidInstances
        checkCanonicalMonoidInstances

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
    checkCanonicalMonadInstances
      | cls == applicativeClassName  = do
          forM_ (bagToList mbinds) $ \(L loc mbind) -> setSrcSpan loc $ do
              case mbind of
                  FunBind { fun_id = L _ name, fun_matches = mg }
                      | name == pureAName, isAliasMG mg == Just returnMName
                      -> addWarnNonCanonicalMethod1
                            Opt_WarnNonCanonicalMonadInstances "pure" "return"

                      | name == thenAName, isAliasMG mg == Just thenMName
                      -> addWarnNonCanonicalMethod1
                            Opt_WarnNonCanonicalMonadInstances "(*>)" "(>>)"

                  _ -> return ()

      | cls == monadClassName  = do
          forM_ (bagToList mbinds) $ \(L loc mbind) -> setSrcSpan loc $ do
              case mbind of
                  FunBind { fun_id = L _ name, fun_matches = mg }
                      | name == returnMName, isAliasMG mg /= Just pureAName
                      -> addWarnNonCanonicalMethod2
                            Opt_WarnNonCanonicalMonadInstances "return" "pure"

                      | name == thenMName, isAliasMG mg /= Just thenAName
                      -> addWarnNonCanonicalMethod2
                            Opt_WarnNonCanonicalMonadInstances "(>>)" "(*>)"

                  _ -> return ()

      | otherwise = return ()

    -- | Warn about unsound/non-canonical 'Monad'/'MonadFail' instance
    -- declarations. Specifically, the following conditions are verified:
    --
    -- In 'Monad' instances declarations:
    --
    --  * If 'fail' is overridden it must be canonical
    --    (i.e. @fail = Control.Monad.Fail.fail@)
    --
    -- In 'MonadFail' instance declarations:
    --
    --  * Warn if 'fail' is defined backwards
    --    (i.e. @fail = Control.Monad.fail@).
    --
    checkCanonicalMonadFailInstances
      | cls == monadFailClassName  = do
          forM_ (bagToList mbinds) $ \(L loc mbind) -> setSrcSpan loc $ do
              case mbind of
                  FunBind { fun_id = L _ name, fun_matches = mg }
                      | name == failMName, isAliasMG mg == Just failMName_preMFP
                      -> addWarnNonCanonicalMethod1
                            Opt_WarnNonCanonicalMonadFailInstances "fail"
                            "Control.Monad.fail"

                  _ -> return ()

      | cls == monadClassName  = do
          forM_ (bagToList mbinds) $ \(L loc mbind) -> setSrcSpan loc $ do
              case mbind of
                  FunBind { fun_id = L _ name, fun_matches = mg }
                      | name == failMName_preMFP, isAliasMG mg /= Just failMName
                      -> addWarnNonCanonicalMethod2
                            Opt_WarnNonCanonicalMonadFailInstances "fail"
                            "Control.Monad.Fail.fail"
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
    checkCanonicalMonoidInstances
      | cls == semigroupClassName  = do
          forM_ (bagToList mbinds) $ \(L loc mbind) -> setSrcSpan loc $ do
              case mbind of
                  FunBind { fun_id = L _ name, fun_matches = mg }
                      | name == sappendName, isAliasMG mg == Just mappendName
                      -> addWarnNonCanonicalMethod1
                            Opt_WarnNonCanonicalMonoidInstances "(<>)" "mappend"

                  _ -> return ()

      | cls == monoidClassName  = do
          forM_ (bagToList mbinds) $ \(L loc mbind) -> setSrcSpan loc $ do
              case mbind of
                  FunBind { fun_id = L _ name, fun_matches = mg }
                      | name == mappendName, isAliasMG mg /= Just sappendName
                      -> addWarnNonCanonicalMethod2NoDefault
                            Opt_WarnNonCanonicalMonoidInstances "mappend" "(<>)"

                  _ -> return ()

      | otherwise = return ()

    -- | test whether MatchGroup represents a trivial \"lhsName = rhsName\"
    -- binding, and return @Just rhsName@ if this is the case
    isAliasMG :: MatchGroup Name (LHsExpr Name) -> Maybe Name
    isAliasMG MG {mg_alts = L _ [L _ (Match { m_pats = [], m_grhss = grhss })]}
        | GRHSs [L _ (GRHS [] body)] lbinds <- grhss
        , L _ EmptyLocalBinds <- lbinds
        , L _ (HsVar (L _ rhsName)) <- body  = Just rhsName
    isAliasMG _ = Nothing

    -- got "lhs = rhs" but expected something different
    addWarnNonCanonicalMethod1 flag lhs rhs = do
        addWarn (Reason flag) $ vcat
                       [ text "Noncanonical" <+>
                         quotes (text (lhs ++ " = " ++ rhs)) <+>
                         text "definition detected"
                       , instDeclCtxt1 poly_ty
                       , text "Move definition from" <+>
                         quotes (text rhs) <+>
                         text "to" <+> quotes (text lhs)
                       ]

    -- expected "lhs = rhs" but got something else
    addWarnNonCanonicalMethod2 flag lhs rhs = do
        addWarn (Reason flag) $ vcat
                       [ text "Noncanonical" <+>
                         quotes (text lhs) <+>
                         text "definition detected"
                       , instDeclCtxt1 poly_ty
                       , text "Either remove definition for" <+>
                         quotes (text lhs) <+> text "or define as" <+>
                         quotes (text (lhs ++ " = " ++ rhs))
                       ]

    -- like above, but method has no default impl
    addWarnNonCanonicalMethod2NoDefault flag lhs rhs = do
        addWarn (Reason flag) $ vcat
                       [ text "Noncanonical" <+>
                         quotes (text lhs) <+>
                         text "definition detected"
                       , instDeclCtxt1 poly_ty
                       , text "Define as" <+>
                         quotes (text (lhs ++ " = " ++ rhs))
                       ]

    -- stolen from TcInstDcls
    instDeclCtxt1 :: LHsSigType Name -> SDoc
    instDeclCtxt1 hs_inst_ty
      = inst_decl_ctxt (ppr (getLHsInstDeclHead hs_inst_ty))

    inst_decl_ctxt :: SDoc -> SDoc
    inst_decl_ctxt doc = hang (text "in the instance declaration for")
                         2 (quotes doc <> text ".")


rnClsInstDecl :: ClsInstDecl RdrName -> RnM (ClsInstDecl Name, FreeVars)
rnClsInstDecl (ClsInstDecl { cid_poly_ty = inst_ty, cid_binds = mbinds
                           , cid_sigs = uprags, cid_tyfam_insts = ats
                           , cid_overlap_mode = oflag
                           , cid_datafam_insts = adts })
  = do { (inst_ty', inst_fvs) <- rnLHsInstType (text "an instance declaration") inst_ty
       ; let (ktv_names, _, head_ty') = splitLHsInstDeclTy inst_ty'
       ; let cls = case hsTyGetAppHead_maybe head_ty' of
                     Nothing -> mkUnboundName (mkTcOccFS (fsLit "<class>"))
                     Just (L _ cls, _) -> cls
                     -- rnLHsInstType has added an error message
                     -- if hsTyGetAppHead_maybe fails

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
       ; return (ClsInstDecl { cid_poly_ty = inst_ty', cid_binds = mbinds'
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

rnFamInstDecl :: HsDocContext
              -> Maybe (Name, [Name])   -- Nothing => not associated
                                        -- Just (cls,tvs) => associated,
                                        --   and gives class and tyvars of the
                                        --   parent instance delc
              -> Located RdrName
              -> HsTyPats RdrName
              -> rhs
              -> (HsDocContext -> rhs -> RnM (rhs', FreeVars))
              -> RnM (Located Name, HsTyPats Name, rhs', FreeVars)
rnFamInstDecl doc mb_cls tycon (HsIB { hsib_body = pats }) payload rnPayload
  = do { tycon'   <- lookupFamInstName (fmap fst mb_cls) tycon
       ; let loc = case pats of
                     []             -> pprPanic "rnFamInstDecl" (ppr tycon)
                     (L loc _ : []) -> loc
                     (L loc _ : ps) -> combineSrcSpans loc (getLoc (last ps))

       ; pat_kity_vars_with_dups <- extractHsTysRdrTyVarsDups pats
             -- Use the "...Dups" form because it's needed
             -- below to report unsed binder on the LHS
       ; var_names <- mapM (newTyVarNameRn mb_cls . L loc . unLoc) $
                      freeKiTyVarsAllVars $
                      rmDupsInRdrTyVars pat_kity_vars_with_dups

             -- All the free vars of the family patterns
             -- with a sensible binding location
       ; ((pats', payload'), fvs)
              <- bindLocalNamesFV var_names $
                 do { (pats', pat_fvs) <- rnLHsTypes (FamPatCtx tycon) pats
                    ; (payload', rhs_fvs) <- rnPayload doc payload

                       -- Report unused binders on the LHS
                       -- See Note [Unused type variables in family instances]
                    ; let groups :: [[Located RdrName]]
                          groups = equivClasses cmpLocated $
                                   freeKiTyVarsAllVars pat_kity_vars_with_dups
                    ; tv_nms_dups <- mapM (lookupOccRn . unLoc) $
                                     [ tv | (tv:_:_) <- groups ]
                          -- Add to the used variables
                          --  a) any variables that appear *more than once* on the LHS
                          --     e.g.   F a Int a = Bool
                          --  b) for associated instances, the variables
                          --     of the instance decl.  See
                          --     Note [Unused type variables in family instances]
                    ; let tv_nms_used = extendNameSetList rhs_fvs $
                                        inst_tvs ++ tv_nms_dups
                          inst_tvs = case mb_cls of
                                       Nothing            -> []
                                       Just (_, inst_tvs) -> inst_tvs
                    ; warnUnusedTypePatterns var_names tv_nms_used

                         -- See Note [Renaming associated types]
                    ; let bad_tvs = case mb_cls of
                                      Nothing           -> []
                                      Just (_,cls_tkvs) -> filter is_bad cls_tkvs
                          var_name_set = mkNameSet var_names

                          is_bad cls_tkv = cls_tkv `elemNameSet` rhs_fvs
                                        && not (cls_tkv `elemNameSet` var_name_set)
                    ; unless (null bad_tvs) (badAssocRhs bad_tvs)

                    ; return ((pats', payload'), rhs_fvs `plusFV` pat_fvs) }

       ; let anon_wcs = concatMap collectAnonWildCards pats'
             all_ibs  = anon_wcs ++ var_names
                        -- all_ibs: include anonymous wildcards in the implicit
                        -- binders In a type pattern they behave just like any
                        -- other type variable except for being anoymous.  See
                        -- Note [Wildcards in family instances]
             all_fvs  = fvs `addOneFV` unLoc tycon'

       ; return (tycon',
                 HsIB { hsib_body = pats'
                      , hsib_vars = all_ibs },
                 payload',
                 all_fvs) }
             -- type instance => use, hence addOneFV

rnTyFamInstDecl :: Maybe (Name, [Name])
                -> TyFamInstDecl RdrName
                -> RnM (TyFamInstDecl Name, FreeVars)
rnTyFamInstDecl mb_cls (TyFamInstDecl { tfid_eqn = L loc eqn })
  = do { (eqn', fvs) <- rnTyFamInstEqn mb_cls eqn
       ; return (TyFamInstDecl { tfid_eqn = L loc eqn'
                               , tfid_fvs = fvs }, fvs) }

rnTyFamInstEqn :: Maybe (Name, [Name])
               -> TyFamInstEqn RdrName
               -> RnM (TyFamInstEqn Name, FreeVars)
rnTyFamInstEqn mb_cls (TyFamEqn { tfe_tycon = tycon
                                , tfe_pats  = pats
                                , tfe_fixity = fixity
                                , tfe_rhs   = rhs })
  = do { (tycon', pats', rhs', fvs) <-
           rnFamInstDecl (TySynCtx tycon) mb_cls tycon pats rhs rnTySyn
       ; return (TyFamEqn { tfe_tycon = tycon'
                          , tfe_pats  = pats'
                          , tfe_fixity = fixity
                          , tfe_rhs   = rhs' }, fvs) }

rnTyFamDefltEqn :: Name
                -> TyFamDefltEqn RdrName
                -> RnM (TyFamDefltEqn Name, FreeVars)
rnTyFamDefltEqn cls (TyFamEqn { tfe_tycon = tycon
                              , tfe_pats  = tyvars
                              , tfe_fixity = fixity
                              , tfe_rhs   = rhs })
  = bindHsQTyVars ctx Nothing (Just cls) [] tyvars $ \ tyvars' _ ->
    do { tycon'      <- lookupFamInstName (Just cls) tycon
       ; (rhs', fvs) <- rnLHsType ctx rhs
       ; return (TyFamEqn { tfe_tycon = tycon'
                          , tfe_pats  = tyvars'
                          , tfe_fixity = fixity
                          , tfe_rhs   = rhs' }, fvs) }
  where
    ctx = TyFamilyCtx tycon

rnDataFamInstDecl :: Maybe (Name, [Name])
                  -> DataFamInstDecl RdrName
                  -> RnM (DataFamInstDecl Name, FreeVars)
rnDataFamInstDecl mb_cls (DataFamInstDecl { dfid_tycon = tycon
                                          , dfid_pats  = pats
                                          , dfid_fixity = fixity
                                          , dfid_defn  = defn })
  = do { (tycon', pats', (defn', _), fvs) <-
           rnFamInstDecl (TyDataCtx tycon) mb_cls tycon pats defn rnDataDefn
       ; return (DataFamInstDecl { dfid_tycon = tycon'
                                 , dfid_pats  = pats'
                                 , dfid_fixity = fixity
                                 , dfid_defn  = defn'
                                 , dfid_fvs   = fvs }, fvs) }

-- Renaming of the associated types in instances.

-- Rename associated type family decl in class
rnATDecls :: Name      -- Class
          -> [LFamilyDecl RdrName]
          -> RnM ([LFamilyDecl Name], FreeVars)
rnATDecls cls at_decls
  = rnList (rnFamDecl (Just cls)) at_decls

rnATInstDecls :: (Maybe (Name, [Name]) ->    -- The function that renames
                  decl RdrName ->            -- an instance. rnTyFamInstDecl
                  RnM (decl Name, FreeVars)) -- or rnDataFamInstDecl
              -> Name      -- Class
              -> [Name]
              -> [Located (decl RdrName)]
              -> RnM ([Located (decl Name)], FreeVars)
-- Used for data and type family defaults in a class decl
-- and the family instance declarations in an instance
--
-- NB: We allow duplicate associated-type decls;
--     See Note [Associated type instances] in TcInstDcls
rnATInstDecls rnFun cls tv_ns at_insts
  = rnList (rnFun (Just (cls, tv_ns))) at_insts
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

This is implemented as follows: during renaming anonymous wild cards
'_' are given freshly generated names. These names are collected after
renaming (rnFamInstDecl) and used to make new type variables during
type checking (tc_fam_ty_pats). One should not confuse these wild
cards with the ones from partial type signatures. The latter generate
fresh meta-variables whereas the former generate fresh skolems.

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

As usual, the warnings are not reported for for type variables with names
beginning with an underscore.

Extra-constraints wild cards are not supported in type/data family
instance declarations.

Relevant tickets: #3699, #10586, #10982 and #11451.

Note [Renaming associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check that the RHS of the decl mentions only type variables
bound on the LHS.  For example, this is not ok
   class C a b where
      type F a x :: *
   instance C (p,q) r where
      type F (p,q) x = (x, r)   -- BAD: mentions 'r'
c.f. Trac #5515

The same thing applies to kind variables, of course (Trac #7938, #9574):
   class Funct f where
      type Codomain f :: *
   instance Funct ('KProxy :: KProxy o) where
      type Codomain 'KProxy = NatTr (Proxy :: o -> *)
Here 'o' is mentioned on the RHS of the Codomain function, but
not on the LHS.

All this applies only for *instance* declarations.  In *class*
declarations there is no RHS to worry about, and the class variables
can all be in scope (Trac #5862):
    class Category (x :: k -> k -> *) where
      type Ob x :: k -> Constraint
      id :: Ob x a => x a a
      (.) :: (Ob x a, Ob x b, Ob x c) => x b c -> x a b -> x a c
Here 'k' is in scope in the kind signature, just like 'x'.
-}


{-
*********************************************************
*                                                      *
\subsection{Stand-alone deriving declarations}
*                                                      *
*********************************************************
-}

rnSrcDerivDecl :: DerivDecl RdrName -> RnM (DerivDecl Name, FreeVars)
rnSrcDerivDecl (DerivDecl ty deriv_strat overlap)
  = do { standalone_deriv_ok <- xoptM LangExt.StandaloneDeriving
       ; deriv_strats_ok     <- xoptM LangExt.DerivingStrategies
       ; unless standalone_deriv_ok (addErr standaloneDerivErr)
       ; failIfTc (isJust deriv_strat && not deriv_strats_ok) $
           illegalDerivStrategyErr $ fmap unLoc deriv_strat
       ; (ty', fvs) <- rnLHsInstType (text "In a deriving declaration") ty
       ; return (DerivDecl ty' deriv_strat overlap, fvs) }

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

rnHsRuleDecls :: RuleDecls RdrName -> RnM (RuleDecls Name, FreeVars)
rnHsRuleDecls (HsRules src rules)
  = do { (rn_rules,fvs) <- rnList rnHsRuleDecl rules
       ; return (HsRules src rn_rules,fvs) }

rnHsRuleDecl :: RuleDecl RdrName -> RnM (RuleDecl Name, FreeVars)
rnHsRuleDecl (HsRule rule_name act vars lhs _fv_lhs rhs _fv_rhs)
  = do { let rdr_names_w_loc = map get_var vars
       ; checkDupRdrNames rdr_names_w_loc
       ; checkShadowedRdrNames rdr_names_w_loc
       ; names <- newLocalBndrsRn rdr_names_w_loc
       ; bindHsRuleVars (snd $ unLoc rule_name) vars names $ \ vars' ->
    do { (lhs', fv_lhs') <- rnLExpr lhs
       ; (rhs', fv_rhs') <- rnLExpr rhs
       ; checkValidRule (snd $ unLoc rule_name) names lhs' fv_lhs'
       ; return (HsRule rule_name act vars' lhs' fv_lhs' rhs' fv_rhs',
                 fv_lhs' `plusFV` fv_rhs') } }
  where
    get_var (L _ (RuleBndrSig v _)) = v
    get_var (L _ (RuleBndr v)) = v

bindHsRuleVars :: RuleName -> [LRuleBndr RdrName] -> [Name]
               -> ([LRuleBndr Name] -> RnM (a, FreeVars))
               -> RnM (a, FreeVars)
bindHsRuleVars rule_name vars names thing_inside
  = go vars names $ \ vars' ->
    bindLocalNamesFV names (thing_inside vars')
  where
    doc = RuleCtx rule_name

    go (L l (RuleBndr (L loc _)) : vars) (n : ns) thing_inside
      = go vars ns $ \ vars' ->
        thing_inside (L l (RuleBndr (L loc n)) : vars')

    go (L l (RuleBndrSig (L loc _) bsig) : vars) (n : ns) thing_inside
      = rnHsSigWcTypeScoped doc bsig $ \ bsig' ->
        go vars ns $ \ vars' ->
        thing_inside (L l (RuleBndrSig (L loc n) bsig') : vars')

    go [] [] thing_inside = thing_inside []
    go vars names _ = pprPanic "bindRuleVars" (ppr vars $$ ppr names)

{-
Note [Rule LHS validity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check the shape of a transformation rule LHS.  Currently we only allow
LHSs of the form @(f e1 .. en)@, where @f@ is not one of the
@forall@'d variables.

We used restrict the form of the 'ei' to prevent you writing rules
with LHSs with a complicated desugaring (and hence unlikely to match);
(e.g. a case expression is not allowed: too elaborate.)

But there are legitimate non-trivial args ei, like sections and
lambdas.  So it seems simmpler not to check at all, and that is why
check_e is commented out.
-}

checkValidRule :: FastString -> [Name] -> LHsExpr Name -> NameSet -> RnM ()
checkValidRule rule_name ids lhs' fv_lhs'
  = do  {       -- Check for the form of the LHS
          case (validRuleLhs ids lhs') of
                Nothing  -> return ()
                Just bad -> failWithTc (badRuleLhsErr rule_name lhs' bad)

                -- Check that LHS vars are all bound
        ; let bad_vars = [var | var <- ids, not (var `elemNameSet` fv_lhs')]
        ; mapM_ (addErr . badRuleVar rule_name) bad_vars }

validRuleLhs :: [Name] -> LHsExpr Name -> Maybe (HsExpr Name)
-- Nothing => OK
-- Just e  => Not ok, and e is the offending sub-expression
validRuleLhs foralls lhs
  = checkl lhs
  where
    checkl (L _ e) = check e

    check (OpApp e1 op _ e2)              = checkl op `mplus` checkl_e e1 `mplus` checkl_e e2
    check (HsApp e1 e2)                   = checkl e1 `mplus` checkl_e e2
    check (HsAppType e _)                 = checkl e
    check (HsVar (L _ v)) | v `notElem` foralls = Nothing
    check other                           = Just other  -- Failure

        -- Check an argument
    checkl_e (L _ _e) = Nothing         -- Was (check_e e); see Note [Rule LHS validity checking]

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

badRuleLhsErr :: FastString -> LHsExpr Name -> HsExpr Name -> SDoc
badRuleLhsErr name lhs bad_e
  = sep [text "Rule" <+> pprRuleName name <> colon,
         nest 4 (vcat [err,
                       text "in left-hand side:" <+> ppr lhs])]
    $$
    text "LHS must be of form (f e1 .. en) where f is not forall'd"
  where
    err = case bad_e of
            HsUnboundVar uv -> text "Not in scope:" <+> ppr uv
            _ -> text "Illegal expression:" <+> ppr bad_e

{-
*********************************************************
*                                                      *
\subsection{Vectorisation declarations}
*                                                      *
*********************************************************
-}

rnHsVectDecl :: VectDecl RdrName -> RnM (VectDecl Name, FreeVars)
-- FIXME: For the moment, the right-hand side is restricted to be a variable as we cannot properly
--        typecheck a complex right-hand side without invoking 'vectType' from the vectoriser.
rnHsVectDecl (HsVect s var rhs@(L _ (HsVar _)))
  = do { var' <- lookupLocatedOccRn var
       ; (rhs', fv_rhs) <- rnLExpr rhs
       ; return (HsVect s var' rhs', fv_rhs `addOneFV` unLoc var')
       }
rnHsVectDecl (HsVect _ _var _rhs)
  = failWith $ vcat
               [ text "IMPLEMENTATION RESTRICTION: right-hand side of a VECTORISE pragma"
               , text "must be an identifier"
               ]
rnHsVectDecl (HsNoVect s var)
  = do { var' <- lookupLocatedTopBndrRn var           -- only applies to local (not imported) names
       ; return (HsNoVect s var', unitFV (unLoc var'))
       }
rnHsVectDecl (HsVectTypeIn s isScalar tycon Nothing)
  = do { tycon' <- lookupLocatedOccRn tycon
       ; return (HsVectTypeIn s isScalar tycon' Nothing, unitFV (unLoc tycon'))
       }
rnHsVectDecl (HsVectTypeIn s isScalar tycon (Just rhs_tycon))
  = do { tycon'     <- lookupLocatedOccRn tycon
       ; rhs_tycon' <- lookupLocatedOccRn rhs_tycon
       ; return ( HsVectTypeIn s isScalar tycon' (Just rhs_tycon')
                , mkFVs [unLoc tycon', unLoc rhs_tycon'])
       }
rnHsVectDecl (HsVectTypeOut _ _ _)
  = panic "RnSource.rnHsVectDecl: Unexpected 'HsVectTypeOut'"
rnHsVectDecl (HsVectClassIn s cls)
  = do { cls' <- lookupLocatedOccRn cls
       ; return (HsVectClassIn s cls', unitFV (unLoc cls'))
       }
rnHsVectDecl (HsVectClassOut _)
  = panic "RnSource.rnHsVectDecl: Unexpected 'HsVectClassOut'"
rnHsVectDecl (HsVectInstIn instTy)
  = do { (instTy', fvs) <- rnLHsInstType (text "a VECTORISE pragma") instTy
       ; return (HsVectInstIn instTy', fvs)
       }
rnHsVectDecl (HsVectInstOut _)
  = panic "RnSource.rnHsVectDecl: Unexpected 'HsVectInstOut'"

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
  is jolly confusing.  See Trac #4875


* Increase kind polymorphism.  See TcTyClsDecls
  Note [Grouping of type and class declarations]

Why do the instance declarations participate?  At least two reasons

* Consider (Trac #11348)

     type family F a
     type instance F Int = Bool

     data R = MkR (F Int)

     type Foo = 'MkR 'True

  For Foo to kind-check we need to know that (F Int) ~ Bool.  But we won't
  know that unless we've looked at the type instance declaration for F
  before kind-checking Foo.

* Another example is this (Trac #3990).

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

  To accomodate for these situations, we ensure that an instance is checked
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
constructors] in TcEnv
-}


rnTyClDecls :: [TyClGroup RdrName]
            -> RnM ([TyClGroup Name], FreeVars)
-- Rename the declarations and do dependency analysis on them
rnTyClDecls tycl_ds
  = do { -- Rename the type/class, instance, and role declaraations
         tycls_w_fvs <- mapM (wrapLocFstM rnTyClDecl)
                             (tyClGroupTyClDecls tycl_ds)
       ; let tc_names = mkNameSet (map (tcdName . unLoc . fst) tycls_w_fvs)

       ; instds_w_fvs <- mapM (wrapLocFstM rnSrcInstDecl) (tyClGroupInstDecls tycl_ds)
       ; role_annots  <- rnRoleAnnots tc_names (tyClGroupRoleDecls tycl_ds)

       ; tycls_w_fvs <- addBootDeps tycls_w_fvs
                      -- TBD must add_boot_deps to instds_w_fvs?

       -- Do SCC analysis on the type/class decls
       ; rdr_env <- getGlobalRdrEnv
       ; let tycl_sccs = depAnalTyClDecls rdr_env tycls_w_fvs
             role_annot_env = mkRoleAnnotEnv role_annots

             inst_ds_map = mkInstDeclFreeVarsMap rdr_env tc_names instds_w_fvs
             (init_inst_ds, rest_inst_ds) = getInsts [] inst_ds_map

             first_group
               | null init_inst_ds = []
               | otherwise = [TyClGroup { group_tyclds = []
                                        , group_roles  = []
                                        , group_instds = init_inst_ds }]

             ((final_inst_ds, orphan_roles), groups)
                = mapAccumL mk_group (rest_inst_ds, role_annot_env) tycl_sccs


             all_fvs = plusFV (foldr (plusFV . snd) emptyFVs tycls_w_fvs)
                              (foldr (plusFV . snd) emptyFVs instds_w_fvs)

             all_groups = first_group ++ groups

       ; ASSERT2( null final_inst_ds,  ppr instds_w_fvs $$ ppr inst_ds_map
                                       $$ ppr (flattenSCCs tycl_sccs) $$ ppr final_inst_ds  )
         mapM_ orphanRoleAnnotErr (nameEnvElts orphan_roles)

       ; traceRn "rnTycl dependency analysis made groups" (ppr all_groups)
       ; return (all_groups, all_fvs) }
  where
    mk_group :: (InstDeclFreeVarsMap, RoleAnnotEnv)
             -> SCC (LTyClDecl Name)
             -> ( (InstDeclFreeVarsMap, RoleAnnotEnv)
                , TyClGroup Name )
    mk_group (inst_map, role_env) scc
      = ((inst_map', role_env'), group)
      where
        tycl_ds              = flattenSCC scc
        bndrs                = map (tcdName . unLoc) tycl_ds
        (inst_ds, inst_map') = getInsts      bndrs inst_map
        (roles,   role_env') = getRoleAnnots bndrs role_env
        group = TyClGroup { group_tyclds = tycl_ds
                          , group_roles  = roles
                          , group_instds = inst_ds }


depAnalTyClDecls :: GlobalRdrEnv
                 -> [(LTyClDecl Name, FreeVars)]
                 -> [SCC (LTyClDecl Name)]
-- See Note [Dependency analysis of type, class, and instance decls]
depAnalTyClDecls rdr_env ds_w_fvs
  = stronglyConnCompFromEdgedVerticesUniq edges
  where
    edges = [ (d, tcdName (unLoc d), map (getParent rdr_env) (nonDetEltsUFM fvs))
            | (d, fvs) <- ds_w_fvs ]
            -- It's OK to use nonDetEltsUFM here as
            -- stronglyConnCompFromEdgedVertices is still deterministic
            -- even if the edges are in nondeterministic order as explained
            -- in Note [Deterministic SCC] in Digraph.

toParents :: GlobalRdrEnv -> NameSet -> NameSet
toParents rdr_env ns
  = nonDetFoldUFM add emptyNameSet ns
  -- It's OK to use nonDetFoldUFM because we immediately forget the
  -- ordering by creating a set
  where
    add n s = extendNameSet s (getParent rdr_env n)

getParent :: GlobalRdrEnv -> Name -> Name
getParent rdr_env n
  = case lookupGRE_Name rdr_env n of
      Just gre -> case gre_par gre of
                    ParentIs  { par_is = p } -> p
                    FldParent { par_is = p } -> p
                    _                        -> n
      Nothing -> n


{- Note [Extra dependencies from .hs-boot files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a long story, so buckle in.

**Dependencies via hs-boot files are not obvious.** Consider the following case:

A.hs-boot
  module A where
    data A1

B.hs
  module B where
    import {-# SOURCE #-} A
    type B1 = A1

A.hs
  module A where
    import B
    data A2 = MkA2 B1
    data A1 = MkA1 A2

Here A2 is really recursive (via B1), but we won't see that easily when
doing dependency analysis when compiling A.hs.  When we look at A2,
we see that its free variables are simply B1, but without (recursively) digging
into the definition of B1 will we see that it actually refers to A1 via an
hs-boot file.

**Recursive declarations, even those broken by an hs-boot file, need to
be type-checked together.**  Whenever we refer to a declaration via
an hs-boot file, we must be careful not to force the TyThing too early:
ala Note [Tying the knot] if we force the TyThing before we have
defined it ourselves in the local type environment, GHC will error.

Conservatively, then, it would make sense that we to typecheck A1
and A2 from the previous example together, because the two types are
truly mutually recursive through B1.

If we are being clever, we might observe that while kind-checking
A2, we don't actually need to force the TyThing for A1: B1
independently records its kind, so there is no need to go "deeper".
But then we are in an uncomfortable situation where we have
constructed a TyThing for A2 before we have checked A1, and we
have to be absolutely certain we don't force it too deeply until
we get around to kind checking A1, which could be for a very long
time.

Indeed, with datatype promotion, we may very well need to look
at the type of MkA2 before we have kind-checked A1: consider,

    data T = MkT (Proxy 'MkA2)

To promote MkA2, we need to lift its type to the kind level.
We never tested this, but it seems likely A1 would get poked
at this point.

**Here's what we do instead.**  So it is expedient for us to
make sure A1 and A2 are kind checked together in a loop.
To ensure that our dependency analysis can catch this,
we add a dependency:

  - from every local declaration
  - to everything that comes from this module's .hs-boot file
    (this is gotten from sb_tcs in the SelfBootInfo).

In this case, we'll add an edges

  - from A1 to A2 (but that edge is there already)
  - from A2 to A1 (which is new)

Well, not quite *every* declaration. Imagine module A
above had another datatype declaration:

  data A3 = A3 Int

Even though A3 has a dependency (on Int), all its dependencies are from things
that live on other packages. Since we don't have mutual dependencies across
packages, it is safe not to add the dependencies on the .hs-boot stuff to A2.

Hence function nameIsHomePackageImport.

Note that this is fairly conservative: it essentially implies that
EVERY type declaration in this modules hs-boot file will be kind-checked
together in one giant loop (and furthermore makes every other type
in the module depend on this loop).  This is perhaps less than ideal, because
the larger a recursive group, the less polymorphism available (we
cannot infer a type to be polymorphically instantiated while we
are inferring its kind), but no one has hollered about this (yet!)
-}

addBootDeps :: [(LTyClDecl Name, FreeVars)] -> RnM [(LTyClDecl Name, FreeVars)]
-- See Note [Extra dependencies from .hs-boot files]
addBootDeps ds_w_fvs
  = do { tcg_env <- getGblEnv
       ; let this_mod  = tcg_mod tcg_env
             boot_info = tcg_self_boot tcg_env

             add_boot_deps :: [(LTyClDecl Name, FreeVars)] -> [(LTyClDecl Name, FreeVars)]
             add_boot_deps ds_w_fvs
               = case boot_info of
                     SelfBoot { sb_tcs = tcs } | not (isEmptyNameSet tcs)
                        -> map (add_one tcs) ds_w_fvs
                     _  -> ds_w_fvs

             add_one :: NameSet -> (LTyClDecl Name, FreeVars) -> (LTyClDecl Name, FreeVars)
             add_one tcs pr@(decl,fvs)
                | has_local_imports fvs = (decl, fvs `plusFV` tcs)
                | otherwise             = pr

             has_local_imports fvs
                 = nameSetAny (nameIsHomePackageImport this_mod) fvs
       ; return (add_boot_deps ds_w_fvs) }



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
             -> [LRoleAnnotDecl RdrName]
             -> RnM [LRoleAnnotDecl Name]
rnRoleAnnots tc_names role_annots
  = do {  -- Check for duplicates *before* renaming, to avoid
          -- lumping together all the unboundNames
         let (no_dups, dup_annots) = removeDups role_annots_cmp role_annots
             role_annots_cmp (L _ annot1) (L _ annot2)
               = roleAnnotDeclName annot1 `compare` roleAnnotDeclName annot2
       ; mapM_ dupRoleAnnotErr dup_annots
       ; mapM (wrapLocM rn_role_annot1) no_dups }
  where
    rn_role_annot1 (RoleAnnotDecl tycon roles)
      = do {  -- the name is an *occurrence*, but look it up only in the
              -- decls defined in this group (see #10263)
             tycon' <- lookupSigCtxtOccRn (RoleAnnotCtxt tc_names)
                                          (text "role annotation")
                                          tycon
           ; return $ RoleAnnotDecl tycon' roles }

dupRoleAnnotErr :: [LRoleAnnotDecl RdrName] -> RnM ()
dupRoleAnnotErr [] = panic "dupRoleAnnotErr"
dupRoleAnnotErr list
  = addErrAt loc $
    hang (text "Duplicate role annotations for" <+>
          quotes (ppr $ roleAnnotDeclName first_decl) <> colon)
       2 (vcat $ map pp_role_annot sorted_list)
    where
      sorted_list = sortBy cmp_annot list
      (L loc first_decl : _) = sorted_list

      pp_role_annot (L loc decl) = hang (ppr decl)
                                      4 (text "-- written at" <+> ppr loc)

      cmp_annot (L loc1 _) (L loc2 _) = loc1 `compare` loc2

orphanRoleAnnotErr :: LRoleAnnotDecl Name -> RnM ()
orphanRoleAnnotErr (L loc decl)
  = addErrAt loc $
    hang (text "Role annotation for a type previously declared:")
       2 (ppr decl) $$
    parens (text "The role annotation must be given where" <+>
            quotes (ppr $ roleAnnotDeclName decl) <+>
            text "is declared.")


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
names. We exclude the annotations for unbound names in the annotation
environment to avoid spurious errors for orphaned annotations.

We then (in rnTyClDecls) do a check for orphan role annotations (role
annotations without an accompanying type decl). The check works by folding
over components (of type [[Either (TyClDecl Name) (InstDecl Name)]]), selecting
out the relevant role declarations for each group, as well as diminishing the
annotation environment. After the fold is complete, anything left over in the
name environment must be an orphan, and errors are generated.

An earlier version of this algorithm short-cut the orphan check by renaming
only with names declared in this module. But, this check is insufficient in
the case of staged module compilation (Template Haskell, GHCi).
See #8485. With the new lookup process (which includes types declared in other
modules), we get better error messages, too.
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
type InstDeclFreeVarsMap = [(LInstDecl Name, FreeVars)]

-- | Construct an @InstDeclFreeVarsMap@ by eliminating any @Name@s from the
--   @FreeVars@ which are *not* the binders of a @TyClDecl@.
mkInstDeclFreeVarsMap :: GlobalRdrEnv
                      -> NameSet
                      -> [(LInstDecl Name, FreeVars)]
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
getInsts :: [Name] -> InstDeclFreeVarsMap -> ([LInstDecl Name], InstDeclFreeVarsMap)
getInsts bndrs inst_decl_map
  = partitionWith pick_me inst_decl_map
  where
    pick_me :: (LInstDecl Name, FreeVars)
            -> Either (LInstDecl Name) (LInstDecl Name, FreeVars)
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

rnTyClDecl :: TyClDecl RdrName
           -> RnM (TyClDecl Name, FreeVars)

-- All flavours of type family declarations ("type family", "newtype family",
-- and "data family"), both top level and (for an associated type)
-- in a class decl
rnTyClDecl (FamDecl { tcdFam = decl })
  = do { (decl', fvs) <- rnFamDecl Nothing decl
       ; return (FamDecl decl', fvs) }

rnTyClDecl (SynDecl { tcdLName = tycon, tcdTyVars = tyvars,
                      tcdFixity = fixity, tcdRhs = rhs })
  = do { tycon' <- lookupLocatedTopBndrRn tycon
       ; kvs <- freeKiTyVarsKindVars <$> extractHsTyRdrTyVars rhs
       ; let doc = TySynCtx tycon
       ; traceRn "rntycl-ty" (ppr tycon <+> ppr kvs)
       ; ((tyvars', rhs'), fvs) <- bindHsQTyVars doc Nothing Nothing kvs tyvars $
                                    \ tyvars' _ ->
                                    do { (rhs', fvs) <- rnTySyn doc rhs
                                       ; return ((tyvars', rhs'), fvs) }
       ; return (SynDecl { tcdLName = tycon', tcdTyVars = tyvars'
                         , tcdFixity = fixity
                         , tcdRhs = rhs', tcdFVs = fvs }, fvs) }

-- "data", "newtype" declarations
-- both top level and (for an associated type) in an instance decl
rnTyClDecl (DataDecl { tcdLName = tycon, tcdTyVars = tyvars,
                       tcdFixity = fixity, tcdDataDefn = defn })
  = do { tycon' <- lookupLocatedTopBndrRn tycon
       ; kvs <- extractDataDefnKindVars defn
       ; let doc = TyDataCtx tycon
       ; traceRn "rntycl-data" (ppr tycon <+> ppr kvs)
       ; ((tyvars', defn', no_kvs), fvs)
           <- bindHsQTyVars doc Nothing Nothing kvs tyvars $ \ tyvars' dep_vars ->
              do { ((defn', kind_sig_fvs), fvs) <- rnDataDefn doc defn
                 ; let sig_tvs         = filterNameSet isTyVarName kind_sig_fvs
                       unbound_sig_tvs = sig_tvs `minusNameSet` dep_vars
                 ; return ((tyvars', defn', isEmptyNameSet unbound_sig_tvs), fvs) }
          -- See Note [Complete user-supplied kind signatures] in HsDecls
       ; typeintype <- xoptM LangExt.TypeInType
       ; let cusk = hsTvbAllKinded tyvars' &&
                    (not typeintype || no_kvs)
       ; return (DataDecl { tcdLName = tycon', tcdTyVars = tyvars'
                          , tcdFixity = fixity
                          , tcdDataDefn = defn', tcdDataCusk = cusk
                          , tcdFVs = fvs }, fvs) }

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
            <- bindHsQTyVars cls_doc Nothing Nothing kvs tyvars $ \ tyvars' _ -> do
                  -- Checks for distinct tyvars
             { (context', cxt_fvs) <- rnContext cls_doc context
             ; fds'  <- rnFds fds
                         -- The fundeps have no free variables
             ; (ats', fv_ats) <- rnATDecls cls' ats
             ; let fvs = cxt_fvs     `plusFV`
                         fv_ats
             ; return ((tyvars', context', fds', ats'), fvs) }

        ; (at_defs', fv_at_defs) <- rnList (rnTyFamDefltEqn cls') at_defs

        -- No need to check for duplicate associated type decls
        -- since that is done by RnNames.extendGlobalRdrEnvRn

        -- Check the signatures
        -- First process the class op sigs (op_sigs), then the fixity sigs (non_op_sigs).
        ; let sig_rdr_names_w_locs = [op | L _ (ClassOpSig False ops _) <- sigs
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
                -- since that is done by RnNames.extendGlobalRdrEnvRn
                -- and the methods are already in scope

  -- Haddock docs
        ; docs' <- mapM (wrapLocM rnDocDecl) docs

        ; let all_fvs = meth_fvs `plusFV` stuff_fvs `plusFV` fv_at_defs
        ; return (ClassDecl { tcdCtxt = context', tcdLName = lcls',
                              tcdTyVars = tyvars', tcdFixity = fixity,
                              tcdFDs = fds', tcdSigs = sigs',
                              tcdMeths = mbinds', tcdATs = ats', tcdATDefs = at_defs',
                              tcdDocs = docs', tcdFVs = all_fvs },
                  all_fvs ) }
  where
    cls_doc  = ClassDeclCtx lcls

-- "type" and "type instance" declarations
rnTySyn :: HsDocContext -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnTySyn doc rhs = rnLHsType doc rhs

rnDataDefn :: HsDocContext -> HsDataDefn RdrName
           -> RnM ((HsDataDefn Name, NameSet), FreeVars)
                -- the NameSet includes all Names free in the kind signature
                -- See Note [Complete user-supplied kind signatures]
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
           -- since that is done by RnNames.extendGlobalRdrEnvRn

        ; let all_fvs = fvs1 `plusFV` fvs3 `plusFV`
                        con_fvs `plusFV` sig_fvs
        ; return (( HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                               , dd_ctxt = context', dd_kindSig = m_sig'
                               , dd_cons = condecls'
                               , dd_derivs = derivs' }
                  , sig_fvs )
                 , all_fvs )
        }
  where
    h98_style = case condecls of  -- Note [Stupid theta]
                     L _ (ConDeclGADT {}) : _  -> False
                     _                         -> True

    rn_derivs (L loc ds)
      = do { deriv_strats_ok <- xoptM LangExt.DerivingStrategies
           ; failIfTc (lengthExceeds ds 1 && not deriv_strats_ok)
               multipleDerivClausesErr
           ; (ds', fvs) <- mapFvRn (rnLHsDerivingClause deriv_strats_ok doc) ds
           ; return (L loc ds', fvs) }

rnLHsDerivingClause :: Bool -> HsDocContext -> LHsDerivingClause RdrName
                    -> RnM (LHsDerivingClause Name, FreeVars)
rnLHsDerivingClause deriv_strats_ok doc
                (L loc (HsDerivingClause { deriv_clause_strategy = dcs
                                         , deriv_clause_tys = L loc' dct }))
  = do { failIfTc (isJust dcs && not deriv_strats_ok) $
           illegalDerivStrategyErr $ fmap unLoc dcs
       ; (dct', fvs) <- mapFvRn (rnHsSigType doc) dct
       ; return ( L loc (HsDerivingClause { deriv_clause_strategy = dcs
                                          , deriv_clause_tys = L loc' dct' })
                , fvs ) }

badGadtStupidTheta :: HsDocContext -> SDoc
badGadtStupidTheta _
  = vcat [text "No context is allowed on a GADT-style data declaration",
          text "(You can put a context on each constructor, though.)"]

illegalDerivStrategyErr :: Maybe DerivStrategy -> SDoc
illegalDerivStrategyErr ds
  = vcat [ text "Illegal deriving strategy" <> colon <+> maybe empty ppr ds
         , text "Use DerivingStrategies to enable this extension" ]

multipleDerivClausesErr :: SDoc
multipleDerivClausesErr
  = vcat [ text "Illegal use of multiple, consecutive deriving clauses"
         , text "Use DerivingStrategies to allow this" ]

rnFamDecl :: Maybe Name -- Just cls => this FamilyDecl is nested
                        --             inside an *class decl* for cls
                        --             used for associated types
          -> FamilyDecl RdrName
          -> RnM (FamilyDecl Name, FreeVars)
rnFamDecl mb_cls (FamilyDecl { fdLName = tycon, fdTyVars = tyvars
                             , fdFixity = fixity
                             , fdInfo = info, fdResultSig = res_sig
                             , fdInjectivityAnn = injectivity })
  = do { tycon' <- lookupLocatedTopBndrRn tycon
       ; kvs <- extractRdrKindSigVars res_sig
       ; ((tyvars', res_sig', injectivity'), fv1) <-
            bindHsQTyVars doc Nothing mb_cls kvs tyvars $
            \ tyvars'@(HsQTvs { hsq_implicit = rn_kvs }) _ ->
            do { let rn_sig = rnFamResultSig doc rn_kvs
               ; (res_sig', fv_kind) <- wrapLocFstM rn_sig res_sig
               ; injectivity' <- traverse (rnInjectivityAnn tyvars' res_sig')
                                          injectivity
               ; return ( (tyvars', res_sig', injectivity') , fv_kind ) }
       ; (info', fv2) <- rn_info info
       ; return (FamilyDecl { fdLName = tycon', fdTyVars = tyvars'
                            , fdFixity = fixity
                            , fdInfo = info', fdResultSig = res_sig'
                            , fdInjectivityAnn = injectivity' }
                , fv1 `plusFV` fv2) }
  where
     doc = TyFamilyCtx tycon

     ----------------------
     rn_info (ClosedTypeFamily (Just eqns))
       = do { (eqns', fvs) <- rnList (rnTyFamInstEqn Nothing) eqns
                                                    -- no class context,
            ; return (ClosedTypeFamily (Just eqns'), fvs) }
     rn_info (ClosedTypeFamily Nothing)
       = return (ClosedTypeFamily Nothing, emptyFVs)
     rn_info OpenTypeFamily = return (OpenTypeFamily, emptyFVs)
     rn_info DataFamily     = return (DataFamily, emptyFVs)

rnFamResultSig :: HsDocContext
               -> [Name]   -- kind variables already in scope
               -> FamilyResultSig RdrName
               -> RnM (FamilyResultSig Name, FreeVars)
rnFamResultSig _ _ NoSig
   = return (NoSig, emptyFVs)
rnFamResultSig doc _ (KindSig kind)
   = do { (rndKind, ftvs) <- rnLHsKind doc kind
        ;  return (KindSig rndKind, ftvs) }
rnFamResultSig doc kv_names (TyVarSig tvbndr)
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

       ; bindLHsTyVarBndr doc Nothing -- this might be a lie, but it's used for
                                      -- scoping checks that are irrelevant here
                          (mkNameSet kv_names) emptyNameSet
                                       -- use of emptyNameSet here avoids
                                       -- redundant duplicate errors
                          tvbndr $ \ _ _ tvbndr' ->
         return (TyVarSig tvbndr', unitFV (hsLTyVarName tvbndr')) }

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
rnInjectivityAnn :: LHsQTyVars Name            -- ^ Type variables declared in
                                               --   type family head
                 -> LFamilyResultSig Name      -- ^ Result signature
                 -> LInjectivityAnn RdrName    -- ^ Injectivity annotation
                 -> RnM (LInjectivityAnn Name)
rnInjectivityAnn tvBndrs (L _ (TyVarSig resTv))
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
Trac #3850 complains about a regression wrt 6.10 for
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

---------------
badAssocRhs :: [Name] -> RnM ()
badAssocRhs ns
  = addErr (hang (text "The RHS of an associated type declaration mentions"
                  <+> pprWithCommas (quotes . ppr) ns)
               2 (text "All such variables must be bound on the LHS"))

-----------------
rnConDecls :: [LConDecl RdrName] -> RnM ([LConDecl Name], FreeVars)
rnConDecls = mapFvRn (wrapLocFstM rnConDecl)

rnConDecl :: ConDecl RdrName -> RnM (ConDecl Name, FreeVars)
rnConDecl decl@(ConDeclH98 { con_name = name, con_qvars = qtvs
                           , con_cxt = mcxt, con_details = details
                           , con_doc = mb_doc })
  = do  { _ <- addLocM checkConName name
        ; new_name     <- lookupLocatedTopBndrRn name
        ; let doc = ConDeclCtx [new_name]
        ; mb_doc'      <- rnMbLHsDoc mb_doc
        ; (kvs, qtvs') <- get_con_qtvs (hsConDeclArgTys details)

        ; bindHsQTyVars doc (Just $ inHsDocContext doc) Nothing kvs qtvs' $
          \new_tyvars _ -> do
        { (new_context, fvs1) <- case mcxt of
                             Nothing   -> return (Nothing,emptyFVs)
                             Just lcxt -> do { (lctx',fvs) <- rnContext doc lcxt
                                             ; return (Just lctx',fvs) }
        ; (new_details, fvs2) <- rnConDeclDetails (unLoc new_name) doc details
        ; let (new_details',fvs3) = (new_details,emptyFVs)
        ; traceRn "rnConDecl" (ppr name <+> vcat
             [ text "free_kvs:" <+> ppr kvs
             , text "qtvs:" <+> ppr qtvs
             , text "qtvs':" <+> ppr qtvs' ])
        ; let all_fvs = fvs1 `plusFV` fvs2 `plusFV` fvs3
              new_tyvars' = case qtvs of
                Nothing -> Nothing
                Just _ -> Just new_tyvars
        ; return (decl { con_name = new_name, con_qvars = new_tyvars'
                       , con_cxt = new_context, con_details = new_details'
                       , con_doc = mb_doc' },
                  all_fvs) }}
 where
    cxt = maybe [] unLoc mcxt
    get_rdr_tvs tys = extractHsTysRdrTyVars (cxt ++ tys)

    get_con_qtvs :: [LHsType RdrName]
                 -> RnM ([Located RdrName], LHsQTyVars RdrName)
    get_con_qtvs arg_tys
      | Just tvs <- qtvs   -- data T = forall a. MkT (a -> a)
      = do { free_vars <- get_rdr_tvs arg_tys
           ; return (freeKiTyVarsKindVars free_vars, tvs) }
      | otherwise  -- data T = MkT (a -> a)
      = return ([], mkHsQTvs [])

rnConDecl decl@(ConDeclGADT { con_names = names, con_type = ty
                            , con_doc = mb_doc })
  = do  { mapM_ (addLocM checkConName) names
        ; new_names    <- mapM lookupLocatedTopBndrRn names
        ; let doc = ConDeclCtx new_names
        ; mb_doc'      <- rnMbLHsDoc mb_doc

        ; (ty', fvs) <- rnHsSigType doc ty
        ; traceRn "rnConDecl" (ppr names <+> vcat
             [ text "fvs:" <+> ppr fvs ])
        ; return (decl { con_names = new_names, con_type = ty'
                       , con_doc = mb_doc' },
                  fvs) }

rnConDeclDetails
   :: Name
   -> HsDocContext
   -> HsConDetails (LHsType RdrName) (Located [LConDeclField RdrName])
   -> RnM (HsConDetails (LHsType Name) (Located [LConDeclField Name]), FreeVars)
rnConDeclDetails _ doc (PrefixCon tys)
  = do { (new_tys, fvs) <- rnLHsTypes doc tys
       ; return (PrefixCon new_tys, fvs) }

rnConDeclDetails _ doc (InfixCon ty1 ty2)
  = do { (new_ty1, fvs1) <- rnLHsType doc ty1
       ; (new_ty2, fvs2) <- rnLHsType doc ty2
       ; return (InfixCon new_ty1 new_ty2, fvs1 `plusFV` fvs2) }

rnConDeclDetails con doc (RecCon (L l fields))
  = do  { fls <- lookupConstructorFields con
        ; (new_fields, fvs) <- rnConDeclFields doc fls fields
                -- No need to check for duplicate fields
                -- since that is done by RnNames.extendGlobalRdrEnvRn
        ; return (RecCon (L l new_fields), fvs) }

-------------------------------------------------

-- | Brings pattern synonym names and also pattern synonym selectors
-- from record pattern synonyms into scope.
extendPatSynEnv :: HsValBinds RdrName -> MiniFixityEnv
                -> ([Name] -> TcRnIf TcGblEnv TcLclEnv a) -> TcM a
extendPatSynEnv val_decls local_fix_env thing = do {
     names_with_fls <- new_ps val_decls
   ; let pat_syn_bndrs = concat [ name: map flSelector fields
                                | (name, fields) <- names_with_fls ]
   ; let avails = map avail pat_syn_bndrs
   ; (gbl_env, lcl_env) <- extendGlobalRdrEnvRn avails local_fix_env

   ; let field_env' = extendNameEnvList (tcg_field_env gbl_env) names_with_fls
         final_gbl_env = gbl_env { tcg_field_env = field_env' }
   ; setEnvs (final_gbl_env, lcl_env) (thing pat_syn_bndrs) }
  where
    new_ps :: HsValBinds RdrName -> TcM [(Name, [FieldLabel])]
    new_ps (ValBindsIn binds _) = foldrBagM new_ps' [] binds
    new_ps _ = panic "new_ps"

    new_ps' :: LHsBindLR RdrName RdrName
            -> [(Name, [FieldLabel])]
            -> TcM [(Name, [FieldLabel])]
    new_ps' bind names
      | L bind_loc (PatSynBind (PSB { psb_id = L _ n
                                    , psb_args = RecordPatSyn as })) <- bind
      = do
          bnd_name <- newTopSrcBinder (L bind_loc n)
          let rnames = map recordPatSynSelectorId as
              mkFieldOcc :: Located RdrName -> LFieldOcc RdrName
              mkFieldOcc (L l name) = L l (FieldOcc (L l name) PlaceHolder)
              field_occs =  map mkFieldOcc rnames
          flds     <- mapM (newRecordSelector False [bnd_name]) field_occs
          return ((bnd_name, flds): names)
      | L bind_loc (PatSynBind (PSB { psb_id = L _ n})) <- bind
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

rnFds :: [Located (FunDep (Located RdrName))]
  -> RnM [Located (FunDep (Located Name))]
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

findSplice :: [LHsDecl RdrName] -> RnM (HsGroup RdrName, Maybe (SpliceDecl RdrName, [LHsDecl RdrName]))
findSplice ds = addl emptyRdrGroup ds

addl :: HsGroup RdrName -> [LHsDecl RdrName]
     -> RnM (HsGroup RdrName, Maybe (SpliceDecl RdrName, [LHsDecl RdrName]))
-- This stuff reverses the declarations (again) but it doesn't matter
addl gp []           = return (gp, Nothing)
addl gp (L l d : ds) = add gp l d ds


add :: HsGroup RdrName -> SrcSpan -> HsDecl RdrName -> [LHsDecl RdrName]
    -> RnM (HsGroup RdrName, Maybe (SpliceDecl RdrName, [LHsDecl RdrName]))

-- #10047: Declaration QuasiQuoters are expanded immediately, without
--         causing a group split
add gp _ (SpliceD (SpliceDecl (L _ qq@HsQuasiQuote{}) _)) ds
  = do { (ds', _) <- rnTopSpliceDecls qq
       ; addl gp (ds' ++ ds)
       }

add gp loc (SpliceD splice@(SpliceDecl _ flag)) ds
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

-- Class declarations: pull out the fixity signatures to the top
add gp@(HsGroup {hs_tyclds = ts, hs_fixds = fs}) l (TyClD d) ds
  | isClassDecl d
  = let fsigs = [ L l f | L l (FixSig f) <- tcdSigs d ] in
    addl (gp { hs_tyclds = add_tycld (L l d) ts, hs_fixds = fsigs ++ fs}) ds
  | otherwise
  = addl (gp { hs_tyclds = add_tycld (L l d) ts }) ds

-- Signatures: fixity sigs go a different place than all others
add gp@(HsGroup {hs_fixds = ts}) l (SigD (FixSig f)) ds
  = addl (gp {hs_fixds = L l f : ts}) ds
add gp@(HsGroup {hs_valds = ts}) l (SigD d) ds
  = addl (gp {hs_valds = add_sig (L l d) ts}) ds

-- Value declarations: use add_bind
add gp@(HsGroup {hs_valds  = ts}) l (ValD d) ds
  = addl (gp { hs_valds = add_bind (L l d) ts }) ds

-- Role annotations: added to the TyClGroup
add gp@(HsGroup {hs_tyclds = ts}) l (RoleAnnotD d) ds
  = addl (gp { hs_tyclds = add_role_annot (L l d) ts }) ds

-- NB instance declarations go into TyClGroups. We throw them into the first
-- group, just as we do for the TyClD case. The renamer will go on to group
-- and order them later.
add gp@(HsGroup {hs_tyclds = ts})  l (InstD d) ds
  = addl (gp { hs_tyclds = add_instd (L l d) ts }) ds

-- The rest are routine
add gp@(HsGroup {hs_derivds = ts})  l (DerivD d) ds
  = addl (gp { hs_derivds = L l d : ts }) ds
add gp@(HsGroup {hs_defds  = ts})  l (DefD d) ds
  = addl (gp { hs_defds = L l d : ts }) ds
add gp@(HsGroup {hs_fords  = ts}) l (ForD d) ds
  = addl (gp { hs_fords = L l d : ts }) ds
add gp@(HsGroup {hs_warnds  = ts})  l (WarningD d) ds
  = addl (gp { hs_warnds = L l d : ts }) ds
add gp@(HsGroup {hs_annds  = ts}) l (AnnD d) ds
  = addl (gp { hs_annds = L l d : ts }) ds
add gp@(HsGroup {hs_ruleds  = ts}) l (RuleD d) ds
  = addl (gp { hs_ruleds = L l d : ts }) ds
add gp@(HsGroup {hs_vects  = ts}) l (VectD d) ds
  = addl (gp { hs_vects = L l d : ts }) ds
add gp l (DocD d) ds
  = addl (gp { hs_docs = (L l d) : (hs_docs gp) })  ds

add_tycld :: LTyClDecl a -> [TyClGroup a] -> [TyClGroup a]
add_tycld d []       = [TyClGroup { group_tyclds = [d]
                                  , group_roles = []
                                  , group_instds = []
                                  }
                       ]
add_tycld d (ds@(TyClGroup { group_tyclds = tyclds }):dss)
  = ds { group_tyclds = d : tyclds } : dss

add_instd :: LInstDecl a -> [TyClGroup a] -> [TyClGroup a]
add_instd d []       = [TyClGroup { group_tyclds = []
                                  , group_roles = []
                                  , group_instds = [d]
                                  }
                       ]
add_instd d (ds@(TyClGroup { group_instds = instds }):dss)
  = ds { group_instds = d : instds } : dss

add_role_annot :: LRoleAnnotDecl a -> [TyClGroup a] -> [TyClGroup a]
add_role_annot d [] = [TyClGroup { group_tyclds = []
                                 , group_roles = [d]
                                 , group_instds = []
                                 }
                      ]
add_role_annot d (tycls@(TyClGroup { group_roles = roles }) : rest)
  = tycls { group_roles = d : roles } : rest

add_bind :: LHsBind a -> HsValBinds a -> HsValBinds a
add_bind b (ValBindsIn bs sigs) = ValBindsIn (bs `snocBag` b) sigs
add_bind _ (ValBindsOut {})     = panic "RdrHsSyn:add_bind"

add_sig :: LSig a -> HsValBinds a -> HsValBinds a
add_sig s (ValBindsIn bs sigs) = ValBindsIn bs (s:sigs)
add_sig _ (ValBindsOut {})     = panic "RdrHsSyn:add_sig"

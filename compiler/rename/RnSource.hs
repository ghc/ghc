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
import PrelNames        ( isUnboundName )
import Name
import NameSet
import NameEnv
import Avail
import Outputable
import Bag
import BasicTypes       ( RuleName, pprRuleName )
import FastString
import SrcLoc
import DynFlags
import HscTypes         ( HscEnv, hsc_dflags )
import ListSetOps       ( findDupsEq, removeDups )
import Digraph          ( SCC, flattenSCC, stronglyConnCompFromEdgedVertices )

import Control.Monad
import Data.List ( sortBy )
import Maybes( orElse, mapMaybe )
import qualified Data.Set as Set ( difference, fromList, toList, null )
#if __GLASGOW_HASKELL__ < 709
import Data.Traversable (traverse)
#endif

{-
@rnSourceDecl@ `renames' declarations.
It simultaneously performs dependency analysis and precedence parsing.
It also does the following error checks:
\begin{enumerate}
\item
Checks that tyvars are used properly. This includes checking
for undefined tyvars, and tyvars in contexts that are ambiguous.
(Some of this checking has now been moved to module @TcMonoType@,
since we don't have functional dependency information at this point.)
\item
Checks that all variable occurrences are defined.
\item
Checks the @(..)@ etc constraints in the export list.
\end{enumerate}
-}

-- Brings the binders of the group into scope in the appropriate places;
-- does NOT assume that anything is in scope already
rnSrcDecls :: HsGroup RdrName -> RnM (TcGblEnv, HsGroup Name)
-- Rename a top-level HsGroup; used for normal source files *and* hs-boot files
rnSrcDecls group@(HsGroup { hs_valds   = val_decls,
                            hs_splcds  = splice_decls,
                            hs_tyclds  = tycl_decls,
                            hs_instds  = inst_decls,
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

   -- (C) Extract the mapping from data constructors to field names and
   --     extend the record field env.
   --     This depends on the data constructors and field names being in
   --     scope from (B) above
   inNewEnv (extendRecordFieldEnv tycl_decls inst_decls) $ \ _ -> do {

   -- (D1) Bring pattern synonyms into scope.
   --      Need to do this before (D2) because rnTopBindsLHS
   --      looks up those pattern synonyms (Trac #9889)
   pat_syn_bndrs <- mapM newTopSrcBinder (hsPatSynBinders val_decls) ;
   tc_envs <- extendGlobalRdrEnvRn (map Avail pat_syn_bndrs) local_fix_env ;
   setEnvs tc_envs $ do {

   -- (D2) Rename the left-hand sides of the value bindings.
   --     This depends on everything from (B) being in scope,
   --     and on (C) for resolving record wild cards.
   --     It uses the fixity env from (A) to bind fixities for view patterns.
   new_lhs <- rnTopBindsLHS local_fix_env val_decls ;

   -- Bind the LHSes (and their fixities) in the global rdr environment
   let { id_bndrs = collectHsIdBinders new_lhs } ;  -- Excludes pattern-synonym binders
                                                    -- They are already in scope
   traceRn (text "rnSrcDecls" <+> ppr id_bndrs) ;
   tc_envs <- extendGlobalRdrEnvRn (map Avail id_bndrs) local_fix_env ;
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
   traceRn (text "Start rnTyClDecls") ;
   (rn_tycl_decls, src_fvs1) <- rnTyClDecls tycl_decls ;

   -- (F) Rename Value declarations right-hand sides
   traceRn (text "Start rnmono") ;
   let { val_bndr_set = mkNameSet id_bndrs `unionNameSet` mkNameSet pat_syn_bndrs } ;
   (rn_val_decls, bind_dus) <- rnTopBindsRHS val_bndr_set new_lhs ;
   traceRn (text "finish rnmono" <+> ppr rn_val_decls) ;

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

   (rn_inst_decls,    src_fvs2) <- rnList rnSrcInstDecl   inst_decls ;
   (rn_rule_decls,    src_fvs3) <- setXOptM Opt_ScopedTypeVariables $
                                   rnList rnHsRuleDecls rule_decls ;
                           -- Inside RULES, scoped type variables are on
   (rn_vect_decls,    src_fvs4) <- rnList rnHsVectDecl    vect_decls ;
   (rn_foreign_decls, src_fvs5) <- rnList rnHsForeignDecl foreign_decls ;
   (rn_ann_decls,     src_fvs6) <- rnList rnAnnDecl       ann_decls ;
   (rn_default_decls, src_fvs7) <- rnList rnDefaultDecl   default_decls ;
   (rn_deriv_decls,   src_fvs8) <- rnList rnSrcDerivDecl  deriv_decls ;
   (rn_splice_decls,  src_fvs9) <- rnList rnSpliceDecl    splice_decls ;
      -- Haddock docs; no free vars
   rn_docs <- mapM (wrapLocM rnDocDecl) docs ;

    last_tcg_env <- getGblEnv ;
   -- (I) Compute the results and return
   let {rn_group = HsGroup { hs_valds   = rn_val_decls,
                             hs_splcds  = rn_splice_decls,
                             hs_tyclds  = rn_tycl_decls,
                             hs_instds  = rn_inst_decls,
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

        tcf_bndrs = hsTyClForeignBinders rn_tycl_decls rn_inst_decls rn_foreign_decls ;
        other_def  = (Just (mkNameSet tcf_bndrs), emptyNameSet) ;
        other_fvs  = plusFVs [src_fvs1, src_fvs2, src_fvs3, src_fvs4,
                              src_fvs5, src_fvs6, src_fvs7, src_fvs8,
                              src_fvs9] ;
                -- It is tiresome to gather the binders from type and class decls

        src_dus = [other_def] `plusDU` bind_dus `plusDU` usesOnly other_fvs ;
                -- Instance decls may have occurrences of things bound in bind_dus
                -- so we must put other_fvs last

        final_tcg_env = let tcg_env' = (last_tcg_env `addTcgDUs` src_dus)
                        in -- we return the deprecs in the env, not in the HsGroup above
                        tcg_env' { tcg_warns = tcg_warns tcg_env' `plusWarns` rn_warns };
       } ;

   traceRn (text "finish rnSrc" <+> ppr rn_group) ;
   traceRn (text "finish Dus" <+> ppr src_dus ) ;
   return (final_tcg_env, rn_group)
                    }}}}}

-- some utils because we do this a bunch above
-- compute and install the new env
inNewEnv :: TcM TcGblEnv -> (TcGblEnv -> TcM a) -> TcM a
inNewEnv env cont = do e <- env
                       setGblEnv e $ cont e

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
           return [ L name_loc name | name <- names ]
    what = ptext (sLit "fixity signature")

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
          ; return [(nameOccName name, txt) | name <- names] }

   what = ptext (sLit "deprecation")

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
  = vcat [ptext (sLit "Multiple warning declarations for") <+> quotes (ppr rdr_name),
          ptext (sLit "also at ") <+> ppr loc]

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
       ; (expr', expr_fvs) <- setStage (Splice False) $
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
rnHsForeignDecl (ForeignImport name ty _ spec)
  = do { topEnv :: HscEnv <- getTopEnv
       ; name' <- lookupLocatedTopBndrRn name
       ; (ty', fvs) <- rnLHsType (ForeignDeclCtx name) ty

        -- Mark any PackageTarget style imports as coming from the current package
       ; let packageKey = thisPackage $ hsc_dflags topEnv
             spec'      = patchForeignImport packageKey spec

       ; return (ForeignImport name' ty' noForeignImportCoercionYet spec', fvs) }

rnHsForeignDecl (ForeignExport name ty _ spec)
  = do { name' <- lookupLocatedOccRn name
       ; (ty', fvs) <- rnLHsType (ForeignDeclCtx name) ty
       ; return (ForeignExport name' ty' noForeignExportCoercionYet spec, fvs `addOneFV` unLoc name') }
        -- NB: a foreign export is an *occurrence site* for name, so
        --     we add it to the free-variable list.  It might, for example,
        --     be imported from another module

-- | For Windows DLLs we need to know what packages imported symbols are from
--      to generate correct calls. Imported symbols are tagged with the current
--      package, so if they get inlined across a package boundry we'll still
--      know where they're from.
--
patchForeignImport :: PackageKey -> ForeignImport -> ForeignImport
patchForeignImport packageKey (CImport cconv safety fs spec src)
        = CImport cconv safety fs (patchCImportSpec packageKey spec) src

patchCImportSpec :: PackageKey -> CImportSpec -> CImportSpec
patchCImportSpec packageKey spec
 = case spec of
        CFunction callTarget    -> CFunction $ patchCCallTarget packageKey callTarget
        _                       -> spec

patchCCallTarget :: PackageKey -> CCallTarget -> CCallTarget
patchCCallTarget packageKey callTarget =
  case callTarget of
  StaticTarget src label Nothing isFun
                              -> StaticTarget src label (Just packageKey) isFun
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

rnClsInstDecl :: ClsInstDecl RdrName -> RnM (ClsInstDecl Name, FreeVars)
rnClsInstDecl (ClsInstDecl { cid_poly_ty = inst_ty, cid_binds = mbinds
                           , cid_sigs = uprags, cid_tyfam_insts = ats
                           , cid_overlap_mode = oflag
                           , cid_datafam_insts = adts })
        -- Used for both source and interface file decls
  = do { (inst_ty', inst_fvs) <- rnLHsInstType (text "In an instance declaration") inst_ty
       ; case splitLHsInstDeclTy_maybe inst_ty' of {
           Nothing -> return (ClsInstDecl { cid_poly_ty = inst_ty', cid_binds = emptyLHsBinds
                                          , cid_sigs = [], cid_tyfam_insts = []
                                          , cid_overlap_mode = oflag
                                          , cid_datafam_insts = [] }
                             , inst_fvs) ;
           Just (inst_tyvars, _, L _ cls,_) ->

    do { let ktv_names = hsLKiTyVarNames inst_tyvars

        -- Rename the bindings
        -- The typechecker (not the renamer) checks that all
        -- the bindings are for the right class
        -- (Slightly strangely) when scoped type variables are on, the
        -- forall-d tyvars scope over the method bindings too
       ; (mbinds', uprags', meth_fvs) <- rnMethodBinds False cls ktv_names mbinds uprags

       -- Rename the associated types, and type signatures
       -- Both need to have the instance type variables in scope
       ; traceRn (text "rnSrcInstDecl"  <+> ppr inst_ty' $$ ppr inst_tyvars $$ ppr ktv_names)
       ; ((ats', adts'), more_fvs)
             <- extendTyVarEnvFVRn ktv_names $
                do { (ats',  at_fvs)  <- rnATInstDecls rnTyFamInstDecl cls inst_tyvars ats
                   ; (adts', adt_fvs) <- rnATInstDecls rnDataFamInstDecl cls inst_tyvars adts
                   ; return ( (ats', adts'), at_fvs `plusFV` adt_fvs) }

       ; let all_fvs = meth_fvs `plusFV` more_fvs
                                `plusFV` inst_fvs
       ; return (ClsInstDecl { cid_poly_ty = inst_ty', cid_binds = mbinds'
                             , cid_sigs = uprags', cid_tyfam_insts = ats'
                             , cid_overlap_mode = oflag
                             , cid_datafam_insts = adts' },
                 all_fvs) } } }
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
              -> Maybe (Name, [Name])
              -> Located RdrName
              -> [LHsType RdrName]
              -> rhs
              -> (HsDocContext -> rhs -> RnM (rhs', FreeVars))
              -> RnM (Located Name, HsWithBndrs Name [LHsType Name], rhs',
                      FreeVars)
rnFamInstDecl doc mb_cls tycon pats payload rnPayload
  = do { tycon'   <- lookupFamInstName (fmap fst mb_cls) tycon
       ; let loc = case pats of
                     []             -> pprPanic "rnFamInstDecl" (ppr tycon)
                     (L loc _ : []) -> loc
                     (L loc _ : ps) -> combineSrcSpans loc (getLoc (last ps))
             (kv_rdr_names, tv_rdr_names) = extractHsTysRdrTyVars pats


       ; rdr_env  <- getLocalRdrEnv
       ; kv_names <- mapM (newTyVarNameRn mb_cls rdr_env loc) kv_rdr_names
       ; tv_names <- mapM (newTyVarNameRn mb_cls rdr_env loc) tv_rdr_names
             -- All the free vars of the family patterns
             -- with a sensible binding location
       ; ((pats', payload'), fvs)
              <- bindLocalNamesFV kv_names $
                 bindLocalNamesFV tv_names $
                 do { (pats', pat_fvs) <- rnLHsTypes doc pats
                    ; (payload', rhs_fvs) <- rnPayload doc payload

                         -- See Note [Renaming associated types]
                    ; let lhs_names = mkNameSet kv_names `unionNameSet` mkNameSet tv_names
                          bad_tvs = case mb_cls of
                                      Nothing           -> []
                                      Just (_,cls_tkvs) -> filter is_bad cls_tkvs

                          is_bad cls_tkv = cls_tkv `elemNameSet` rhs_fvs
                                        && not (cls_tkv `elemNameSet` lhs_names)

                    ; unless (null bad_tvs) (badAssocRhs bad_tvs)
                    ; return ((pats', payload'), rhs_fvs `plusFV` pat_fvs) }


       ; let all_fvs = fvs `addOneFV` unLoc tycon'
             awcs = concatMap collectAnonymousWildCardNames pats'
       ; return (tycon',
                 HsWB { hswb_cts = pats', hswb_kvs = kv_names,
                        hswb_tvs = tv_names, hswb_wcs = awcs },
                 payload',
                 all_fvs) }
             -- type instance => use, hence addOneFV
  where
    collectAnonymousWildCardNames ty
      = [ wildCardName wc
        | L _ wc <- snd (collectWildCards ty)
        , isAnonWildCard wc ]


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
                                , tfe_pats  = HsWB { hswb_cts = pats }
                                , tfe_rhs   = rhs })
  = do { (tycon', pats', rhs', fvs) <-
           rnFamInstDecl (TySynCtx tycon) mb_cls tycon pats rhs rnTySyn
       ; return (TyFamEqn { tfe_tycon = tycon'
                          , tfe_pats  = pats'
                          , tfe_rhs   = rhs' }, fvs) }

rnTyFamDefltEqn :: Name
                -> TyFamDefltEqn RdrName
                -> RnM (TyFamDefltEqn Name, FreeVars)
rnTyFamDefltEqn cls (TyFamEqn { tfe_tycon = tycon
                              , tfe_pats  = tyvars
                              , tfe_rhs   = rhs })
  = bindHsTyVars ctx (Just cls) [] tyvars $ \ tyvars' ->
    do { tycon'      <- lookupFamInstName (Just cls) tycon
       ; (rhs', fvs) <- rnLHsType ctx rhs
       ; return (TyFamEqn { tfe_tycon = tycon'
                          , tfe_pats  = tyvars'
                          , tfe_rhs   = rhs' }, fvs) }
  where
    ctx = TyFamilyCtx tycon

rnDataFamInstDecl :: Maybe (Name, [Name])
                  -> DataFamInstDecl RdrName
                  -> RnM (DataFamInstDecl Name, FreeVars)
rnDataFamInstDecl mb_cls (DataFamInstDecl { dfid_tycon = tycon
                                          , dfid_pats  = HsWB { hswb_cts = pats }
                                          , dfid_defn  = defn })
  = do { (tycon', pats', defn', fvs) <-
           rnFamInstDecl (TyDataCtx tycon) mb_cls tycon pats defn rnDataDefn
       ; return (DataFamInstDecl { dfid_tycon = tycon'
                                 , dfid_pats  = pats'
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
              -> LHsTyVarBndrs Name
              -> [Located (decl RdrName)]
              -> RnM ([Located (decl Name)], FreeVars)
-- Used for data and type family defaults in a class decl
-- and the family instance declarations in an instance
--
-- NB: We allow duplicate associated-type decls;
--     See Note [Associated type instances] in TcInstDcls
rnATInstDecls rnFun cls hs_tvs at_insts
  = rnList (rnFun (Just (cls, tv_ns))) at_insts
  where
    tv_ns = hsLKiTyVarNames hs_tvs
    -- See Note [Renaming associated types]

{-
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
rnSrcDerivDecl (DerivDecl ty overlap)
  = do { standalone_deriv_ok <- xoptM Opt_StandaloneDeriving
       ; unless standalone_deriv_ok (addErr standaloneDerivErr)
       ; (ty', fvs) <- rnLHsInstType (text "In a deriving declaration") ty
       ; return (DerivDecl ty' overlap, fvs) }

standaloneDerivErr :: SDoc
standaloneDerivErr
  = hang (ptext (sLit "Illegal standalone deriving declaration"))
       2 (ptext (sLit "Use StandaloneDeriving to enable this extension"))

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
      = rnHsBndrSig doc bsig $ \ bsig' ->
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
    check (HsVar v) | v `notElem` foralls = Nothing
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
  = sep [ptext (sLit "Rule") <+> doubleQuotes (ftext name) <> colon,
         ptext (sLit "Forall'd variable") <+> quotes (ppr var) <+>
                ptext (sLit "does not appear on left hand side")]

badRuleLhsErr :: FastString -> LHsExpr Name -> HsExpr Name -> SDoc
badRuleLhsErr name lhs bad_e
  = sep [ptext (sLit "Rule") <+> pprRuleName name <> colon,
         nest 4 (vcat [err,
                       ptext (sLit "in left-hand side:") <+> ppr lhs])]
    $$
    ptext (sLit "LHS must be of form (f e1 .. en) where f is not forall'd")
  where
    err = case bad_e of
            HsUnboundVar occ -> ptext (sLit "Not in scope:") <+> ppr occ
            _ -> ptext (sLit "Illegal expression:") <+> ppr bad_e

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
               [ ptext (sLit "IMPLEMENTATION RESTRICTION: right-hand side of a VECTORISE pragma")
               , ptext (sLit "must be an identifier")
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
  = do { (instTy', fvs) <- rnLHsInstType (text "In a VECTORISE pragma") instTy
       ; return (HsVectInstIn instTy', fvs)
       }
rnHsVectDecl (HsVectInstOut _)
  = panic "RnSource.rnHsVectDecl: Unexpected 'HsVectInstOut'"

{-
*********************************************************
*                                                      *
\subsection{Type, class and iface sig declarations}
*                                                      *
*********************************************************

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


Note [Extra dependencies from .hs-boot files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following case:

A.hs-boot
  module A where
    data A1

B.hs
  module B where
    import {-# SOURCE #-} A
    type DisguisedA1 = A1
    data B1 = B1 DisguisedA1

A.hs
  module A where
    import B
    data A2 = A2 A1
    data A1 = A1 B1

Here A1 is really recursive (via B1), but we won't see that easily when
doing dependency analysis when compiling A.hs

To handle this problem, we add a dependency
  - from every local declaration
  - to everything that comes from this module's .hs-boot file.
In this case, we'll ad and edges
  - from A2 to A1 (but that edge is there already)
  - from A1 to A1 (which is new)

Well, not quite *every* declaration. Imagine module A
above had another datatype declaration:

  data A3 = A3 Int

Even though A3 has a dependency (on Int), all its dependencies are from things
that live on other packages. Since we don't have mutual dependencies across
packages, it is safe not to add the dependencies on the .hs-boot stuff to A2.

Hence function Name.thisPackageImport.

See also Note [Grouping of type and class declarations] in TcTyClsDecls.
-}


rnTyClDecls :: [TyClGroup RdrName]
            -> RnM ([TyClGroup Name], FreeVars)
-- Rename the declarations and do depedency analysis on them
rnTyClDecls tycl_ds
  = do { ds_w_fvs       <- mapM (wrapLocFstM rnTyClDecl) (tyClGroupConcat tycl_ds)
       ; let decl_names = mkNameSet (map (tcdName . unLoc . fst) ds_w_fvs)
       ; role_annot_env <- rnRoleAnnots decl_names (concatMap group_roles tycl_ds)
       ; tcg_env        <- getGblEnv
       ; let this_mod  = tcg_mod tcg_env
             boot_info = tcg_self_boot tcg_env

             add_boot_deps :: [(LTyClDecl Name, FreeVars)] -> [(LTyClDecl Name, FreeVars)]
             -- See Note [Extra dependencies from .hs-boot files]
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
                 = foldNameSet ((||) . nameIsHomePackageImport this_mod)
                               False fvs

             ds_w_fvs' = add_boot_deps ds_w_fvs

             sccs :: [SCC (LTyClDecl Name)]
             sccs = depAnalTyClDecls ds_w_fvs'

             all_fvs = foldr (plusFV . snd) emptyFVs ds_w_fvs'

             raw_groups = map flattenSCC sccs
             -- See Note [Role annotations in the renamer]
             (groups, orphan_roles)
               = foldr (\group (groups_acc, orphans_acc) ->
                         let names = map (tcdName . unLoc) group
                             roles = mapMaybe (lookupNameEnv orphans_acc) names
                             orphans' = delListFromNameEnv orphans_acc names
                              -- there doesn't seem to be an interface to
                              -- do the above more efficiently
                         in ( TyClGroup { group_tyclds = group
                                        , group_roles  = roles } : groups_acc
                            , orphans' )
                       )
                       ([], role_annot_env)
                       raw_groups

       ; mapM_ orphanRoleAnnotErr (nameEnvElts orphan_roles)
       ; traceRn (text "rnTycl"  <+> (ppr ds_w_fvs $$ ppr sccs))
       ; return (groups, all_fvs) }

rnTyClDecl :: TyClDecl RdrName
           -> RnM (TyClDecl Name, FreeVars)

-- All flavours of type family declarations ("type family", "newtype family",
-- and "data family"), both top level and (for an associated type)
-- in a class decl
rnTyClDecl (FamDecl { tcdFam = decl })
  = do { (decl', fvs) <- rnFamDecl Nothing decl
       ; return (FamDecl decl', fvs) }

rnTyClDecl (SynDecl { tcdLName = tycon, tcdTyVars = tyvars, tcdRhs = rhs })
  = do { tycon' <- lookupLocatedTopBndrRn tycon
       ; let kvs = fst (extractHsTyRdrTyVars rhs)
             doc = TySynCtx tycon
       ; traceRn (text "rntycl-ty" <+> ppr tycon <+> ppr kvs)
       ; ((tyvars', rhs'), fvs) <- bindHsTyVars doc Nothing kvs tyvars $
                                    \ tyvars' ->
                                    do { (rhs', fvs) <- rnTySyn doc rhs
                                       ; return ((tyvars', rhs'), fvs) }
       ; return (SynDecl { tcdLName = tycon', tcdTyVars = tyvars'
                         , tcdRhs = rhs', tcdFVs = fvs }, fvs) }

-- "data", "newtype" declarations
-- both top level and (for an associated type) in an instance decl
rnTyClDecl (DataDecl { tcdLName = tycon, tcdTyVars = tyvars, tcdDataDefn = defn })
  = do { tycon' <- lookupLocatedTopBndrRn tycon
       ; let kvs = extractDataDefnKindVars defn
             doc = TyDataCtx tycon
       ; traceRn (text "rntycl-data" <+> ppr tycon <+> ppr kvs)
       ; ((tyvars', defn'), fvs) <-
                      bindHsTyVars doc Nothing kvs tyvars $ \ tyvars' ->
                                    do { (defn', fvs) <- rnDataDefn doc defn
                                       ; return ((tyvars', defn'), fvs) }
       ; return (DataDecl { tcdLName = tycon', tcdTyVars = tyvars'
                          , tcdDataDefn = defn', tcdFVs = fvs }, fvs) }

rnTyClDecl (ClassDecl {tcdCtxt = context, tcdLName = lcls,
                              tcdTyVars = tyvars, tcdFDs = fds, tcdSigs = sigs,
                              tcdMeths = mbinds, tcdATs = ats, tcdATDefs = at_defs,
                              tcdDocs = docs})
  = do  { lcls' <- lookupLocatedTopBndrRn lcls
        ; let cls' = unLoc lcls'
              kvs = []  -- No scoped kind vars except those in
                        -- kind signatures on the tyvars

        -- Tyvars scope over superclass context and method signatures
        ; ((tyvars', context', fds', ats'), stuff_fvs)
            <- bindHsTyVars cls_doc Nothing kvs tyvars $ \ tyvars' -> do
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
        ; let sig_rdr_names_w_locs = [op | L _ (TypeSig ops _ _) <- sigs, op <- ops]
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
            <- rnMethodBinds True cls' (hsLKiTyVarNames tyvars') mbinds sigs
                -- No need to check for duplicate method signatures
                -- since that is done by RnNames.extendGlobalRdrEnvRn
                -- and the methods are already in scope

  -- Haddock docs
        ; docs' <- mapM (wrapLocM rnDocDecl) docs

        ; let all_fvs = meth_fvs `plusFV` stuff_fvs `plusFV` fv_at_defs
        ; return (ClassDecl { tcdCtxt = context', tcdLName = lcls',
                              tcdTyVars = tyvars', tcdFDs = fds', tcdSigs = sigs',
                              tcdMeths = mbinds', tcdATs = ats', tcdATDefs = at_defs',
                              tcdDocs = docs', tcdFVs = all_fvs },
                  all_fvs ) }
  where
    cls_doc  = ClassDeclCtx lcls

-- "type" and "type instance" declarations
rnTySyn :: HsDocContext -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnTySyn doc rhs = rnLHsType doc rhs

-- | Renames role annotations, returning them as the values in a NameEnv
-- and checks for duplicate role annotations.
-- It is quite convenient to do both of these in the same place.
-- See also Note [Role annotations in the renamer]
rnRoleAnnots :: NameSet  -- ^ of the decls in this group
             -> [LRoleAnnotDecl RdrName]
             -> RnM (NameEnv (LRoleAnnotDecl Name))
rnRoleAnnots decl_names role_annots
  = do {  -- check for duplicates *before* renaming, to avoid lumping
          -- together all the unboundNames
         let (no_dups, dup_annots) = removeDups role_annots_cmp role_annots
             role_annots_cmp (L _ annot1) (L _ annot2)
               = roleAnnotDeclName annot1 `compare` roleAnnotDeclName annot2
       ; mapM_ dupRoleAnnotErr dup_annots
       ; role_annots' <- mapM (wrapLocM rn_role_annot1) no_dups
          -- some of the role annots will be unbound; we don't wish
          -- to include these
       ; return $ mkNameEnv [ (name, ra)
                            | ra <- role_annots'
                            , let name = roleAnnotDeclName (unLoc ra)
                            , not (isUnboundName name) ] }
  where
    rn_role_annot1 (RoleAnnotDecl tycon roles)
      = do {  -- the name is an *occurrence*, but look it up only in the
              -- decls defined in this group (see #10263)
             tycon' <- lookupSigCtxtOccRn (RoleAnnotCtxt decl_names)
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

rnDataDefn :: HsDocContext -> HsDataDefn RdrName -> RnM (HsDataDefn Name, FreeVars)
rnDataDefn doc (HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                           , dd_ctxt = context, dd_cons = condecls
                           , dd_kindSig = sig, dd_derivs = derivs })
  = do  { checkTc (h98_style || null (unLoc context))
                  (badGadtStupidTheta doc)

        ; (sig', sig_fvs)  <- rnLHsMaybeKind doc sig
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
        ; return ( HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                              , dd_ctxt = context', dd_kindSig = sig'
                              , dd_cons = condecls'
                              , dd_derivs = derivs' }
                 , all_fvs )
        }
  where
    h98_style = case condecls of  -- Note [Stupid theta]
                     L _ (ConDecl { con_res = ResTyGADT {} }) : _  -> False
                     _                                             -> True

    rn_derivs Nothing   = return (Nothing, emptyFVs)
    rn_derivs (Just (L ld ds)) = do { (ds', fvs) <- rnLHsTypes doc ds
                                    ; return (Just (L ld ds'), fvs) }

badGadtStupidTheta :: HsDocContext -> SDoc
badGadtStupidTheta _
  = vcat [ptext (sLit "No context is allowed on a GADT-style data declaration"),
          ptext (sLit "(You can put a context on each contructor, though.)")]

rnFamDecl :: Maybe Name -- Just cls => this FamilyDecl is nested
                        --             inside an *class decl* for cls
                        --             used for associated types
          -> FamilyDecl RdrName
          -> RnM (FamilyDecl Name, FreeVars)
rnFamDecl mb_cls (FamilyDecl { fdLName = tycon, fdTyVars = tyvars
                             , fdInfo = info, fdResultSig = res_sig
                             , fdInjectivityAnn = injectivity })
  = do { tycon' <- lookupLocatedTopBndrRn tycon
       ; ((tyvars', res_sig', injectivity'), fv1) <-
            bindHsTyVars doc mb_cls kvs tyvars $ \ tyvars' ->
            do { (res_sig', fv_kind) <- wrapLocFstM (rnFamResultSig doc) res_sig
               ; injectivity' <- traverse (rnInjectivityAnn tyvars' res_sig')
                                          injectivity
               ; return ( (tyvars', res_sig', injectivity') , fv_kind )  }
       ; (info', fv2) <- rn_info info
       ; return (FamilyDecl { fdLName = tycon', fdTyVars = tyvars'
                            , fdInfo = info', fdResultSig = res_sig'
                            , fdInjectivityAnn = injectivity' }
                , fv1 `plusFV` fv2) }
  where
     doc = TyFamilyCtx tycon
     kvs = extractRdrKindSigVars res_sig

     ----------------------
     rn_info (ClosedTypeFamily (Just eqns))
       = do { (eqns', fvs) <- rnList (rnTyFamInstEqn Nothing) eqns
                                                    -- no class context,
            ; return (ClosedTypeFamily (Just eqns'), fvs) }
     rn_info (ClosedTypeFamily Nothing)
       = return (ClosedTypeFamily Nothing, emptyFVs)
     rn_info OpenTypeFamily = return (OpenTypeFamily, emptyFVs)
     rn_info DataFamily     = return (DataFamily, emptyFVs)

rnFamResultSig :: HsDocContext -> FamilyResultSig RdrName
               -> RnM (FamilyResultSig Name, FreeVars)
rnFamResultSig _ NoSig
   = return (NoSig, emptyFVs)
rnFamResultSig doc (KindSig kind)
   = do { (rndKind, ftvs) <- rnLHsKind doc kind
        ;  return (KindSig rndKind, ftvs) }
rnFamResultSig doc (TyVarSig tvbndr)
   = do { -- `TyVarSig` tells us that user named the result of a type family by
          -- writing `= tyvar` or `= (tyvar :: kind)`. In such case we want to
          -- be sure that the supplied result name is not identical to an
          -- already in-scope type variables:
          --
          --  (a) one of already declared type family arguments. Example of
          --      disallowed declaration:
          --        type family F a = a
          --
          --  (b) already in-scope type variable. This second case might happen
          --      for associated types, where type class head bounds some type
          --      variables. Example of disallowed declaration:
          --         class C a b where
          --            type F b = a | a -> b
          -- Both are caught by the "in-scope" check that comes next
          rdr_env <- getLocalRdrEnv
       ;  let resName = hsLTyVarName tvbndr
       ;  when (resName `elemLocalRdrEnv` rdr_env) $
          addErrAt (getLoc tvbndr) $
                     (hsep [ text "Type variable", quotes (ppr resName) <> comma
                           , text "naming a type family result,"
                           ] $$
                      text "shadows an already bound type variable")

       ; (tvbndr', fvs) <- rnLHsTyVarBndr doc Nothing rdr_env tvbndr
       ; return (TyVarSig tvbndr', fvs) }

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
rnInjectivityAnn :: LHsTyVarBndrs Name         -- ^ Type variables declared in
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
             do { injFrom' <- rnLTyVar True injFrom
                ; injTo'   <- mapM (rnLTyVar True) injTo
                ; return $ L srcSpan (InjectivityAnn injFrom' injTo') }

   ; let tvNames  = Set.fromList $ hsLKiTyVarNames tvBndrs
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
     injFrom' <- rnLTyVar True injFrom
     injTo'   <- mapM (rnLTyVar True) injTo
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

depAnalTyClDecls :: [(LTyClDecl Name, FreeVars)] -> [SCC (LTyClDecl Name)]
-- See Note [Dependency analysis of type and class decls]
depAnalTyClDecls ds_w_fvs
  = stronglyConnCompFromEdgedVertices edges
  where
    edges = [ (d, tcdName (unLoc d), map get_parent (nameSetElems fvs))
            | (d, fvs) <- ds_w_fvs ]

    -- We also need to consider data constructor names since
    -- they may appear in types because of promotion.
    get_parent n = lookupNameEnv assoc_env n `orElse` n

    assoc_env :: NameEnv Name   -- Maps a data constructor back
                                -- to its parent type constructor
    assoc_env = mkNameEnv $ concat assoc_env_list
    assoc_env_list = do
      (L _ d, _) <- ds_w_fvs
      case d of
        ClassDecl { tcdLName = L _ cls_name
                  , tcdATs = ats }
          -> do L _ (FamilyDecl { fdLName = L _ fam_name }) <- ats
                return [(fam_name, cls_name)]
        DataDecl { tcdLName = L _ data_name
                 , tcdDataDefn = HsDataDefn { dd_cons = cons } }
          -> do L _ dc <- cons
                return $ zip (map unLoc $ con_names dc) (repeat data_name)
        _ -> []

{-
Note [Dependency analysis of type and class decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to do dependency analysis on type and class declarations
else we get bad error messages.  Consider

     data T f a = MkT f a
     data S f a = MkS f (T f a)

This has a kind error, but the error message is better if you
check T first, (fixing its kind) and *then* S.  If you do kind
inference together, you might get an error reported in S, which
is jolly confusing.  See Trac #4875

Note [Role annotations in the renamer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
over raw_groups (of type [[TyClDecl Name]]), selecting out the relevant
role declarations for each group, as well as diminishing the annotation
environment. After the fold is complete, anything left over in the name
environment must be an orphan, and errors are generated.

An earlier version of this algorithm short-cut the orphan check by renaming
only with names declared in this module. But, this check is insufficient in
the case of staged module compilation (Template Haskell, GHCi).
See #8485. With the new lookup process (which includes types declared in other
modules), we get better error messages, too.

*********************************************************
*                                                      *
\subsection{Support code for type/data declarations}
*                                                      *
*********************************************************
-}

---------------
badAssocRhs :: [Name] -> RnM ()
badAssocRhs ns
  = addErr (hang (ptext (sLit "The RHS of an associated type declaration mentions")
                  <+> pprWithCommas (quotes . ppr) ns)
               2 (ptext (sLit "All such variables must be bound on the LHS")))

-----------------
rnConDecls :: [LConDecl RdrName] -> RnM ([LConDecl Name], FreeVars)
rnConDecls = mapFvRn (wrapLocFstM rnConDecl)

rnConDecl :: ConDecl RdrName -> RnM (ConDecl Name, FreeVars)
rnConDecl decl@(ConDecl { con_names = names, con_qvars = tvs
                        , con_cxt = lcxt@(L loc cxt), con_details = details
                        , con_res = res_ty, con_doc = mb_doc
                        , con_old_rec = old_rec, con_explicit = expl })
  = do  { mapM_ (addLocM checkConName) names
        ; when old_rec (addWarn (deprecRecSyntax decl))
        ; new_names <- mapM lookupLocatedTopBndrRn names

           -- For H98 syntax, the tvs are the existential ones
           -- For GADT syntax, the tvs are all the quantified tyvars
           -- Hence the 'filter' in the ResTyH98 case only
        ; rdr_env <- getLocalRdrEnv
        ; let arg_tys    = hsConDeclArgTys details
              (free_kvs, free_tvs) = case res_ty of
                ResTyH98 -> filterInScope rdr_env (get_rdr_tvs arg_tys)
                ResTyGADT _ ty -> get_rdr_tvs (ty : arg_tys)

         -- With an Explicit forall, check for unused binders
         -- With Implicit, find the mentioned ones, and use them as binders
         -- With Qualified, do the same as with Implicit, but give a warning
         --   See Note [Context quantification]
        ; new_tvs <- case expl of
                       Implicit -> return (mkHsQTvs (userHsTyVarBndrs loc free_tvs))
                       Qualified -> do { warnContextQuantification (docOfHsDocContext doc)
                                                                   (userHsTyVarBndrs loc free_tvs)
                                       ; return (mkHsQTvs (userHsTyVarBndrs loc free_tvs)) }
                       Explicit -> do { warnUnusedForAlls (docOfHsDocContext doc) tvs free_tvs
                                      ; return tvs }

        ; mb_doc' <- rnMbLHsDoc mb_doc

        ; bindHsTyVars doc Nothing free_kvs new_tvs $ \new_tyvars -> do
        { (new_context, fvs1) <- rnContext doc lcxt
        ; (new_details, fvs2) <- rnConDeclDetails doc details
        ; (new_details', new_res_ty, fvs3)
                     <- rnConResult doc (map unLoc new_names) new_details res_ty
        ; return (decl { con_names = new_names, con_qvars = new_tyvars
                       , con_cxt = new_context, con_details = new_details'
                       , con_res = new_res_ty, con_doc = mb_doc' },
                  fvs1 `plusFV` fvs2 `plusFV` fvs3) }}
 where
    doc = ConDeclCtx names
    get_rdr_tvs tys = extractHsTysRdrTyVars (cxt ++ tys)

rnConResult :: HsDocContext -> [Name]
            -> HsConDetails (LHsType Name) (Located [LConDeclField Name])
            -> ResType (LHsType RdrName)
            -> RnM (HsConDetails (LHsType Name) (Located [LConDeclField Name]),
                    ResType (LHsType Name), FreeVars)
rnConResult _   _   details ResTyH98 = return (details, ResTyH98, emptyFVs)
rnConResult doc _con details (ResTyGADT ls ty)
  = do { (ty', fvs) <- rnLHsType doc ty
       ; let (arg_tys, res_ty) = splitHsFunType ty'
                -- We can finally split it up,
                -- now the renamer has dealt with fixities
                -- See Note [Sorting out the result type] in RdrHsSyn

       ; case details of
           InfixCon {}  -> pprPanic "rnConResult" (ppr ty)
           -- See Note [Sorting out the result type] in RdrHsSyn

           RecCon {}    -> do { unless (null arg_tys)
                                       (addErr (badRecResTy (docOfHsDocContext doc)))
                              ; return (details, ResTyGADT ls res_ty, fvs) }

           PrefixCon {} -> return (PrefixCon arg_tys, ResTyGADT ls res_ty, fvs)}

rnConDeclDetails
   :: HsDocContext
   -> HsConDetails (LHsType RdrName) (Located [LConDeclField RdrName])
   -> RnM (HsConDetails (LHsType Name) (Located [LConDeclField Name]), FreeVars)
rnConDeclDetails doc (PrefixCon tys)
  = do { (new_tys, fvs) <- rnLHsTypes doc tys
       ; return (PrefixCon new_tys, fvs) }

rnConDeclDetails doc (InfixCon ty1 ty2)
  = do { (new_ty1, fvs1) <- rnLHsType doc ty1
       ; (new_ty2, fvs2) <- rnLHsType doc ty2
       ; return (InfixCon new_ty1 new_ty2, fvs1 `plusFV` fvs2) }

rnConDeclDetails doc (RecCon (L l fields))
  = do  { (new_fields, fvs) <- rnConDeclFields doc fields
                -- No need to check for duplicate fields
                -- since that is done by RnNames.extendGlobalRdrEnvRn
        ; return (RecCon (L l new_fields), fvs) }

-------------------------------------------------
deprecRecSyntax :: ConDecl RdrName -> SDoc
deprecRecSyntax decl
  = vcat [ ptext (sLit "Declaration of") <+> quotes (ppr (con_names decl))
                 <+> ptext (sLit "uses deprecated syntax")
         , ptext (sLit "Instead, use the form")
         , nest 2 (ppr decl) ]   -- Pretty printer uses new form

badRecResTy :: SDoc -> SDoc
badRecResTy doc = ptext (sLit "Malformed constructor signature") $$ doc

{-
*********************************************************
*                                                      *
\subsection{Support code for type/data declarations}
*                                                      *
*********************************************************

Get the mapping from constructors to fields for this module.
It's convenient to do this after the data type decls have been renamed
-}

extendRecordFieldEnv :: [TyClGroup RdrName] -> [LInstDecl RdrName] -> TcM TcGblEnv
extendRecordFieldEnv tycl_decls inst_decls
  = do  { tcg_env <- getGblEnv
        ; field_env' <- foldrM get_con (tcg_field_env tcg_env) all_data_cons
        ; return (tcg_env { tcg_field_env = field_env' }) }
  where
    -- we want to lookup:
    --  (a) a datatype constructor
    --  (b) a record field
    -- knowing that they're from this module.
    -- lookupLocatedTopBndrRn does this, because it does a lookupGreLocalRn_maybe,
    -- which keeps only the local ones.
    lookup x = do { x' <- lookupLocatedTopBndrRn x
                    ; return $ unLoc x'}

    all_data_cons :: [ConDecl RdrName]
    all_data_cons = [con | HsDataDefn { dd_cons = cons } <- all_ty_defs
                         , L _ con <- cons ]
    all_ty_defs = [ defn | L _ (DataDecl { tcdDataDefn = defn })
                                                 <- tyClGroupConcat tycl_decls ]
               ++ map dfid_defn (instDeclDataFamInsts inst_decls)
                                              -- Do not forget associated types!

    get_con (ConDecl { con_names = cons, con_details = RecCon flds })
            (RecFields env fld_set)
        = do { cons' <- mapM lookup cons
             ; flds' <- mapM lookup (concatMap (cd_fld_names . unLoc)
                                               (unLoc flds))
             ; let env'    = foldl (\e c -> extendNameEnv e c flds') env cons'

                   fld_set' = extendNameSetList fld_set flds'
             ; return $ (RecFields env' fld_set') }
    get_con _ env = return env

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
           ImplicitSplice -> do { th_on <- xoptM Opt_TemplateHaskell
                                ; unless th_on $ setSrcSpan loc $
                                  failWith badImplicitSplice }

       ; return (gp, Just (splice, ds)) }
  where
    badImplicitSplice = ptext (sLit "Parse error: naked expression at top level")
                     $$ ptext (sLit "Perhaps you intended to use TemplateHaskell")

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

-- The rest are routine
add gp@(HsGroup {hs_instds = ts})  l (InstD d) ds
  = addl (gp { hs_instds = L l d : ts }) ds
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
add_tycld d []       = [TyClGroup { group_tyclds = [d], group_roles = [] }]
add_tycld d (ds@(TyClGroup { group_tyclds = tyclds }):dss)
  = ds { group_tyclds = d : tyclds } : dss

add_role_annot :: LRoleAnnotDecl a -> [TyClGroup a] -> [TyClGroup a]
add_role_annot d [] = [TyClGroup { group_tyclds = [], group_roles = [d] }]
add_role_annot d (tycls@(TyClGroup { group_roles = roles }) : rest)
  = tycls { group_roles = d : roles } : rest

add_bind :: LHsBind a -> HsValBinds a -> HsValBinds a
add_bind b (ValBindsIn bs sigs) = ValBindsIn (bs `snocBag` b) sigs
add_bind _ (ValBindsOut {})     = panic "RdrHsSyn:add_bind"

add_sig :: LSig a -> HsValBinds a -> HsValBinds a
add_sig s (ValBindsIn bs sigs) = ValBindsIn bs (s:sigs)
add_sig _ (ValBindsOut {})     = panic "RdrHsSyn:add_sig"

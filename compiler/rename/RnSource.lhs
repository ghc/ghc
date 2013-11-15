%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnSource (
        rnSrcDecls, addTcgDUs, rnTyClDecls, findSplice
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} RnExpr( rnLExpr )
import {-# SOURCE #-} RnSplice ( rnSpliceDecl )
import {-# SOURCE #-} TcSplice ( runQuasiQuoteDecl )

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
import BasicTypes       ( RuleName )
import FastString
import SrcLoc
import DynFlags
import HscTypes         ( HscEnv, hsc_dflags )
import ListSetOps       ( findDupsEq, removeDups )
import Digraph          ( SCC, flattenSCC, stronglyConnCompFromEdgedVertices )
import Util             ( mapSnd )

import Control.Monad
import Data.List( partition, sortBy )
import Data.Traversable (traverse)
import Maybes( orElse, mapMaybe )
\end{code}

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
Checks that all variable occurences are defined.
\item
Checks the @(..)@ etc constraints in the export list.
\end{enumerate}


\begin{code}
-- Brings the binders of the group into scope in the appropriate places;
-- does NOT assume that anything is in scope already
rnSrcDecls :: [Name] -> HsGroup RdrName -> RnM (TcGblEnv, HsGroup Name)
-- Rename a HsGroup; used for normal source files *and* hs-boot files
rnSrcDecls extra_deps group@(HsGroup { hs_valds   = val_decls,
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
   --     Also checks for duplcates.
   local_fix_env <- makeMiniFixityEnv fix_decls ;

   -- (B) Bring top level binders (and their fixities) into scope,
   --     *except* for the value bindings, which get brought in below.
   --     However *do* include class ops, data constructors
   --     And for hs-boot files *do* include the value signatures
   (tc_envs, tc_bndrs) <- getLocalNonValBinders local_fix_env group ;
   setEnvs tc_envs $ do {

   failIfErrsM ; -- No point in continuing if (say) we have duplicate declarations

   -- (C) Extract the mapping from data constructors to field names and
   --     extend the record field env.
   --     This depends on the data constructors and field names being in
   --     scope from (B) above
   inNewEnv (extendRecordFieldEnv tycl_decls inst_decls) $ \ _ -> do {

   -- (D) Rename the left-hand sides of the value bindings.
   --     This depends on everything from (B) being in scope,
   --     and on (C) for resolving record wild cards.
   --     It uses the fixity env from (A) to bind fixities for view patterns.
   new_lhs <- rnTopBindsLHS local_fix_env val_decls ;
   -- bind the LHSes (and their fixities) in the global rdr environment
   let { val_binders = collectHsValBinders new_lhs ;
         all_bndrs   = addListToNameSet tc_bndrs val_binders ;
         val_avails  = map Avail val_binders  } ;
   (tcg_env, tcl_env) <- extendGlobalRdrEnvRn val_avails local_fix_env ;
   traceRn (ptext (sLit "Val binders") <+> (ppr val_binders)) ;
   setEnvs (tcg_env, tcl_env) $ do {

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
   (rn_tycl_decls, src_fvs1) <- rnTyClDecls extra_deps tycl_decls ;

   -- (F) Rename Value declarations right-hand sides
   traceRn (text "Start rnmono") ;
   (rn_val_decls, bind_dus) <- rnTopBindsRHS all_bndrs new_lhs ;
   traceRn (text "finish rnmono" <+> ppr rn_val_decls) ;

   -- (G) Rename Fixity and deprecations

   -- Rename fixity declarations and error if we try to
   -- fix something from another module (duplicates were checked in (A))
   rn_fix_decls <- rnSrcFixityDecls all_bndrs fix_decls ;

   -- Rename deprec decls;
   -- check for duplicates and ensure that deprecated things are defined locally
   -- at the moment, we don't keep these around past renaming
   rn_warns <- rnSrcWarnDecls all_bndrs warn_decls ;

   -- (H) Rename Everything else

   (rn_inst_decls,    src_fvs2) <- rnList rnSrcInstDecl   inst_decls ;
   (rn_rule_decls,    src_fvs3) <- setXOptM Opt_ScopedTypeVariables $
                                   rnList rnHsRuleDecl    rule_decls ;
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

        tycl_bndrs = hsTyClDeclsBinders rn_tycl_decls rn_inst_decls ;
        ford_bndrs = hsForeignDeclsBinders rn_foreign_decls ;
        other_def  = (Just (mkNameSet tycl_bndrs `unionNameSets` mkNameSet ford_bndrs), emptyNameSet) ;
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
                    }}}}

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
\end{code}


%*********************************************************
%*                                                       *
        HsDoc stuff
%*                                                       *
%*********************************************************

\begin{code}
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
\end{code}


%*********************************************************
%*                                                       *
        Source-code fixity declarations
%*                                                       *
%*********************************************************

\begin{code}
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
    sig_ctxt = TopSigCtxt bndr_set True
       -- True <=> can give fixity for class decls and record selectors

    rn_decl :: LFixitySig RdrName -> RnM [LFixitySig Name]
        -- GHC extension: look up both the tycon and data con
        -- for con-like things; hence returning a list
        -- If neither are in scope, report an error; otherwise
        -- return a fixity sig for each (slightly odd)
    rn_decl (L loc (FixitySig (L name_loc rdr_name) fixity))
      = setSrcSpan name_loc $
                    -- this lookup will fail if the definition isn't local
        do names <- lookupLocalTcNames sig_ctxt what rdr_name
           return [ L loc (FixitySig (L name_loc name) fixity)
                  | name <- names ]
    what = ptext (sLit "fixity signature")
\end{code}


%*********************************************************
%*                                                       *
        Source-code deprecations declarations
%*                                                       *
%*********************************************************

Check that the deprecated names are defined, are defined locally, and
that there are no duplicate deprecations.

It's only imported deprecations, dealt with in RnIfaces, that we
gather them together.

\begin{code}
-- checks that the deprecations are defined locally, and that there are no duplicates
rnSrcWarnDecls :: NameSet -> [LWarnDecl RdrName] -> RnM Warnings
rnSrcWarnDecls _ []
  = return NoWarnings

rnSrcWarnDecls bndr_set decls
  = do { -- check for duplicates
       ; mapM_ (\ dups -> let (L loc rdr:lrdr':_) = dups
                          in addErrAt loc (dupWarnDecl lrdr' rdr))
               warn_rdr_dups
       ; pairs_s <- mapM (addLocM rn_deprec) decls
       ; return (WarnSome ((concat pairs_s))) }
 where
   sig_ctxt = TopSigCtxt bndr_set True
      -- True <=> Can give deprecations for class ops and record sels

   rn_deprec (Warning rdr_name txt)
       -- ensures that the names are defined locally
     = do { names <- lookupLocalTcNames sig_ctxt what rdr_name
          ; return [(nameOccName name, txt) | name <- names] }

   what = ptext (sLit "deprecation")

   warn_rdr_dups = findDupRdrNames (map (\ (L loc (Warning rdr_name _)) -> L loc rdr_name) decls)

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

\end{code}

%*********************************************************
%*                                                      *
\subsection{Annotation declarations}
%*                                                      *
%*********************************************************

\begin{code}
rnAnnDecl :: AnnDecl RdrName -> RnM (AnnDecl Name, FreeVars)
rnAnnDecl ann@(HsAnnotation provenance expr)
  = addErrCtxt (annCtxt ann) $
    do { (provenance', provenance_fvs) <- rnAnnProvenance provenance
       ; (expr', expr_fvs) <- setStage (Splice False) $
                              rnLExpr expr
       ; return (HsAnnotation provenance' expr', provenance_fvs `plusFV` expr_fvs) }

rnAnnProvenance :: AnnProvenance RdrName -> RnM (AnnProvenance Name, FreeVars)
rnAnnProvenance provenance = do
    provenance' <- traverse lookupTopBndrRn provenance
    return (provenance', maybe emptyFVs unitFV (annProvenanceName_maybe provenance'))
\end{code}

%*********************************************************
%*                                                      *
\subsection{Default declarations}
%*                                                      *
%*********************************************************

\begin{code}
rnDefaultDecl :: DefaultDecl RdrName -> RnM (DefaultDecl Name, FreeVars)
rnDefaultDecl (DefaultDecl tys)
  = do { (tys', fvs) <- rnLHsTypes doc_str tys
       ; return (DefaultDecl tys', fvs) }
  where
    doc_str = DefaultDeclCtx
\end{code}

%*********************************************************
%*                                                      *
\subsection{Foreign declarations}
%*                                                      *
%*********************************************************

\begin{code}
rnHsForeignDecl :: ForeignDecl RdrName -> RnM (ForeignDecl Name, FreeVars)
rnHsForeignDecl (ForeignImport name ty _ spec)
  = do { topEnv :: HscEnv <- getTopEnv
       ; name' <- lookupLocatedTopBndrRn name
       ; (ty', fvs) <- rnLHsType (ForeignDeclCtx name) ty

        -- Mark any PackageTarget style imports as coming from the current package
       ; let packageId = thisPackage $ hsc_dflags topEnv
             spec'     = patchForeignImport packageId spec

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
patchForeignImport :: PackageId -> ForeignImport -> ForeignImport
patchForeignImport packageId (CImport cconv safety fs spec)
        = CImport cconv safety fs (patchCImportSpec packageId spec)

patchCImportSpec :: PackageId -> CImportSpec -> CImportSpec
patchCImportSpec packageId spec
 = case spec of
        CFunction callTarget    -> CFunction $ patchCCallTarget packageId callTarget
        _                       -> spec

patchCCallTarget :: PackageId -> CCallTarget -> CCallTarget
patchCCallTarget packageId callTarget =
  case callTarget of
  StaticTarget label Nothing isFun -> StaticTarget label (Just packageId) isFun
  _                                -> callTarget


\end{code}


%*********************************************************
%*                                                      *
\subsection{Instance declarations}
%*                                                      *
%*********************************************************

\begin{code}
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
                           , cid_datafam_insts = adts })
        -- Used for both source and interface file decls
  = do { (inst_ty', inst_fvs) <- rnLHsInstType (text "In an instance declaration") inst_ty
       ; case splitLHsInstDeclTy_maybe inst_ty' of {
           Nothing -> return (ClsInstDecl { cid_poly_ty = inst_ty', cid_binds = emptyLHsBinds
                                          , cid_sigs = [], cid_tyfam_insts = []
                                          , cid_datafam_insts = [] }
                             , inst_fvs) ;
           Just (inst_tyvars, _, L _ cls,_) ->

    do { let (spec_inst_prags, other_sigs) = partition isSpecInstLSig uprags
             ktv_names = hsLKiTyVarNames inst_tyvars

       -- Rename the associated types, and type signatures
       -- Both need to have the instance type variables in scope
       ; traceRn (text "rnSrcInstDecl"  <+> ppr inst_ty' $$ ppr inst_tyvars $$ ppr ktv_names)
       ; ((ats', adts', other_sigs'), more_fvs) 
             <- extendTyVarEnvFVRn ktv_names $
                do { (ats', at_fvs) <- rnATInstDecls rnTyFamInstDecl cls inst_tyvars ats
                   ; (adts', adt_fvs) <- rnATInstDecls rnDataFamInstDecl cls inst_tyvars adts
                   ; (other_sigs', sig_fvs) <- renameSigs (InstDeclCtxt cls) other_sigs
                   ; return ( (ats', adts', other_sigs')
                            , at_fvs `plusFV` adt_fvs `plusFV` sig_fvs) }

        -- Rename the bindings
        -- The typechecker (not the renamer) checks that all
        -- the bindings are for the right class
        -- (Slightly strangely) when scoped type variables are on, the
        -- forall-d tyvars scope over the method bindings too
       ; (mbinds', meth_fvs) <- extendTyVarEnvForMethodBinds ktv_names $
                                rnMethodBinds cls (mkSigTvFn other_sigs')
                                                  mbinds

        -- Rename the SPECIALISE instance pramas
        -- Annoyingly the type variables are not in scope here,
        -- so that      instance Eq a => Eq (T a) where
        --                      {-# SPECIALISE instance Eq a => Eq (T [a]) #-}
        -- works OK. That's why we did the partition game above
        --
       ; (spec_inst_prags', spec_inst_fvs)
             <- renameSigs (InstDeclCtxt cls) spec_inst_prags

       ; let uprags' = spec_inst_prags' ++ other_sigs'
             all_fvs = meth_fvs `plusFV` more_fvs
                          `plusFV` spec_inst_fvs
                          `plusFV` inst_fvs
       ; return (ClsInstDecl { cid_poly_ty = inst_ty', cid_binds = mbinds'
                             , cid_sigs = uprags', cid_tyfam_insts = ats'
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
              -> RnM (Located Name, HsWithBndrs [LHsType Name], rhs', FreeVars)
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
                    ; let bad_tvs = case mb_cls of
                                      Nothing          -> []
                                      Just (_,cls_tvs) -> filter is_bad cls_tvs
                          is_bad tv = not (tv `elem` tv_names) && tv `elemNameSet` rhs_fvs

                    ; unless (null bad_tvs) (badAssocRhs bad_tvs)
                    ; return ((pats', payload'), rhs_fvs `plusFV` pat_fvs) }
                              

       ; let all_fvs = fvs `addOneFV` unLoc tycon'
       ; return (tycon',
                 HsWB { hswb_cts = pats', hswb_kvs = kv_names, hswb_tvs = tv_names },
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
rnTyFamInstEqn mb_cls (TyFamInstEqn { tfie_tycon = tycon
                                    , tfie_pats  = HsWB { hswb_cts = pats }
                                    , tfie_rhs   = rhs })
  = do { (tycon', pats', rhs', fvs) <-
           rnFamInstDecl (TySynCtx tycon) mb_cls tycon pats rhs rnTySyn
       ; return (TyFamInstEqn { tfie_tycon = tycon'
                              , tfie_pats  = pats'
                              , tfie_rhs   = rhs' }, fvs) }

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
\end{code}

Renaming of the associated types in instances.

\begin{code}
-- rename associated type family decl in class
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
    tv_ns = hsLTyVarNames hs_tvs
    -- Type variable binders (but NOT kind variables)
    -- See Note [Renaming associated types] in RnTypes
\end{code}

For the method bindings in class and instance decls, we extend the
type variable environment iff -fglasgow-exts

\begin{code}
extendTyVarEnvForMethodBinds :: [Name]
                             -> RnM (Bag (LHsBind Name), FreeVars)
                             -> RnM (Bag (LHsBind Name), FreeVars)
extendTyVarEnvForMethodBinds ktv_names thing_inside
  = do  { scoped_tvs <- xoptM Opt_ScopedTypeVariables
        ; if scoped_tvs then
                extendTyVarEnvFVRn ktv_names thing_inside
          else
                thing_inside }
\end{code}

%*********************************************************
%*                                                      *
\subsection{Stand-alone deriving declarations}
%*                                                      *
%*********************************************************

\begin{code}
rnSrcDerivDecl :: DerivDecl RdrName -> RnM (DerivDecl Name, FreeVars)
rnSrcDerivDecl (DerivDecl ty)
  = do { standalone_deriv_ok <- xoptM Opt_StandaloneDeriving
       ; unless standalone_deriv_ok (addErr standaloneDerivErr)
       ; (ty', fvs) <- rnLHsInstType (text "In a deriving declaration") ty
       ; return (DerivDecl ty', fvs) }

standaloneDerivErr :: SDoc
standaloneDerivErr
  = hang (ptext (sLit "Illegal standalone deriving declaration"))
       2 (ptext (sLit "Use StandaloneDeriving to enable this extension"))
\end{code}

%*********************************************************
%*                                                      *
\subsection{Rules}
%*                                                      *
%*********************************************************

\begin{code}
rnHsRuleDecl :: RuleDecl RdrName -> RnM (RuleDecl Name, FreeVars)
rnHsRuleDecl (HsRule rule_name act vars lhs _fv_lhs rhs _fv_rhs)
  = do { let rdr_names_w_loc = map get_var vars
       ; checkDupRdrNames rdr_names_w_loc
       ; checkShadowedRdrNames rdr_names_w_loc
       ; names <- newLocalBndrsRn rdr_names_w_loc
       ; bindHsRuleVars rule_name vars names $ \ vars' ->
    do { (lhs', fv_lhs') <- rnLExpr lhs
       ; (rhs', fv_rhs') <- rnLExpr rhs
       ; checkValidRule rule_name names lhs' fv_lhs'
       ; return (HsRule rule_name act vars' lhs' fv_lhs' rhs' fv_rhs',
                 fv_lhs' `plusFV` fv_rhs') } }
  where
    get_var (RuleBndrSig v _) = v
    get_var (RuleBndr v) = v

bindHsRuleVars :: RuleName -> [RuleBndr RdrName] -> [Name]
               -> ([RuleBndr Name] -> RnM (a, FreeVars))
               -> RnM (a, FreeVars)
bindHsRuleVars rule_name vars names thing_inside
  = go vars names $ \ vars' ->
    bindLocalNamesFV names (thing_inside vars')
  where
    doc = RuleCtx rule_name

    go (RuleBndr (L loc _) : vars) (n : ns) thing_inside
      = go vars ns $ \ vars' ->
        thing_inside (RuleBndr (L loc n) : vars')

    go (RuleBndrSig (L loc _) bsig : vars) (n : ns) thing_inside
      = rnHsBndrSig doc bsig $ \ bsig' ->
        go vars ns $ \ vars' ->
        thing_inside (RuleBndrSig (L loc n) bsig' : vars')

    go [] [] thing_inside = thing_inside []
    go vars names _ = pprPanic "bindRuleVars" (ppr vars $$ ppr names)
\end{code}

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

\begin{code}
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
-- Just e  => Not ok, and e is the offending expression
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
  = sep [ptext (sLit "Rule") <+> ftext name <> colon,
         nest 4 (vcat [ptext (sLit "Illegal expression:") <+> ppr bad_e,
                       ptext (sLit "in left-hand side:") <+> ppr lhs])]
    $$
    ptext (sLit "LHS must be of form (f e1 .. en) where f is not forall'd")
\end{code}


%*********************************************************
%*                                                      *
\subsection{Vectorisation declarations}
%*                                                      *
%*********************************************************

\begin{code}
rnHsVectDecl :: VectDecl RdrName -> RnM (VectDecl Name, FreeVars)
-- FIXME: For the moment, the right-hand side is restricted to be a variable as we cannot properly
--        typecheck a complex right-hand side without invoking 'vectType' from the vectoriser.
rnHsVectDecl (HsVect var rhs@(L _ (HsVar _)))
  = do { var' <- lookupLocatedOccRn var
       ; (rhs', fv_rhs) <- rnLExpr rhs
       ; return (HsVect var' rhs', fv_rhs `addOneFV` unLoc var')
       }
rnHsVectDecl (HsVect _var _rhs)
  = failWith $ vcat 
               [ ptext (sLit "IMPLEMENTATION RESTRICTION: right-hand side of a VECTORISE pragma")
               , ptext (sLit "must be an identifier")
               ]
rnHsVectDecl (HsNoVect var)
  = do { var' <- lookupLocatedTopBndrRn var           -- only applies to local (not imported) names
       ; return (HsNoVect var', unitFV (unLoc var'))
       }
rnHsVectDecl (HsVectTypeIn isScalar tycon Nothing)
  = do { tycon' <- lookupLocatedOccRn tycon
       ; return (HsVectTypeIn isScalar tycon' Nothing, unitFV (unLoc tycon'))
       }
rnHsVectDecl (HsVectTypeIn isScalar tycon (Just rhs_tycon))
  = do { tycon'     <- lookupLocatedOccRn tycon
       ; rhs_tycon' <- lookupLocatedOccRn rhs_tycon
       ; return ( HsVectTypeIn isScalar tycon' (Just rhs_tycon')
                , mkFVs [unLoc tycon', unLoc rhs_tycon'])
       }
rnHsVectDecl (HsVectTypeOut _ _ _)
  = panic "RnSource.rnHsVectDecl: Unexpected 'HsVectTypeOut'"
rnHsVectDecl (HsVectClassIn cls)
  = do { cls' <- lookupLocatedOccRn cls
       ; return (HsVectClassIn cls', unitFV (unLoc cls'))
       }
rnHsVectDecl (HsVectClassOut _)
  = panic "RnSource.rnHsVectDecl: Unexpected 'HsVectClassOut'"
rnHsVectDecl (HsVectInstIn instTy)
  = do { (instTy', fvs) <- rnLHsInstType (text "In a VECTORISE pragma") instTy
       ; return (HsVectInstIn instTy', fvs)
       }
rnHsVectDecl (HsVectInstOut _)
  = panic "RnSource.rnHsVectDecl: Unexpected 'HsVectInstOut'"
\end{code}

%*********************************************************
%*                                                      *
\subsection{Type, class and iface sig declarations}
%*                                                      *
%*********************************************************

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

  module A where
    import B
    data A1 = A1 B1

  module B where
    import {-# SOURCE #-} A
    type DisguisedA1 = A1
    data B1 = B1 DisguisedA1

We do not follow type synonyms when building the dependencies for each datatype,
so we will not find out that B1 really depends on A1 (which means it depends on
itself). To handle this problem, at the moment we add dependencies to everything
that comes from an .hs-boot file. But we don't add those dependencies to
everything. Imagine module B above had another datatype declaration:

  data B2 = B2 Int

Even though B2 has a dependency (on Int), all its dependencies are from things
that live on other packages. Since we don't have mutual dependencies across
packages, it is safe not to add the dependencies on the .hs-boot stuff to B2.

See also Note [Grouping of type and class declarations] in TcTyClsDecls.

\begin{code}
isInPackage :: PackageId -> Name -> Bool
isInPackage pkgId nm = case nameModule_maybe nm of
                         Nothing -> False
                         Just m  -> pkgId == modulePackageId m
-- We use nameModule_maybe because we might be in a TH splice, in which case
-- there is no module name. In that case we cannot have mutual dependencies,
-- so it's fine to return False here.

rnTyClDecls :: [Name] -> [TyClGroup RdrName]
            -> RnM ([TyClGroup Name], FreeVars)
-- Rename the declarations and do depedency analysis on them
rnTyClDecls extra_deps tycl_ds
  = do { ds_w_fvs <- mapM (wrapLocFstM rnTyClDecl) (tyClGroupConcat tycl_ds)
       ; role_annot_env <- rnRoleAnnots (concatMap group_roles tycl_ds)
       ; thisPkg  <- fmap thisPackage getDynFlags
       ; let add_boot_deps :: FreeVars -> FreeVars
             -- See Note [Extra dependencies from .hs-boot files]
             add_boot_deps fvs | any (isInPackage thisPkg) (nameSetToList fvs)
                               = fvs `plusFV` mkFVs extra_deps
                               | otherwise
                               = fvs

             ds_w_fvs' = mapSnd add_boot_deps ds_w_fvs

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
rnTyClDecl (ForeignType {tcdLName = name, tcdExtName = ext_name})
  = do { name' <- lookupLocatedTopBndrRn name
       ; return (ForeignType {tcdLName = name', tcdExtName = ext_name},
                 emptyFVs) }

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
       ; ((tyvars', defn'), fvs) <- bindHsTyVars doc Nothing kvs tyvars $ \ tyvars' ->
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
        ; ((tyvars', context', fds', ats', at_defs', sigs'), stuff_fvs)
            <- bindHsTyVars cls_doc Nothing kvs tyvars $ \ tyvars' -> do
                  -- Checks for distinct tyvars
             { (context', cxt_fvs) <- rnContext cls_doc context
             ; fds'  <- rnFds (docOfHsDocContext cls_doc) fds
                         -- The fundeps have no free variables
             ; (ats',     fv_ats)     <- rnATDecls cls' ats
             ; (at_defs', fv_at_defs) <- rnATInstDecls rnTyFamInstDecl cls' tyvars' at_defs
             ; (sigs', sig_fvs) <- renameSigs (ClsDeclCtxt cls') sigs
             ; let fvs = cxt_fvs     `plusFV`
                         sig_fvs     `plusFV`
                         fv_ats      `plusFV`
                         fv_at_defs
             ; return ((tyvars', context', fds', ats', at_defs', sigs'), fvs) }

        -- No need to check for duplicate associated type decls
        -- since that is done by RnNames.extendGlobalRdrEnvRn

        -- Check the signatures
        -- First process the class op sigs (op_sigs), then the fixity sigs (non_op_sigs).
        ; let sig_rdr_names_w_locs = [op | L _ (TypeSig ops _) <- sigs, op <- ops]
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
        ; (mbinds', meth_fvs)
            <- extendTyVarEnvForMethodBinds (hsLKiTyVarNames tyvars') $
                -- No need to check for duplicate method signatures
                -- since that is done by RnNames.extendGlobalRdrEnvRn
                -- and the methods are already in scope
                 rnMethodBinds cls' (mkSigTvFn sigs') mbinds

  -- Haddock docs
        ; docs' <- mapM (wrapLocM rnDocDecl) docs

        ; let all_fvs = meth_fvs `plusFV` stuff_fvs
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

-- Renames role annotations, returning them as the values in a NameEnv
-- and checks for duplicate role annotations.
-- It is quite convenient to do both of these in the same place.
-- See also Note [Role annotations in the renamer]
rnRoleAnnots :: [LRoleAnnotDecl RdrName]
                -> RnM (NameEnv (LRoleAnnotDecl Name))
rnRoleAnnots role_annots
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
      = do {  -- the name is an *occurrence*
             tycon' <- wrapLocM lookupGlobalOccRn tycon
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
        ; (condecls', con_fvs) <- zap_lcl_env $
                                  rnConDecls condecls
           -- No need to check for duplicate constructor decls
           -- since that is done by RnNames.extendGlobalRdrEnvRn

        ; let all_fvs = fvs1 `plusFV` fvs3 `plusFV`
                        con_fvs `plusFV` sig_fvs
        ; return ( HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                              , dd_ctxt = context', dd_kindSig = sig'
                              , dd_cons = condecls', dd_derivs = derivs' }
                 , all_fvs )
        }
  where
    h98_style = case condecls of         -- Note [Stupid theta]
                     L _ (ConDecl { con_res = ResTyGADT {} }) : _  -> False
                     _                                             -> True

    rn_derivs Nothing   = return (Nothing, emptyFVs)
    rn_derivs (Just ds) = do { (ds', fvs) <- rnLHsTypes doc ds
                             ; return (Just ds', fvs) }

badGadtStupidTheta :: HsDocContext -> SDoc
badGadtStupidTheta _
  = vcat [ptext (sLit "No context is allowed on a GADT-style data declaration"),
          ptext (sLit "(You can put a context on each contructor, though.)")]

rnFamDecl :: Maybe Name
                    -- Just cls => this FamilyDecl is nested 
                    --             inside an *class decl* for cls
                    --             used for associated types
          -> FamilyDecl RdrName
          -> RnM (FamilyDecl Name, FreeVars)
rnFamDecl mb_cls (FamilyDecl { fdLName = tycon, fdTyVars = tyvars
                             , fdInfo = info, fdKindSig = kind })
  = do { ((tycon', tyvars', kind'), fv1) <-
           bindHsTyVars fmly_doc mb_cls kvs tyvars $ \tyvars' ->
           do { tycon' <- lookupLocatedTopBndrRn tycon
              ; (kind', fv_kind) <- rnLHsMaybeKind fmly_doc kind
              ; return ((tycon', tyvars', kind'), fv_kind) }
       ; (info', fv2) <- rn_info info
       ; return (FamilyDecl { fdLName = tycon', fdTyVars = tyvars'
                            , fdInfo = info', fdKindSig = kind' }
                , fv1 `plusFV` fv2) }
  where 
     fmly_doc = TyFamilyCtx tycon
     kvs = extractRdrKindSigVars kind

     rn_info (ClosedTypeFamily eqns)
       = do { (eqns', fvs) <- rnList (rnTyFamInstEqn Nothing) eqns
                                                    -- no class context,
            ; return (ClosedTypeFamily eqns', fvs) }
     rn_info OpenTypeFamily = return (OpenTypeFamily, emptyFVs)
     rn_info DataFamily     = return (DataFamily, emptyFVs)
     
\end{code}

Note [Stupid theta]
~~~~~~~~~~~~~~~~~~~
Trac #3850 complains about a regression wrt 6.10 for
     data Show a => T a
There is no reason not to allow the stupid theta if there are no data
constructors.  It's still stupid, but does no harm, and I don't want
to cause programs to break unnecessarily (notably HList).  So if there
are no data constructors we allow h98_style = True


\begin{code}
depAnalTyClDecls :: [(LTyClDecl Name, FreeVars)] -> [SCC (LTyClDecl Name)]
-- See Note [Dependency analysis of type and class decls]
depAnalTyClDecls ds_w_fvs
  = stronglyConnCompFromEdgedVertices edges
  where
    edges = [ (d, tcdName (unLoc d), map get_parent (nameSetToList fvs))
            | (d, fvs) <- ds_w_fvs ]

    -- We also need to consider data constructor names since
    -- they may appear in types because of promotion.
    get_parent n = lookupNameEnv assoc_env n `orElse` n

    assoc_env :: NameEnv Name   -- Maps a data constructor back
                                -- to its parent type constructor
    assoc_env = mkNameEnv assoc_env_list
    assoc_env_list = do
      (L _ d, _) <- ds_w_fvs
      case d of
        ClassDecl { tcdLName = L _ cls_name
                  , tcdATs = ats } 
          -> do L _ (FamilyDecl { fdLName = L _ fam_name }) <- ats
                return (fam_name, cls_name)
        DataDecl { tcdLName = L _ data_name
                 , tcdDataDefn = HsDataDefn { dd_cons = cons } } 
          -> do L _ dc <- cons
                return (unLoc (con_name dc), data_name)
        _ -> []
\end{code}

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

%*********************************************************
%*                                                      *
\subsection{Support code for type/data declarations}
%*                                                      *
%*********************************************************

\begin{code}
---------------
badAssocRhs :: [Name] -> RnM ()
badAssocRhs ns
  = addErr (hang (ptext (sLit "The RHS of an associated type declaration mentions type variable")
                  <> plural ns
                  <+> pprWithCommas (quotes . ppr) ns)
               2 (ptext (sLit "All such variables must be bound on the LHS")))

-----------------
rnConDecls :: [LConDecl RdrName] -> RnM ([LConDecl Name], FreeVars)
rnConDecls = mapFvRn (wrapLocFstM rnConDecl)

rnConDecl :: ConDecl RdrName -> RnM (ConDecl Name, FreeVars)
rnConDecl decl@(ConDecl { con_name = name, con_qvars = tvs
                        , con_cxt = lcxt@(L loc cxt), con_details = details
                        , con_res = res_ty, con_doc = mb_doc
                        , con_old_rec = old_rec, con_explicit = expl })
  = do  { addLocM checkConName name
        ; when old_rec (addWarn (deprecRecSyntax decl))
        ; new_name <- lookupLocatedTopBndrRn name

           -- For H98 syntax, the tvs are the existential ones
           -- For GADT syntax, the tvs are all the quantified tyvars
           -- Hence the 'filter' in the ResTyH98 case only
        ; rdr_env <- getLocalRdrEnv
        ; let arg_tys    = hsConDeclArgTys details
              (free_kvs, free_tvs) = case res_ty of
                                     ResTyH98 -> filterInScope rdr_env (get_rdr_tvs arg_tys)
                                     ResTyGADT ty -> get_rdr_tvs (ty : arg_tys)

         -- With an Explicit forall, check for unused binders
         -- With Implicit, find the mentioned ones, and use them as binders
        ; new_tvs <- case expl of
                       Implicit -> return (mkHsQTvs (userHsTyVarBndrs loc free_tvs))
                       Explicit -> do { warnUnusedForAlls (docOfHsDocContext doc) tvs free_tvs
                                      ; return tvs }

        ; mb_doc' <- rnMbLHsDoc mb_doc

        ; bindHsTyVars doc Nothing free_kvs new_tvs $ \new_tyvars -> do
        { (new_context, fvs1) <- rnContext doc lcxt
        ; (new_details, fvs2) <- rnConDeclDetails doc details
        ; (new_details', new_res_ty, fvs3) <- rnConResult doc (unLoc new_name) new_details res_ty
        ; return (decl { con_name = new_name, con_qvars = new_tyvars, con_cxt = new_context
                       , con_details = new_details', con_res = new_res_ty, con_doc = mb_doc' },
                  fvs1 `plusFV` fvs2 `plusFV` fvs3) }}
 where
    doc = ConDeclCtx name
    get_rdr_tvs tys = extractHsTysRdrTyVars (cxt ++ tys)

rnConResult :: HsDocContext -> Name
            -> HsConDetails (LHsType Name) [ConDeclField Name]
            -> ResType (LHsType RdrName)
            -> RnM (HsConDetails (LHsType Name) [ConDeclField Name],
                    ResType (LHsType Name), FreeVars)
rnConResult _   _   details ResTyH98 = return (details, ResTyH98, emptyFVs)
rnConResult doc con details (ResTyGADT ty)
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
                              ; return (details, ResTyGADT res_ty, fvs) }

           PrefixCon {} | isSymOcc (getOccName con)  -- See Note [Infix GADT cons]
                        , [ty1,ty2] <- arg_tys
                        -> do { fix_env <- getFixityEnv
                              ; return (if   con `elemNameEnv` fix_env
                                        then InfixCon ty1 ty2
                                        else PrefixCon arg_tys
                                       , ResTyGADT res_ty, fvs) }
                        | otherwise
                        -> return (PrefixCon arg_tys, ResTyGADT res_ty, fvs) }

rnConDeclDetails :: HsDocContext
                 -> HsConDetails (LHsType RdrName) [ConDeclField RdrName]
                 -> RnM (HsConDetails (LHsType Name) [ConDeclField Name], FreeVars)
rnConDeclDetails doc (PrefixCon tys)
  = do { (new_tys, fvs) <- rnLHsTypes doc tys
       ; return (PrefixCon new_tys, fvs) }

rnConDeclDetails doc (InfixCon ty1 ty2)
  = do { (new_ty1, fvs1) <- rnLHsType doc ty1
       ; (new_ty2, fvs2) <- rnLHsType doc ty2
       ; return (InfixCon new_ty1 new_ty2, fvs1 `plusFV` fvs2) }

rnConDeclDetails doc (RecCon fields)
  = do  { (new_fields, fvs) <- rnConDeclFields doc fields
                -- No need to check for duplicate fields
                -- since that is done by RnNames.extendGlobalRdrEnvRn
        ; return (RecCon new_fields, fvs) }

-------------------------------------------------
deprecRecSyntax :: ConDecl RdrName -> SDoc
deprecRecSyntax decl
  = vcat [ ptext (sLit "Declaration of") <+> quotes (ppr (con_name decl))
                 <+> ptext (sLit "uses deprecated syntax")
         , ptext (sLit "Instead, use the form")
         , nest 2 (ppr decl) ]   -- Pretty printer uses new form

badRecResTy :: SDoc -> SDoc
badRecResTy doc = ptext (sLit "Malformed constructor signature") $$ doc

-- This data decl will parse OK
--      data T = a Int
-- treating "a" as the constructor.
-- It is really hard to make the parser spot this malformation.
-- So the renamer has to check that the constructor is legal
--
-- We can get an operator as the constructor, even in the prefix form:
--      data T = :% Int Int
-- from interface files, which always print in prefix form

checkConName :: RdrName -> TcRn ()
checkConName name = checkErr (isRdrDataCon name) (badDataCon name)

badDataCon :: RdrName -> SDoc
badDataCon name
   = hsep [ptext (sLit "Illegal data constructor name"), quotes (ppr name)]
\end{code}

Note [Infix GADT constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not currently have syntax to declare an infix constructor in GADT syntax,
but it makes a (small) difference to the Show instance.  So as a slightly
ad-hoc solution, we regard a GADT data constructor as infix if
  a) it is an operator symbol
  b) it has two arguments
  c) there is a fixity declaration for it
For example:
   infix 6 (:--:)
   data T a where
     (:--:) :: t1 -> t2 -> T Int

%*********************************************************
%*                                                      *
\subsection{Support code for type/data declarations}
%*                                                      *
%*********************************************************

Get the mapping from constructors to fields for this module.
It's convenient to do this after the data type decls have been renamed
\begin{code}
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
    all_ty_defs = [ defn | L _ (DataDecl { tcdDataDefn = defn }) <- tyClGroupConcat tycl_decls ]
               ++ map dfid_defn (instDeclDataFamInsts inst_decls)  -- Do not forget associated types!

    get_con (ConDecl { con_name = con, con_details = RecCon flds })
            (RecFields env fld_set)
        = do { con' <- lookup con
             ; flds' <- mapM lookup (map cd_fld_name flds)
             ; let env'    = extendNameEnv env con' flds'
                   fld_set' = addListToNameSet fld_set flds'
             ; return $ (RecFields env' fld_set') }
    get_con _ env = return env
\end{code}

%*********************************************************
%*                                                      *
\subsection{Support code to rename types}
%*                                                      *
%*********************************************************

\begin{code}
rnFds :: SDoc -> [Located (FunDep RdrName)] -> RnM [Located (FunDep Name)]

rnFds doc fds
  = mapM (wrapLocM rn_fds) fds
  where
    rn_fds (tys1, tys2)
      = do { tys1' <- rnHsTyVars doc tys1
           ; tys2' <- rnHsTyVars doc tys2
           ; return (tys1', tys2') }

rnHsTyVars :: SDoc -> [RdrName] -> RnM [Name]
rnHsTyVars doc tvs  = mapM (rnHsTyVar doc) tvs

rnHsTyVar :: SDoc -> RdrName -> RnM Name
rnHsTyVar _doc tyvar = lookupOccRn tyvar
\end{code}


%*********************************************************
%*                                                      *
        findSplice
%*                                                      *
%*********************************************************

This code marches down the declarations, looking for the first
Template Haskell splice.  As it does so it
        a) groups the declarations into a HsGroup
        b) runs any top-level quasi-quotes

\begin{code}
findSplice :: [LHsDecl RdrName] -> RnM (HsGroup RdrName, Maybe (SpliceDecl RdrName, [LHsDecl RdrName]))
findSplice ds = addl emptyRdrGroup ds

addl :: HsGroup RdrName -> [LHsDecl RdrName]
     -> RnM (HsGroup RdrName, Maybe (SpliceDecl RdrName, [LHsDecl RdrName]))
-- This stuff reverses the declarations (again) but it doesn't matter
addl gp []           = return (gp, Nothing)
addl gp (L l d : ds) = add gp l d ds


add :: HsGroup RdrName -> SrcSpan -> HsDecl RdrName -> [LHsDecl RdrName]
    -> RnM (HsGroup RdrName, Maybe (SpliceDecl RdrName, [LHsDecl RdrName]))

add gp loc (SpliceD splice@(SpliceDecl _ flag)) ds
  = do { -- We've found a top-level splice.  If it is an *implicit* one
         -- (i.e. a naked top level expression)
         case flag of
           Explicit -> return ()
           Implicit -> do { th_on <- xoptM Opt_TemplateHaskell
                          ; unless th_on $ setSrcSpan loc $
                            failWith badImplicitSplice }

       ; return (gp, Just (splice, ds)) }
  where
    badImplicitSplice = ptext (sLit "Parse error: naked expression at top level")
                     $$ ptext (sLit "Perhaps you intended to use TemplateHaskell")

add gp _ (QuasiQuoteD qq) ds            -- Expand quasiquotes
  = do { ds' <- runQuasiQuoteDecl qq
       ; addl gp (ds' ++ ds) }

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
\end{code}

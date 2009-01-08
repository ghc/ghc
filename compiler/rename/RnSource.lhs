%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnSource ( 
	rnSrcDecls, addTcgDUs, 
	rnTyClDecls, 
	rnSplice, checkTH
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} RnExpr( rnLExpr )

import HsSyn
import RdrName		( RdrName, isRdrDataCon, elemLocalRdrEnv, 
			  globalRdrEnvElts, GlobalRdrElt(..), isLocalGRE, rdrNameOcc )
import RdrHsSyn		( extractGenericPatTyVars, extractHsRhoRdrTyVars )
import RnHsSyn
import RnTypes		( rnLHsType, rnLHsTypes, rnHsSigType, rnHsTypeFVs, rnContext )
import RnBinds		( rnTopBindsLHS, rnTopBindsRHS, rnMethodBinds, renameSigs, mkSigTvFn,
                                makeMiniFixityEnv)
import RnEnv		( lookupLocalDataTcNames, lookupLocatedOccRn,
			  lookupTopBndrRn, lookupLocatedTopBndrRn,
			  lookupOccRn, newLocalsRn, 
			  bindLocatedLocalsFV, bindPatSigTyVarsFV,
			  bindTyVarsRn, extendTyVarEnvFVRn,
			  bindLocalNames, checkDupRdrNames, mapFvRn,
			)
import RnNames       	( getLocalNonValBinders, extendGlobalRdrEnvRn )
import HscTypes      	( GenAvailInfo(..), availsToNameSet )
import RnHsDoc          ( rnHsDoc, rnMbLHsDoc )
import TcRnMonad

import HscTypes		( Warnings(..), plusWarns )
import Class		( FunDep )
import Name		( Name, nameOccName )
import NameSet
import NameEnv
import OccName 
import Outputable
import Bag
import FastString
import SrcLoc
import DynFlags	( DynFlag(..) )
import Maybe            ( isNothing )
import BasicTypes       ( Boxity(..) )

import ListSetOps    (findDupsEq)
import List

import Control.Monad
\end{code}

\begin{code}
-- XXX
thenM :: Monad a => a b -> (b -> a c) -> a c
thenM = (>>=)

thenM_ :: Monad a => a b -> a c -> a c
thenM_ = (>>)

returnM :: Monad m => a -> m a
returnM = return

mappM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mappM = mapM

mappM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mappM_ = mapM_

checkM :: Monad m => Bool -> m () -> m ()
checkM = unless
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
rnSrcDecls :: HsGroup RdrName -> RnM (TcGblEnv, HsGroup Name)
-- Rename a HsGroup; used for normal source files *and* hs-boot files
rnSrcDecls group@(HsGroup {hs_valds  = val_decls,
                                   hs_tyclds = tycl_decls,
                                   hs_instds = inst_decls,
                                   hs_derivds = deriv_decls,
                                   hs_fixds  = fix_decls,
                                   hs_warnds  = warn_decls,
                                   hs_annds  = ann_decls,
                                   hs_fords  = foreign_decls,
                                   hs_defds  = default_decls,
                                   hs_ruleds = rule_decls,
                                   hs_docs   = docs })
 = do {
   -- (A) Process the fixity declarations, creating a mapping from
   --     FastStrings to FixItems.
   --     Also checks for duplcates.
   local_fix_env <- makeMiniFixityEnv fix_decls;

   -- (B) Bring top level binders (and their fixities) into scope,
   --     *except* for the value bindings, which get brought in below.
   --     However *do* include class ops, data constructors
   --     And for hs-boot files *do* include the value signatures
   tc_avails <- getLocalNonValBinders group ;
   tc_envs <- extendGlobalRdrEnvRn tc_avails local_fix_env ;
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
   let { val_binders = map unLoc $ collectHsValBinders new_lhs ;
	 val_bndr_set = mkNameSet val_binders ;
	 all_bndr_set = val_bndr_set `unionNameSets` availsToNameSet tc_avails ;
         val_avails = map Avail val_binders 
       } ;
   (tcg_env, tcl_env) <- extendGlobalRdrEnvRn val_avails local_fix_env ;
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
   (rn_tycl_decls, src_fvs1) <- rnList rnTyClDecl tycl_decls ;

   -- (F) Rename Value declarations right-hand sides
   traceRn (text "Start rnmono") ;
   (rn_val_decls, bind_dus) <- rnTopBindsRHS val_bndr_set new_lhs ;
   traceRn (text "finish rnmono" <+> ppr rn_val_decls) ;

   -- (G) Rename Fixity and deprecations
   
   -- Rename fixity declarations and error if we try to
   -- fix something from another module (duplicates were checked in (A))
   rn_fix_decls <- rnSrcFixityDecls all_bndr_set fix_decls ;

   -- Rename deprec decls;
   -- check for duplicates and ensure that deprecated things are defined locally
   -- at the moment, we don't keep these around past renaming
   rn_warns <- rnSrcWarnDecls all_bndr_set warn_decls ;

   -- (H) Rename Everything else

   (rn_inst_decls,    src_fvs2) <- rnList rnSrcInstDecl   inst_decls ;
   (rn_rule_decls,    src_fvs3) <- setOptM Opt_ScopedTypeVariables $
   		      		   rnList rnHsRuleDecl    rule_decls ;
			   -- Inside RULES, scoped type variables are on
   (rn_foreign_decls, src_fvs4) <- rnList rnHsForeignDecl foreign_decls ;
   (rn_ann_decls,     src_fvs5) <- rnList rnAnnDecl       ann_decls ;
   (rn_default_decls, src_fvs6) <- rnList rnDefaultDecl   default_decls ;
   (rn_deriv_decls,   src_fvs7) <- rnList rnSrcDerivDecl  deriv_decls ;
      -- Haddock docs; no free vars
   rn_docs <- mapM (wrapLocM rnDocDecl) docs ;

   -- (I) Compute the results and return
   let {rn_group = HsGroup { hs_valds  = rn_val_decls,
			     hs_tyclds = rn_tycl_decls,
			     hs_instds = rn_inst_decls,
                             hs_derivds = rn_deriv_decls,
			     hs_fixds  = rn_fix_decls,
			     hs_warnds = [], -- warns are returned in the tcg_env
	                                     -- (see below) not in the HsGroup
			     hs_fords  = rn_foreign_decls,
			     hs_annds   = rn_ann_decls,
			     hs_defds  = rn_default_decls,
			     hs_ruleds = rn_rule_decls,
                             hs_docs   = rn_docs } ;

	other_fvs = plusFVs [src_fvs1, src_fvs2, src_fvs3, src_fvs4, 
			     src_fvs5, src_fvs6, src_fvs7] ;
	src_dus = bind_dus `plusDU` usesOnly other_fvs;
		-- Note: src_dus will contain *uses* for locally-defined types
		-- and classes, but no *defs* for them.  (Because rnTyClDecl 
		-- returns only the uses.)  This is a little 
		-- surprising but it doesn't actually matter at all.

       final_tcg_env = let tcg_env' = (tcg_env `addTcgDUs` src_dus)
                       in -- we return the deprecs in the env, not in the HsGroup above
                         tcg_env' { tcg_warns = tcg_warns tcg_env' `plusWarns` rn_warns };
       } ;

   traceRn (text "finish rnSrc" <+> ppr rn_group) ;
   traceRn (text "finish Dus" <+> ppr src_dus ) ;
   return (final_tcg_env , rn_group)
                    }}}}

-- some utils because we do this a bunch above
-- compute and install the new env
inNewEnv :: TcM TcGblEnv -> (TcGblEnv -> TcM a) -> TcM a
inNewEnv env cont = do e <- env
                       setGblEnv e $ cont e

rnTyClDecls :: [LTyClDecl RdrName] -> RnM [LTyClDecl Name]
-- Used for external core
rnTyClDecls tycl_decls = do  (decls', _fvs) <- rnList rnTyClDecl tycl_decls
			     return decls'

addTcgDUs :: TcGblEnv -> DefUses -> TcGblEnv 
addTcgDUs tcg_env dus = tcg_env { tcg_dus = tcg_dus tcg_env `plusDU` dus }

rnList :: (a -> RnM (b, FreeVars)) -> [Located a] -> RnM ([Located b], FreeVars)
rnList f xs = mapFvRn (wrapLocFstM f) xs
\end{code}


%*********************************************************
%*						 	 *
	HsDoc stuff
%*							 *
%*********************************************************

\begin{code}
rnDocDecl :: DocDecl RdrName -> RnM (DocDecl Name)
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
%*						 	 *
	Source-code fixity declarations
%*							 *
%*********************************************************

\begin{code}
rnSrcFixityDecls :: NameSet -> [LFixitySig RdrName] -> RnM [LFixitySig Name]
-- Rename the fixity decls, so we can put
-- the renamed decls in the renamed syntax tree
-- Errors if the thing being fixed is not defined locally.
--
-- The returned FixitySigs are not actually used for anything,
-- except perhaps the GHCi API
rnSrcFixityDecls bound_names fix_decls
  = do fix_decls <- mapM rn_decl fix_decls
       return (concat fix_decls)
  where
    rn_decl :: LFixitySig RdrName -> RnM [LFixitySig Name]
        -- GHC extension: look up both the tycon and data con 
	-- for con-like things; hence returning a list
	-- If neither are in scope, report an error; otherwise
	-- return a fixity sig for each (slightly odd)
    rn_decl (L loc (FixitySig (L name_loc rdr_name) fixity))
      = setSrcSpan name_loc $
                    -- this lookup will fail if the definition isn't local
        do names <- lookupLocalDataTcNames bound_names what rdr_name
           return [ L loc (FixitySig (L name_loc name) fixity)
                  | name <- names ]
    what = ptext (sLit "fixity signature")
\end{code}


%*********************************************************
%*						 	 *
	Source-code deprecations declarations
%*							 *
%*********************************************************

Check that the deprecated names are defined, are defined locally, and
that there are no duplicate deprecations.

It's only imported deprecations, dealt with in RnIfaces, that we
gather them together.

\begin{code}
-- checks that the deprecations are defined locally, and that there are no duplicates
rnSrcWarnDecls :: NameSet -> [LWarnDecl RdrName] -> RnM Warnings
rnSrcWarnDecls _bound_names [] 
  = returnM NoWarnings

rnSrcWarnDecls bound_names decls 
  = do { -- check for duplicates
       ; mappM_ (\ (lrdr:lrdr':_) -> addLocErr lrdr (dupWarnDecl lrdr')) warn_rdr_dups
       ; mappM (addLocM rn_deprec) decls	`thenM` \ pairs_s ->
         returnM (WarnSome ((concat pairs_s))) }
 where
   rn_deprec (Warning rdr_name txt)
       -- ensures that the names are defined locally
     = lookupLocalDataTcNames bound_names what rdr_name	`thenM` \ names ->
       returnM [(nameOccName name, txt) | name <- names]
   
   what = ptext (sLit "deprecation")

   -- look for duplicates among the OccNames;
   -- we check that the names are defined above
   -- invt: the lists returned by findDupsEq always have at least two elements
   warn_rdr_dups = findDupsEq (\ x -> \ y -> rdrNameOcc (unLoc x) == rdrNameOcc (unLoc y))
                     (map (\ (L loc (Warning rdr_name _)) -> L loc rdr_name) decls)
               
dupWarnDecl :: Located RdrName -> RdrName -> SDoc
-- Located RdrName -> DeprecDecl RdrName -> SDoc
dupWarnDecl (L loc _) rdr_name
  = vcat [ptext (sLit "Multiple warning declarations for") <+> quotes (ppr rdr_name),
          ptext (sLit "also at ") <+> ppr loc]

\end{code}

%*********************************************************
%*							*
\subsection{Annotation declarations}
%*							*
%*********************************************************

\begin{code}
rnAnnDecl :: AnnDecl RdrName -> RnM (AnnDecl Name, FreeVars)
rnAnnDecl (HsAnnotation provenance expr) = do
    (provenance', provenance_fvs) <- rnAnnProvenance provenance
    (expr', expr_fvs) <- rnLExpr expr
    return (HsAnnotation provenance' expr', provenance_fvs `plusFV` expr_fvs)

rnAnnProvenance :: AnnProvenance RdrName -> RnM (AnnProvenance Name, FreeVars)
rnAnnProvenance provenance = do
    provenance' <- modifyAnnProvenanceNameM lookupTopBndrRn provenance
    return (provenance', maybe emptyFVs unitFV (annProvenanceName_maybe provenance'))
\end{code}

%*********************************************************
%*							*
\subsection{Default declarations}
%*							*
%*********************************************************

\begin{code}
rnDefaultDecl :: DefaultDecl RdrName -> RnM (DefaultDecl Name, FreeVars)
rnDefaultDecl (DefaultDecl tys)
  = mapFvRn (rnHsTypeFVs doc_str) tys	`thenM` \ (tys', fvs) ->
    returnM (DefaultDecl tys', fvs)
  where
    doc_str = text "In a `default' declaration"
\end{code}

%*********************************************************
%*							*
\subsection{Foreign declarations}
%*							*
%*********************************************************

\begin{code}
rnHsForeignDecl :: ForeignDecl RdrName -> RnM (ForeignDecl Name, FreeVars)
rnHsForeignDecl (ForeignImport name ty spec)
  = lookupLocatedTopBndrRn name	        `thenM` \ name' ->
    rnHsTypeFVs (fo_decl_msg name) ty	`thenM` \ (ty', fvs) ->
    returnM (ForeignImport name' ty' spec, fvs)

rnHsForeignDecl (ForeignExport name ty spec)
  = lookupLocatedOccRn name	        `thenM` \ name' ->
    rnHsTypeFVs (fo_decl_msg name) ty  	`thenM` \ (ty', fvs) ->
    returnM (ForeignExport name' ty' spec, fvs `addOneFV` unLoc name')
	-- NB: a foreign export is an *occurrence site* for name, so 
	--     we add it to the free-variable list.  It might, for example,
	--     be imported from another module

fo_decl_msg :: Located RdrName -> SDoc
fo_decl_msg name = ptext (sLit "In the foreign declaration for") <+> ppr name
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************

\begin{code}
rnSrcInstDecl :: InstDecl RdrName -> RnM (InstDecl Name, FreeVars)
rnSrcInstDecl (InstDecl inst_ty mbinds uprags ats)
	-- Used for both source and interface file decls
  = rnHsSigType (text "an instance decl") inst_ty	`thenM` \ inst_ty' ->

	-- Rename the bindings
	-- The typechecker (not the renamer) checks that all 
	-- the bindings are for the right class
    let
	meth_doc    = text "In the bindings in an instance declaration"
	meth_names  = collectHsBindLocatedBinders mbinds
	(inst_tyvars, _, cls,_) = splitHsInstDeclTy (unLoc inst_ty')
    in
    checkDupRdrNames meth_doc meth_names 	`thenM_`
	-- Check that the same method is not given twice in the
	-- same instance decl	instance C T where
	--			      f x = ...
	--			      g y = ...
	--			      f x = ...
	-- We must use checkDupRdrNames because the Name of the
	-- method is the Name of the class selector, whose SrcSpan
	-- points to the class declaration

    extendTyVarEnvForMethodBinds inst_tyvars (		
	-- (Slightly strangely) the forall-d tyvars scope over
	-- the method bindings too
	rnMethodBinds cls (\_ -> []) 	-- No scoped tyvars
		      [] mbinds
    )						`thenM` \ (mbinds', meth_fvs) ->
	-- Rename the associated types
	-- The typechecker (not the renamer) checks that all 
	-- the declarations are for the right class
    let
	at_doc   = text "In the associated types of an instance declaration"
	at_names = map (head . tyClDeclNames . unLoc) ats
    in
    checkDupRdrNames at_doc at_names		`thenM_`
	-- See notes with checkDupRdrNames for methods, above

    rnATInsts ats				`thenM` \ (ats', at_fvs) ->

	-- Rename the prags and signatures.
	-- Note that the type variables are not in scope here,
	-- so that	instance Eq a => Eq (T a) where
	--			{-# SPECIALISE instance Eq a => Eq (T [a]) #-}
	-- works OK. 
	--
	-- But the (unqualified) method names are in scope
    let 
	binders = collectHsBindBinders mbinds'
	bndr_set = mkNameSet binders
    in
    bindLocalNames binders 
	(renameSigs (Just bndr_set) okInstDclSig uprags)	`thenM` \ uprags' ->

    returnM (InstDecl inst_ty' mbinds' uprags' ats',
	     meth_fvs `plusFV` at_fvs
		      `plusFV` hsSigsFVs uprags'
		      `plusFV` extractHsTyNames inst_ty')
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
\end{code}

Renaming of the associated types in instances.  

\begin{code}
rnATInsts :: [LTyClDecl RdrName] -> RnM ([LTyClDecl Name], FreeVars)
rnATInsts atDecls = rnList rnATInst atDecls
  where
    rnATInst tydecl@TyData     {} = rnTyClDecl tydecl
    rnATInst tydecl@TySynonym  {} = rnTyClDecl tydecl
    rnATInst tydecl               =
      pprPanic "RnSource.rnATInsts: invalid AT instance" 
	       (ppr (tcdName tydecl))
\end{code}

For the method bindings in class and instance decls, we extend the 
type variable environment iff -fglasgow-exts

\begin{code}
extendTyVarEnvForMethodBinds :: [LHsTyVarBndr Name]
                             -> RnM (Bag (LHsBind Name), FreeVars)
                             -> RnM (Bag (LHsBind Name), FreeVars)
extendTyVarEnvForMethodBinds tyvars thing_inside
  = do	{ scoped_tvs <- doptM Opt_ScopedTypeVariables
	; if scoped_tvs then
		extendTyVarEnvFVRn (map hsLTyVarName tyvars) thing_inside
	  else
		thing_inside }
\end{code}

%*********************************************************
%*							*
\subsection{Stand-alone deriving declarations}
%*							*
%*********************************************************

\begin{code}
rnSrcDerivDecl :: DerivDecl RdrName -> RnM (DerivDecl Name, FreeVars)
rnSrcDerivDecl (DerivDecl ty)
  = do ty' <- rnLHsType (text "a deriving decl") ty
       let fvs = extractHsTyNames ty'
       return (DerivDecl ty', fvs)
\end{code}

%*********************************************************
%*							*
\subsection{Rules}
%*							*
%*********************************************************

\begin{code}
rnHsRuleDecl :: RuleDecl RdrName -> RnM (RuleDecl Name, FreeVars)
rnHsRuleDecl (HsRule rule_name act vars lhs _fv_lhs rhs _fv_rhs)
  = bindPatSigTyVarsFV (collectRuleBndrSigTys vars)	$
    bindLocatedLocalsFV doc (map get_var vars)		$ \ ids ->
    do	{ (vars', fv_vars) <- mapFvRn rn_var (vars `zip` ids)
		-- NB: The binders in a rule are always Ids
		--     We don't (yet) support type variables

	; (lhs', fv_lhs') <- rnLExpr lhs
	; (rhs', fv_rhs') <- rnLExpr rhs

	; checkValidRule rule_name ids lhs' fv_lhs'

	; return (HsRule rule_name act vars' lhs' fv_lhs' rhs' fv_rhs',
		  fv_vars `plusFV` fv_lhs' `plusFV` fv_rhs') }
  where
    doc = text "In the transformation rule" <+> ftext rule_name
  
    get_var (RuleBndr v)      = v
    get_var (RuleBndrSig v _) = v

    rn_var (RuleBndr (L loc _), id)
	= returnM (RuleBndr (L loc id), emptyFVs)
    rn_var (RuleBndrSig (L loc _) t, id)
	= rnHsTypeFVs doc t	`thenM` \ (t', fvs) ->
	  returnM (RuleBndrSig (L loc id) t', fvs)

badRuleVar :: FastString -> Name -> SDoc
badRuleVar name var
  = sep [ptext (sLit "Rule") <+> doubleQuotes (ftext name) <> colon,
	 ptext (sLit "Forall'd variable") <+> quotes (ppr var) <+> 
		ptext (sLit "does not appear on left hand side")]
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
  = do 	{ 	-- Check for the form of the LHS
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

    check (OpApp e1 op _ e2)		  = checkl op `mplus` checkl_e e1 `mplus` checkl_e e2
    check (HsApp e1 e2) 		  = checkl e1 `mplus` checkl_e e2
    check (HsVar v) | v `notElem` foralls = Nothing
    check other				  = Just other 	-- Failure

	-- Check an argument
    checkl_e (L _ _e) = Nothing 	-- Was (check_e e); see Note [Rule LHS validity checking]

{-	Commented out; see Note [Rule LHS validity checking] above 
    check_e (HsVar v)     = Nothing
    check_e (HsPar e) 	  = checkl_e e
    check_e (HsLit e) 	  = Nothing
    check_e (HsOverLit e) = Nothing

    check_e (OpApp e1 op _ e2) 	 = checkl_e e1 `mplus` checkl_e op `mplus` checkl_e e2
    check_e (HsApp e1 e2)      	 = checkl_e e1 `mplus` checkl_e e2
    check_e (NegApp e _)       	 = checkl_e e
    check_e (ExplicitList _ es)	 = checkl_es es
    check_e (ExplicitTuple es _) = checkl_es es
    check_e other		 = Just other	-- Fails

    checkl_es es = foldr (mplus . checkl_e) Nothing es
-}

badRuleLhsErr :: FastString -> LHsExpr Name -> HsExpr Name -> SDoc
badRuleLhsErr name lhs bad_e
  = sep [ptext (sLit "Rule") <+> ftext name <> colon,
	 nest 4 (vcat [ptext (sLit "Illegal expression:") <+> ppr bad_e, 
		       ptext (sLit "in left-hand side:") <+> ppr lhs])]
    $$
    ptext (sLit "LHS must be of form (f e1 .. en) where f is not forall'd")
\end{code}


%*********************************************************
%*							*
\subsection{Type, class and iface sig declarations}
%*							*
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

\begin{code}
rnTyClDecl :: TyClDecl RdrName -> RnM (TyClDecl Name, FreeVars)
rnTyClDecl (ForeignType {tcdLName = name, tcdFoType = fo_type, tcdExtName = ext_name})
  = lookupLocatedTopBndrRn name		`thenM` \ name' ->
    returnM (ForeignType {tcdLName = name', tcdFoType = fo_type, tcdExtName = ext_name},
	     emptyFVs)

-- all flavours of type family declarations ("type family", "newtype fanily",
-- and "data family")
rnTyClDecl (tydecl@TyFamily {}) =
  rnFamily tydecl bindTyVarsRn

-- "data", "newtype", "data instance, and "newtype instance" declarations
rnTyClDecl (tydecl@TyData {tcdND = new_or_data, tcdCtxt = context, 
			   tcdLName = tycon, tcdTyVars = tyvars, 
			   tcdTyPats = typatsMaybe, tcdCons = condecls, 
			   tcdKindSig = sig, tcdDerivs = derivs})
  | is_vanilla	          -- Normal Haskell data type decl
  = ASSERT( isNothing sig )	-- In normal H98 form, kind signature on the 
				-- data type is syntactically illegal
    do  { tyvars <- pruneTyVars tydecl
        ; bindTyVarsRn data_doc tyvars                  $ \ tyvars' -> do
    	{ tycon' <- if isFamInstDecl tydecl
		    then lookupLocatedOccRn     tycon -- may be imported family
		    else lookupLocatedTopBndrRn tycon
	; context' <- rnContext data_doc context
	; typats' <- rnTyPats data_doc typatsMaybe
	; (derivs', deriv_fvs) <- rn_derivs derivs
	; condecls' <- rnConDecls (unLoc tycon') condecls
		-- No need to check for duplicate constructor decls
		-- since that is done by RnNames.extendGlobalRdrEnvRn
	; returnM (TyData {tcdND = new_or_data, tcdCtxt = context', 
			   tcdLName = tycon', tcdTyVars = tyvars', 
			   tcdTyPats = typats', tcdKindSig = Nothing, 
			   tcdCons = condecls', tcdDerivs = derivs'}, 
		   delFVs (map hsLTyVarName tyvars')	$
	     	   extractHsCtxtTyNames context'	`plusFV`
	     	   plusFVs (map conDeclFVs condecls')   `plusFV`
	     	   deriv_fvs				`plusFV`
		   (if isFamInstDecl tydecl
		   then unitFV (unLoc tycon')	-- type instance => use
		   else emptyFVs)) 
        } }

  | otherwise	          -- GADT
  = do	{ tycon' <- if isFamInstDecl tydecl
		    then lookupLocatedOccRn     tycon -- may be imported family
		    else lookupLocatedTopBndrRn tycon
	; checkTc (null (unLoc context)) (badGadtStupidTheta tycon)
    	; (tyvars', typats')
		<- bindTyVarsRn data_doc tyvars $ \ tyvars' -> do
		   { typats' <- rnTyPats data_doc typatsMaybe
		   ; return (tyvars', typats') }
		-- For GADTs, the type variables in the declaration 
		-- do not scope over the constructor signatures
		-- 	data T a where { T1 :: forall b. b-> b }

	; (derivs', deriv_fvs) <- rn_derivs derivs
	; condecls' <- rnConDecls (unLoc tycon') condecls
		-- No need to check for duplicate constructor decls
		-- since that is done by RnNames.extendGlobalRdrEnvRn

	; returnM (TyData {tcdND = new_or_data, tcdCtxt = noLoc [], 
			   tcdLName = tycon', tcdTyVars = tyvars', 
			   tcdTyPats = typats', tcdKindSig = sig,
			   tcdCons = condecls', tcdDerivs = derivs'}, 
	     	   plusFVs (map conDeclFVs condecls') `plusFV` 
		   deriv_fvs			      `plusFV`
		   (if isFamInstDecl tydecl
		   then unitFV (unLoc tycon')	-- type instance => use
		   else emptyFVs))
        }
  where
    is_vanilla = case condecls of	-- Yuk
		     [] 		   -> True
		     L _ (ConDecl { con_res = ResTyH98 }) : _  -> True
		     _    		   -> False

    data_doc = text "In the data type declaration for" <+> quotes (ppr tycon)

    rn_derivs Nothing   = returnM (Nothing, emptyFVs)
    rn_derivs (Just ds) = rnLHsTypes data_doc ds	`thenM` \ ds' -> 
			  returnM (Just ds', extractHsTyNames_s ds')

-- "type" and "type instance" declarations
rnTyClDecl tydecl@(TySynonym {tcdLName = name,
			      tcdTyPats = typatsMaybe, tcdSynRhs = ty})
  = do { tyvars <- pruneTyVars tydecl
       ; bindTyVarsRn syn_doc tyvars                    $ \ tyvars' -> do
       { name' <- if isFamInstDecl tydecl
		  then lookupLocatedOccRn     name -- may be imported family
		  else lookupLocatedTopBndrRn name
       ; typats' <- rnTyPats syn_doc typatsMaybe
       ; (ty', fvs) <- rnHsTypeFVs syn_doc ty
       ; returnM (TySynonym {tcdLName = name', tcdTyVars = tyvars', 
			     tcdTyPats = typats', tcdSynRhs = ty'},
	          delFVs (map hsLTyVarName tyvars') $
		  fvs			      `plusFV`
		   (if isFamInstDecl tydecl
		   then unitFV (unLoc name')	-- type instance => use
		   else emptyFVs))
       } }
  where
    syn_doc = text "In the declaration for type synonym" <+> quotes (ppr name)

rnTyClDecl (ClassDecl {tcdCtxt = context, tcdLName = cname, 
		       tcdTyVars = tyvars, tcdFDs = fds, tcdSigs = sigs, 
		       tcdMeths = mbinds, tcdATs = ats, tcdDocs = docs})
  = do	{ cname' <- lookupLocatedTopBndrRn cname

	-- Tyvars scope over superclass context and method signatures
	; (tyvars', context', fds', ats', ats_fvs, sigs')
	    <- bindTyVarsRn cls_doc tyvars $ \ tyvars' -> do
	     { context' <- rnContext cls_doc context
	     ; fds' <- rnFds cls_doc fds
	     ; (ats', ats_fvs) <- rnATs ats
	     ; sigs' <- renameSigs Nothing okClsDclSig sigs
	     ; return   (tyvars', context', fds', ats', ats_fvs, sigs') }

	-- No need to check for duplicate associated type decls
	-- since that is done by RnNames.extendGlobalRdrEnvRn

	-- Check the signatures
	-- First process the class op sigs (op_sigs), then the fixity sigs (non_op_sigs).
	; let sig_rdr_names_w_locs = [op | L _ (TypeSig op _) <- sigs]
	; checkDupRdrNames sig_doc sig_rdr_names_w_locs
		-- Typechecker is responsible for checking that we only
		-- give default-method bindings for things in this class.
		-- The renamer *could* check this for class decls, but can't
		-- for instance decls.

   	-- The newLocals call is tiresome: given a generic class decl
	--	class C a where
	--	  op :: a -> a
	--	  op {| x+y |} (Inl a) = ...
	--	  op {| x+y |} (Inr b) = ...
	--	  op {| a*b |} (a*b)   = ...
	-- we want to name both "x" tyvars with the same unique, so that they are
	-- easy to group together in the typechecker.  
	; (mbinds', meth_fvs) 
	    <- extendTyVarEnvForMethodBinds tyvars' $ do
	    { name_env <- getLocalRdrEnv
	    ; let gen_rdr_tyvars_w_locs = [ tv | tv <- extractGenericPatTyVars mbinds,
	    		 		         not (unLoc tv `elemLocalRdrEnv` name_env) ]
		-- No need to check for duplicate method signatures
		-- since that is done by RnNames.extendGlobalRdrEnvRn
		-- and the methods are already in scope
	    ; gen_tyvars <- newLocalsRn gen_rdr_tyvars_w_locs
	    ; rnMethodBinds (unLoc cname') (mkSigTvFn sigs') gen_tyvars mbinds }

  -- Haddock docs 
	; docs' <- mapM (wrapLocM rnDocDecl) docs

	; return (ClassDecl { tcdCtxt = context', tcdLName = cname', 
			      tcdTyVars = tyvars', tcdFDs = fds', tcdSigs = sigs',
			      tcdMeths = mbinds', tcdATs = ats', tcdDocs = docs'},

	     	  delFVs (map hsLTyVarName tyvars')	$
	     	  extractHsCtxtTyNames context'	    	`plusFV`
	     	  plusFVs (map extractFunDepNames (map unLoc fds'))  `plusFV`
	     	  hsSigsFVs sigs'		  	`plusFV`
	     	  meth_fvs				`plusFV`
	     	  ats_fvs) }
  where
    cls_doc  = text "In the declaration for class" 	<+> ppr cname
    sig_doc  = text "In the signatures for class"  	<+> ppr cname

badGadtStupidTheta :: Located RdrName -> SDoc
badGadtStupidTheta _
  = vcat [ptext (sLit "No context is allowed on a GADT-style data declaration"),
	  ptext (sLit "(You can put a context on each contructor, though.)")]
\end{code}

%*********************************************************
%*							*
\subsection{Support code for type/data declarations}
%*							*
%*********************************************************

\begin{code}
-- Remove any duplicate type variables in family instances may have non-linear
-- left-hand sides.  Complain if any, but the first occurence of a type
-- variable has a user-supplied kind signature.
--
pruneTyVars :: TyClDecl RdrName -> RnM [LHsTyVarBndr RdrName]
pruneTyVars tydecl
  | isFamInstDecl tydecl
  = do { let pruned_tyvars = nubBy eqLTyVar tyvars
       ; assertNoSigsInRepeats tyvars
       ; return pruned_tyvars
       }
  | otherwise 
  = return tyvars
  where
    tyvars = tcdTyVars tydecl

    assertNoSigsInRepeats []       = return ()
    assertNoSigsInRepeats (tv:tvs)
      = do { let offending_tvs = [ tv' | tv'@(L _ (KindedTyVar _ _)) <- tvs
                                       , tv' `eqLTyVar` tv]
           ; checkErr (null offending_tvs) $
               illegalKindSig (head offending_tvs)
           ; assertNoSigsInRepeats tvs
           }

    illegalKindSig tv
      = hsep [ptext (sLit "Repeat variable occurrence may not have a"), 
              ptext (sLit "kind signature:"), quotes (ppr tv)]

    tv1 `eqLTyVar` tv2 = hsLTyVarLocName tv1 `eqLocated` hsLTyVarLocName tv2

-- Although, we are processing type patterns here, all type variables will
-- already be in scope (they are the same as in the 'tcdTyVars' field of the
-- type declaration to which these patterns belong)
--
rnTyPats :: SDoc -> Maybe [LHsType RdrName] -> RnM (Maybe [LHsType Name])
rnTyPats _   Nothing       = return Nothing
rnTyPats doc (Just typats) = liftM Just $ rnLHsTypes doc typats

rnConDecls :: Name -> [LConDecl RdrName] -> RnM [LConDecl Name]
rnConDecls _tycon condecls
  = mappM (wrapLocM rnConDecl) condecls

rnConDecl :: ConDecl RdrName -> RnM (ConDecl Name)
rnConDecl (ConDecl name expl tvs cxt details res_ty mb_doc)
  = do	{ addLocM checkConName name

	; new_name <- lookupLocatedTopBndrRn name
	; name_env <- getLocalRdrEnv
	
	-- For H98 syntax, the tvs are the existential ones
	-- For GADT syntax, the tvs are all the quantified tyvars
	-- Hence the 'filter' in the ResTyH98 case only
	; let not_in_scope  = not . (`elemLocalRdrEnv` name_env) . unLoc
	      arg_tys       = hsConDeclArgTys details
	      implicit_tvs  = case res_ty of
	      	    		ResTyH98 -> filter not_in_scope $
						get_rdr_tvs arg_tys
	      	    		ResTyGADT ty -> get_rdr_tvs (ty : arg_tys)
	      tvs' = case expl of
	        	Explicit -> tvs
		    	Implicit -> userHsTyVarBndrs implicit_tvs

	; mb_doc' <- rnMbLHsDoc mb_doc 

	; bindTyVarsRn doc tvs' $ \new_tyvars -> do
	{ new_context <- rnContext doc cxt
        ; new_details <- rnConDeclDetails doc details
        ; (new_details', new_res_ty)  <- rnConResult doc new_details res_ty
        ; return (ConDecl new_name expl new_tyvars new_context new_details' new_res_ty mb_doc') }}
 where
    doc = text "In the definition of data constructor" <+> quotes (ppr name)
    get_rdr_tvs tys  = extractHsRhoRdrTyVars cxt (noLoc (HsTupleTy Boxed tys))

rnConResult :: SDoc
            -> HsConDetails (LHsType Name) [ConDeclField Name]
            -> ResType RdrName
            -> RnM (HsConDetails (LHsType Name) [ConDeclField Name],
                    ResType Name)
rnConResult _ details ResTyH98 = return (details, ResTyH98)

rnConResult doc details (ResTyGADT ty) = do
    ty' <- rnHsSigType doc ty
    let (arg_tys, res_ty) = splitHsFunType ty'
	-- We can split it up, now the renamer has dealt with fixities
    case details of
	PrefixCon _xs -> ASSERT( null _xs ) return (PrefixCon arg_tys, ResTyGADT res_ty)
	RecCon _ -> return (details, ResTyGADT ty')
	InfixCon {}   -> panic "rnConResult"

rnConDeclDetails :: SDoc
                 -> HsConDetails (LHsType RdrName) [ConDeclField RdrName]
                 -> RnM (HsConDetails (LHsType Name) [ConDeclField Name])
rnConDeclDetails doc (PrefixCon tys)
  = mappM (rnLHsType doc) tys	`thenM` \ new_tys  ->
    returnM (PrefixCon new_tys)

rnConDeclDetails doc (InfixCon ty1 ty2)
  = rnLHsType doc ty1  		`thenM` \ new_ty1 ->
    rnLHsType doc ty2  		`thenM` \ new_ty2 ->
    returnM (InfixCon new_ty1 new_ty2)

rnConDeclDetails doc (RecCon fields)
  = do	{ new_fields <- mappM (rnField doc) fields
		-- No need to check for duplicate fields
		-- since that is done by RnNames.extendGlobalRdrEnvRn
	; return (RecCon new_fields) }

rnField :: SDoc -> ConDeclField RdrName -> RnM (ConDeclField Name)
rnField doc (ConDeclField name ty haddock_doc)
  = lookupLocatedTopBndrRn name	`thenM` \ new_name ->
    rnLHsType doc ty		`thenM` \ new_ty ->
    rnMbLHsDoc haddock_doc      `thenM` \ new_haddock_doc ->
    returnM (ConDeclField new_name new_ty new_haddock_doc) 

-- Rename family declarations
--
-- * This function is parametrised by the routine handling the index
--   variables.  On the toplevel, these are defining occurences, whereas they
--   are usage occurences for associated types.
--
rnFamily :: TyClDecl RdrName 
         -> (SDoc -> [LHsTyVarBndr RdrName] -> 
	     ([LHsTyVarBndr Name] -> RnM (TyClDecl Name, FreeVars)) ->
	     RnM (TyClDecl Name, FreeVars))
         -> RnM (TyClDecl Name, FreeVars)

rnFamily (tydecl@TyFamily {tcdFlavour = flavour, 
			   tcdLName = tycon, tcdTyVars = tyvars}) 
        bindIdxVars =
      do { checkM (isDataFlavour flavour                      -- for synonyms,
		   || not (null tyvars)) $ addErr needOneIdx  -- no. of indexes >= 1
	 ; bindIdxVars (family_doc tycon) tyvars $ \tyvars' -> do {
	 ; tycon' <- lookupLocatedTopBndrRn tycon
	 ; returnM (TyFamily {tcdFlavour = flavour, tcdLName = tycon', 
			      tcdTyVars = tyvars', tcdKind = tcdKind tydecl}, 
		    emptyFVs) 
         } }
      where
        isDataFlavour DataFamily = True
	isDataFlavour _		 = False
rnFamily d _ = pprPanic "rnFamily" (ppr d)

family_doc :: Located RdrName -> SDoc
family_doc tycon = text "In the family declaration for" <+> quotes (ppr tycon)

needOneIdx :: SDoc
needOneIdx = text "Type family declarations requires at least one type index"

-- Rename associated type declarations (in classes)
--
-- * This can be family declarations and (default) type instances
--
rnATs :: [LTyClDecl RdrName] -> RnM ([LTyClDecl Name], FreeVars)
rnATs ats = mapFvRn (wrapLocFstM rn_at) ats
  where
    rn_at (tydecl@TyFamily  {}) = rnFamily tydecl lookupIdxVars
    rn_at (tydecl@TySynonym {}) = 
      do
        checkM (isNothing (tcdTyPats tydecl)) $ addErr noPatterns
        rnTyClDecl tydecl
    rn_at _                      = panic "RnSource.rnATs: invalid TyClDecl"

    lookupIdxVars _ tyvars cont = 
      do { checkForDups tyvars;
	 ; tyvars' <- mappM lookupIdxVar tyvars
	 ; cont tyvars'
	 }
    -- Type index variables must be class parameters, which are the only
    -- type variables in scope at this point.
    lookupIdxVar (L l tyvar) =
      do
	name' <- lookupOccRn (hsTyVarName tyvar)
	return $ L l (replaceTyVarName tyvar name')

    -- Type variable may only occur once.
    --
    checkForDups [] = return ()
    checkForDups (L loc tv:ltvs) = 
      do { setSrcSpan loc $
	     when (hsTyVarName tv `ltvElem` ltvs) $
	       addErr (repeatedTyVar tv)
	 ; checkForDups ltvs
	 }

    _       `ltvElem` [] = False
    rdrName `ltvElem` (L _ tv:ltvs)
      | rdrName == hsTyVarName tv = True
      | otherwise		  = rdrName `ltvElem` ltvs

noPatterns :: SDoc
noPatterns = text "Default definition for an associated synonym cannot have"
	     <+> text "type pattern"

repeatedTyVar :: HsTyVarBndr RdrName -> SDoc
repeatedTyVar tv = ptext (sLit "Illegal repeated type variable") <+>
		   quotes (ppr tv)

-- This data decl will parse OK
--	data T = a Int
-- treating "a" as the constructor.
-- It is really hard to make the parser spot this malformation.
-- So the renamer has to check that the constructor is legal
--
-- We can get an operator as the constructor, even in the prefix form:
--	data T = :% Int Int
-- from interface files, which always print in prefix form

checkConName :: RdrName -> TcRn ()
checkConName name = checkErr (isRdrDataCon name) (badDataCon name)

badDataCon :: RdrName -> SDoc
badDataCon name
   = hsep [ptext (sLit "Illegal data constructor name"), quotes (ppr name)]
\end{code}


%*********************************************************
%*							*
\subsection{Support code for type/data declarations}
%*							*
%*********************************************************

Get the mapping from constructors to fields for this module.
It's convenient to do this after the data type decls have been renamed
\begin{code}
extendRecordFieldEnv :: [LTyClDecl RdrName] -> [LInstDecl RdrName] -> TcM TcGblEnv
extendRecordFieldEnv tycl_decls inst_decls
  = do	{ tcg_env <- getGblEnv
	; field_env' <- foldrM get_con (tcg_field_env tcg_env) all_data_cons
	; return (tcg_env { tcg_field_env = field_env' }) }
  where
    -- we want to lookup:
    --  (a) a datatype constructor
    --  (b) a record field
    -- knowing that they're from this module.
    -- lookupLocatedTopBndrRn does this, because it does a lookupGreLocalRn,
    -- which keeps only the local ones.
    lookup x = do { x' <- lookupLocatedTopBndrRn x
                    ; return $ unLoc x'}

    all_data_cons :: [ConDecl RdrName]
    all_data_cons = [con | L _ (TyData { tcdCons = cons }) <- all_tycl_decls
    		         , L _ con <- cons ]
    all_tycl_decls = at_tycl_decls ++ tycl_decls
    at_tycl_decls = [at | L _ (InstDecl _ _ _ ats) <- inst_decls, at <- ats]
    		      -- Do not forget associated types!

    get_con (ConDecl { con_name = con, con_details = RecCon flds })
	    (RecFields env fld_set)
	= do { con' <- lookup con
             ; flds' <- mappM lookup (map cd_fld_name flds)
	     ; let env'    = extendNameEnv env con' flds'
	           fld_set' = addListToNameSet fld_set flds'
             ; return $ (RecFields env' fld_set') }
    get_con _ env = return env
\end{code}

%*********************************************************
%*							*
\subsection{Support code to rename types}
%*							*
%*********************************************************

\begin{code}
rnFds :: SDoc -> [Located (FunDep RdrName)] -> RnM [Located (FunDep Name)]

rnFds doc fds
  = mappM (wrapLocM rn_fds) fds
  where
    rn_fds (tys1, tys2)
      =	rnHsTyVars doc tys1		`thenM` \ tys1' ->
	rnHsTyVars doc tys2		`thenM` \ tys2' ->
	returnM (tys1', tys2')

rnHsTyVars :: SDoc -> [RdrName] -> RnM [Name]
rnHsTyVars doc tvs  = mappM (rnHsTyVar doc) tvs

rnHsTyVar :: SDoc -> RdrName -> RnM Name
rnHsTyVar _doc tyvar = lookupOccRn tyvar
\end{code}


%*********************************************************
%*							*
		Splices
%*							*
%*********************************************************

Note [Splices]
~~~~~~~~~~~~~~
Consider
	f = ...
	h = ...$(thing "f")...

The splice can expand into literally anything, so when we do dependency
analysis we must assume that it might mention 'f'.  So we simply treat
all locally-defined names as mentioned by any splice.  This is terribly
brutal, but I don't see what else to do.  For example, it'll mean
that every locally-defined thing will appear to be used, so no unused-binding
warnings.  But if we miss the dependency, then we might typecheck 'h' before 'f',
and that will crash the type checker because 'f' isn't in scope.

Currently, I'm not treating a splice as also mentioning every import,
which is a bit inconsistent -- but there are a lot of them.  We might
thereby get some bogus unused-import warnings, but we won't crash the
type checker.  Not very satisfactory really.

\begin{code}
rnSplice :: HsSplice RdrName -> RnM (HsSplice Name, FreeVars)
rnSplice (HsSplice n expr)
  = do	{ checkTH expr "splice"
	; loc  <- getSrcSpanM
	; [n'] <- newLocalsRn [L loc n]
	; (expr', fvs) <- rnLExpr expr

	-- Ugh!  See Note [Splices] above
	; lcl_rdr <- getLocalRdrEnv
	; gbl_rdr <- getGlobalRdrEnv
	; let gbl_names = mkNameSet [gre_name gre | gre <- globalRdrEnvElts gbl_rdr, 
						    isLocalGRE gre]
	      lcl_names = mkNameSet (occEnvElts lcl_rdr)

	; return (HsSplice n' expr', fvs `plusFV` lcl_names `plusFV` gbl_names) }

checkTH :: Outputable a => a -> String -> RnM ()
#ifdef GHCI 
checkTH _ _ = returnM ()	-- OK
#else
checkTH e what 	-- Raise an error in a stage-1 compiler
  = addErr (vcat [ptext (sLit "Template Haskell") <+> text what <+>  
	          ptext (sLit "illegal in a stage-1 compiler"),
	          nest 2 (ppr e)])
#endif   
\end{code}

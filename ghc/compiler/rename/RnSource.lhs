%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnSource ( 
	rnSrcDecls, addTcgDUs, 
	rnTyClDecls, checkModDeprec,
	rnBinds, rnBindsAndThen
    ) where

#include "HsVersions.h"

import HsSyn
import RdrName		( RdrName, isRdrDataCon, rdrNameOcc, elemLocalRdrEnv )
import RdrHsSyn		( RdrNameConDecl, RdrNameHsBinds,
			  RdrNameDeprecation, RdrNameFixitySig,
			  extractGenericPatTyVars )
import RnHsSyn
import RnExpr		( rnExpr )
import RnTypes		( rnHsType, rnHsSigType, rnHsTypeFVs, rnContext )
import RnBinds		( rnTopMonoBinds, rnMonoBinds, rnMethodBinds, 
			  rnMonoBindsAndThen, renameSigs, checkSigs )
import RnEnv		( lookupTopBndrRn, lookupTopFixSigNames,
			  lookupOccRn, newLocalsRn, 
			  bindLocalsFV, bindPatSigTyVarsFV,
			  bindTyVarsRn, extendTyVarEnvFVRn,
			  bindLocalNames, newIPNameRn,
			  checkDupNames, mapFvRn,
			  unknownNameErr
			)
import TcRnMonad

import BasicTypes	( TopLevelFlag(..)  )
import HscTypes		( FixityEnv, FixItem(..),
			  Deprecations, Deprecs(..), DeprecTxt, plusDeprecs )
import Class		( FunDep )
import Name		( Name )
import NameSet
import NameEnv
import Outputable
import SrcLoc		( SrcLoc )
import CmdLineOpts	( DynFlag(..) )
				-- Warn of unused for-all'd tyvars
import Maybes		( seqMaybe )
import Maybe            ( catMaybes, isNothing )
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
rnSrcDecls :: HsGroup RdrName -> RnM (TcGblEnv, HsGroup Name)

rnSrcDecls (HsGroup { hs_valds  = MonoBind binds sigs _,
		      hs_tyclds = tycl_decls,
		      hs_instds = inst_decls,
		      hs_fixds  = fix_decls,
		      hs_depds  = deprec_decls,
		      hs_fords  = foreign_decls,
		      hs_defds  = default_decls,
		      hs_ruleds = rule_decls })

 = do {		-- Deal with deprecations (returns only the extra deprecations)
	deprecs <- rnSrcDeprecDecls deprec_decls ;
	updGblEnv (\gbl -> gbl { tcg_deprecs = tcg_deprecs gbl `plusDeprecs` deprecs })
		  $ do {

		-- Deal with top-level fixity decls 
		-- (returns the total new fixity env)
	fix_env <- rnSrcFixityDecls fix_decls ;
	updGblEnv (\gbl -> gbl { tcg_fix_env = fix_env })
		  $ do {

		-- Rename other declarations
	traceRn (text "Start rnmono") ;
	(rn_val_decls, bind_dus) <- rnTopMonoBinds binds sigs ;
	traceRn (text "finish rnmono" <+> ppr rn_val_decls) ;

		-- You might think that we could build proper def/use information
		-- for type and class declarations, but they can be involved
		-- in mutual recursion across modules, and we only do the SCC
		-- analysis for them in the type checker.
		-- So we content ourselves with gathering uses only; that
		-- means we'll only report a declaration as unused if it isn't
		-- mentioned at all.  Ah well.
	(rn_tycl_decls,    src_fvs1) <- mapFvRn rnTyClDecl tycl_decls ;
	(rn_inst_decls,    src_fvs2) <- mapFvRn rnSrcInstDecl inst_decls ;
	(rn_rule_decls,    src_fvs3) <- mapFvRn rnHsRuleDecl rule_decls ;
	(rn_foreign_decls, src_fvs4) <- mapFvRn rnHsForeignDecl foreign_decls ;
	(rn_default_decls, src_fvs5) <- mapFvRn rnDefaultDecl default_decls ;
	
	let {
	   rn_group = HsGroup { hs_valds  = rn_val_decls,
			    	hs_tyclds = rn_tycl_decls,
			    	hs_instds = rn_inst_decls,
			    	hs_fixds  = [],
			    	hs_depds  = [],
			    	hs_fords  = rn_foreign_decls,
			    	hs_defds  = rn_default_decls,
			    	hs_ruleds = rn_rule_decls } ;

	   other_fvs = plusFVs [src_fvs1, src_fvs2, src_fvs3, 
				src_fvs4, src_fvs5] ;
	   src_dus = bind_dus `plusDU` usesOnly other_fvs 
	} ;

	traceRn (text "finish rnSrc" <+> ppr rn_group) ;
	tcg_env <- getGblEnv ;
	return (tcg_env `addTcgDUs` src_dus, rn_group)
    }}}
rnTyClDecls :: [TyClDecl RdrName] -> RnM [TyClDecl Name]
rnTyClDecls tycl_decls = do { (decls', fvs) <- mapFvRn rnTyClDecl tycl_decls
			    ; return decls' }

addTcgDUs :: TcGblEnv -> DefUses -> TcGblEnv 
addTcgDUs tcg_env dus = tcg_env { tcg_dus = tcg_dus tcg_env `plusDU` dus }
\end{code}


%*********************************************************
%*						 	 *
	Source-code fixity declarations
%*							 *
%*********************************************************

\begin{code}
rnSrcFixityDecls :: [RdrNameFixitySig] -> RnM FixityEnv
rnSrcFixityDecls fix_decls
  = getGblEnv					`thenM` \ gbl_env ->
    foldlM rnFixityDecl (tcg_fix_env gbl_env) 
	    fix_decls				 	`thenM` \ fix_env ->
    traceRn (text "fixity env" <+> pprFixEnv fix_env)	`thenM_`
    returnM fix_env

rnFixityDecl :: FixityEnv -> RdrNameFixitySig -> RnM FixityEnv
rnFixityDecl fix_env (FixitySig rdr_name fixity loc)
  =	-- GHC extension: look up both the tycon and data con 
	-- for con-like things
	-- If neither are in scope, report an error; otherwise
	-- add both to the fixity env
     lookupTopFixSigNames rdr_name	`thenM` \ names ->
     if null names then
	  addSrcLoc loc (addErr (unknownNameErr rdr_name))	`thenM_`
	  returnM fix_env
     else
	  foldlM add fix_env names
  where
    add fix_env name
      = case lookupNameEnv fix_env name of
          Just (FixItem _ _ loc') 
		  -> addErr (dupFixityDecl rdr_name loc loc')	`thenM_`
    		     returnM fix_env
    	  Nothing -> returnM (extendNameEnv fix_env name fix_item)
      where
	fix_item = FixItem (rdrNameOcc rdr_name) fixity loc

pprFixEnv :: FixityEnv -> SDoc
pprFixEnv env 
  = pprWithCommas (\ (FixItem n f _) -> ppr f <+> ppr n)
		  (nameEnvElts env)

dupFixityDecl rdr_name loc1 loc2
  = vcat [ptext SLIT("Multiple fixity declarations for") <+> quotes (ppr rdr_name),
	  ptext SLIT("at ") <+> ppr loc1,
	  ptext SLIT("and") <+> ppr loc2]
\end{code}


%*********************************************************
%*						 	 *
	Source-code deprecations declarations
%*							 *
%*********************************************************

For deprecations, all we do is check that the names are in scope.
It's only imported deprecations, dealt with in RnIfaces, that we
gather them together.

\begin{code}
rnSrcDeprecDecls :: [RdrNameDeprecation] -> RnM Deprecations
rnSrcDeprecDecls [] 
  = returnM NoDeprecs

rnSrcDeprecDecls decls
  = mappM rn_deprec decls	`thenM` \ pairs ->
    returnM (DeprecSome (mkNameEnv (catMaybes pairs)))
 where
   rn_deprec (Deprecation rdr_name txt loc)
     = addSrcLoc loc		$
       lookupTopBndrRn rdr_name	`thenM` \ name ->
       returnM (Just (name, (rdrNameOcc rdr_name, txt)))

checkModDeprec :: Maybe DeprecTxt -> Deprecations
-- Check for a module deprecation; done once at top level
checkModDeprec Nothing    = NoDeprecs
checkModDeprec (Just txt) = DeprecAll txt
\end{code}

%*********************************************************
%*							*
\subsection{Source code declarations}
%*							*
%*********************************************************

\begin{code}
rnDefaultDecl (DefaultDecl tys src_loc)
  = addSrcLoc src_loc 			$
    mapFvRn (rnHsTypeFVs doc_str) tys	`thenM` \ (tys', fvs) ->
    returnM (DefaultDecl tys' src_loc, fvs)
  where
    doc_str = text "In a `default' declaration"
\end{code}

%*********************************************************
%*							*
		Bindings
%*							*
%*********************************************************

These chaps are here, rather than in TcBinds, so that there
is just one hi-boot file (for RnSource).  rnSrcDecls is part
of the loop too, and it must be defined in this module.

\begin{code}
rnBinds    :: RdrNameHsBinds -> RnM (RenamedHsBinds, DefUses)
-- This version assumes that the binders are already in scope
-- It's used only in 'mdo'
rnBinds EmptyBinds	       = returnM (EmptyBinds, emptyDUs)
rnBinds (MonoBind bind sigs _) = rnMonoBinds NotTopLevel bind sigs
rnBinds b@(IPBinds bind)       = addErr (badIpBinds b)	`thenM_` 
			         returnM (EmptyBinds, emptyDUs)

rnBindsAndThen	:: RdrNameHsBinds 
		-> (RenamedHsBinds -> RnM (result, FreeVars))
		-> RnM (result, FreeVars)
-- This version (a) assumes that the binding vars are not already in scope
--		(b) removes the binders from the free vars of the thing inside
-- The parser doesn't produce ThenBinds
rnBindsAndThen EmptyBinds	       thing_inside = thing_inside EmptyBinds
rnBindsAndThen (MonoBind bind sigs _)  thing_inside = rnMonoBindsAndThen bind sigs thing_inside
rnBindsAndThen (IPBinds binds) thing_inside
  = rnIPBinds binds				`thenM` \ (binds',fv_binds) ->
    thing_inside (IPBinds binds')		`thenM` \ (thing, fvs_thing) ->
    returnM (thing, fvs_thing `plusFV` fv_binds)

rnIPBinds [] = returnM ([], emptyFVs)
rnIPBinds ((n, expr) : binds)
  = newIPNameRn  n		`thenM` \ name ->
    rnExpr expr			`thenM` \ (expr',fvExpr) ->
    rnIPBinds binds		`thenM` \ (binds',fvBinds) ->
    returnM ((name, expr') : binds', fvExpr `plusFV` fvBinds)

badIpBinds binds
  = hang (ptext SLIT("Implicit-parameter bindings illegal in 'mdo':")) 4
	 (ppr binds)
\end{code}


%*********************************************************
%*							*
\subsection{Foreign declarations}
%*							*
%*********************************************************

\begin{code}
rnHsForeignDecl (ForeignImport name ty spec isDeprec src_loc)
  = addSrcLoc src_loc 		$
    lookupTopBndrRn name	        `thenM` \ name' ->
    rnHsTypeFVs (fo_decl_msg name) ty	`thenM` \ (ty', fvs) ->
    returnM (ForeignImport name' ty' spec isDeprec src_loc, fvs)

rnHsForeignDecl (ForeignExport name ty spec isDeprec src_loc)
  = addSrcLoc src_loc 			$
    lookupOccRn name		        `thenM` \ name' ->
    rnHsTypeFVs (fo_decl_msg name) ty  	`thenM` \ (ty', fvs) ->
    returnM (ForeignExport name' ty' spec isDeprec src_loc, fvs )
	-- NB: a foreign export is an *occurrence site* for name, so 
	--     we add it to the free-variable list.  It might, for example,
	--     be imported from another module

fo_decl_msg name = ptext SLIT("In the foreign declaration for") <+> ppr name
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************

\begin{code}
rnSrcInstDecl (InstDecl inst_ty mbinds uprags src_loc)
	-- Used for both source and interface file decls
  = addSrcLoc src_loc $
    rnHsSigType (text "an instance decl") inst_ty	`thenM` \ inst_ty' ->

	-- Rename the bindings
	-- The typechecker (not the renamer) checks that all 
	-- the bindings are for the right class
    let
	meth_doc    = text "In the bindings in an instance declaration"
	meth_names  = collectLocatedMonoBinders mbinds
	(inst_tyvars, _, cls,_) = splitHsInstDeclTy inst_ty'
    in
    checkDupNames meth_doc meth_names 	`thenM_`
    extendTyVarEnvForMethodBinds inst_tyvars (		
	-- (Slightly strangely) the forall-d tyvars scope over
	-- the method bindings too
	rnMethodBinds cls [] mbinds
    )						`thenM` \ (mbinds', meth_fvs) ->
	-- Rename the prags and signatures.
	-- Note that the type variables are not in scope here,
	-- so that	instance Eq a => Eq (T a) where
	--			{-# SPECIALISE instance Eq a => Eq (T [a]) #-}
	-- works OK. 
	--
	-- But the (unqualified) method names are in scope
    let 
	binders = collectMonoBinders mbinds'
    in
    bindLocalNames binders (renameSigs uprags)			`thenM` \ uprags' ->
    checkSigs (okInstDclSig (mkNameSet binders)) uprags'	`thenM_`

    returnM (InstDecl inst_ty' mbinds' uprags' src_loc,
	     meth_fvs `plusFV` hsSigsFVs uprags' 
		      `plusFV` extractHsTyNames inst_ty')
\end{code}

For the method bindings in class and instance decls, we extend the 
type variable environment iff -fglasgow-exts

\begin{code}
extendTyVarEnvForMethodBinds tyvars thing_inside
  = doptM Opt_GlasgowExts			`thenM` \ opt_GlasgowExts ->
    if opt_GlasgowExts then
	extendTyVarEnvFVRn (map hsTyVarName tyvars) thing_inside
    else
	thing_inside
\end{code}


%*********************************************************
%*							*
\subsection{Rules}
%*							*
%*********************************************************

\begin{code}
rnHsRuleDecl (HsRule rule_name act vars lhs rhs src_loc)
  = addSrcLoc src_loc					$
    bindPatSigTyVarsFV (collectRuleBndrSigTys vars)	$

    bindLocalsFV doc (map get_var vars)		$ \ ids ->
    mapFvRn rn_var (vars `zip` ids)		`thenM` \ (vars', fv_vars) ->

    rnExpr lhs					`thenM` \ (lhs', fv_lhs) ->
    rnExpr rhs					`thenM` \ (rhs', fv_rhs) ->
    let
	mb_bad = validRuleLhs ids lhs'
    in
    checkErr (isNothing mb_bad)
	     (badRuleLhsErr rule_name lhs' mb_bad)	`thenM_`
    let
	bad_vars = [var | var <- ids, not (var `elemNameSet` fv_lhs)]
    in
    mappM (addErr . badRuleVar rule_name) bad_vars	`thenM_`
    returnM (HsRule rule_name act vars' lhs' rhs' src_loc,
	     fv_vars `plusFV` fv_lhs `plusFV` fv_rhs)
  where
    doc = text "In the transformation rule" <+> ftext rule_name
  
    get_var (RuleBndr v)      = v
    get_var (RuleBndrSig v _) = v

    rn_var (RuleBndr v, id)	 = returnM (RuleBndr id, emptyFVs)
    rn_var (RuleBndrSig v t, id) = rnHsTypeFVs doc t	`thenM` \ (t', fvs) ->
				   returnM (RuleBndrSig id t', fvs)
\end{code}

Check the shape of a transformation rule LHS.  Currently
we only allow LHSs of the form @(f e1 .. en)@, where @f@ is
not one of the @forall@'d variables.  We also restrict the form of the LHS so
that it may be plausibly matched.  Basically you only get to write ordinary 
applications.  (E.g. a case expression is not allowed: too elaborate.)

NB: if you add new cases here, make sure you add new ones to TcRule.ruleLhsTvs

\begin{code}
validRuleLhs :: [Name] -> RenamedHsExpr -> Maybe RenamedHsExpr
-- Nothing => OK
-- Just e  => Not ok, and e is the offending expression
validRuleLhs foralls lhs
  = check lhs
  where
    check (OpApp e1 op _ e2)		  = check op `seqMaybe` check_e e1 `seqMaybe` check_e e2
    check (HsApp e1 e2) 		  = check e1 `seqMaybe` check_e e2
    check (HsVar v) | v `notElem` foralls = Nothing
    check other				  = Just other 	-- Failure

    check_e (HsVar v)     = Nothing
    check_e (HsPar e) 	  = check_e e
    check_e (HsLit e) 	  = Nothing
    check_e (HsOverLit e) = Nothing

    check_e (OpApp e1 op _ e2) 	 = check_e e1 `seqMaybe` check_e op `seqMaybe` check_e e2
    check_e (HsApp e1 e2)      	 = check_e e1 `seqMaybe` check_e e2
    check_e (NegApp e _)       	 = check_e e
    check_e (ExplicitList _ es)	 = check_es es
    check_e (ExplicitTuple es _) = check_es es
    check_e other		 = Just other	-- Fails

    check_es es = foldr (seqMaybe . check_e) Nothing es

badRuleLhsErr name lhs (Just bad_e)
  = sep [ptext SLIT("Rule") <+> ftext name <> colon,
	 nest 4 (vcat [ptext SLIT("Illegal expression:") <+> ppr bad_e, 
		       ptext SLIT("in left-hand side:") <+> ppr lhs])]
    $$
    ptext SLIT("LHS must be of form (f e1 .. en) where f is not forall'd")

badRuleVar name var
  = sep [ptext SLIT("Rule") <+> doubleQuotes (ftext name) <> colon,
	 ptext SLIT("Forall'd variable") <+> quotes (ppr var) <+> 
		ptext SLIT("does not appear on left hand side")]
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
rnTyClDecl (ForeignType {tcdName = name, tcdFoType = fo_type, tcdExtName = ext_name, tcdLoc = loc})
  = addSrcLoc loc 			$
    lookupTopBndrRn name		`thenM` \ name' ->
    returnM (ForeignType {tcdName = name', tcdFoType = fo_type, tcdExtName = ext_name, tcdLoc = loc},
	     emptyFVs)

rnTyClDecl (TyData {tcdND = new_or_data, tcdCtxt = context, tcdName = tycon,
		       tcdTyVars = tyvars, tcdCons = condecls, 
		       tcdDerivs = derivs, tcdLoc = src_loc})
  = addSrcLoc src_loc $
    lookupTopBndrRn tycon		    	`thenM` \ tycon' ->
    bindTyVarsRn data_doc tyvars		$ \ tyvars' ->
    rnContext data_doc context 			`thenM` \ context' ->
    rn_derivs derivs 				`thenM` \ (derivs', deriv_fvs) ->
    checkDupNames data_doc con_names	`thenM_`
    rnConDecls tycon' condecls			`thenM` \ condecls' ->
    returnM (TyData {tcdND = new_or_data, tcdCtxt = context', tcdName = tycon',
		     tcdTyVars = tyvars', tcdCons = condecls', 
		     tcdDerivs = derivs', tcdLoc = src_loc}, 
	     delFVs (map hsTyVarName tyvars')	$
	     extractHsCtxtTyNames context'	`plusFV`
	     plusFVs (map conDeclFVs condecls') `plusFV`
	     deriv_fvs)
  where
    data_doc = text "In the data type declaration for" <+> quotes (ppr tycon)
    con_names = map conDeclName condecls

    rn_derivs Nothing   = returnM (Nothing, emptyFVs)
    rn_derivs (Just ds) = rnContext data_doc ds	`thenM` \ ds' -> 
			  returnM (Just ds', extractHsCtxtTyNames ds')
    
rnTyClDecl (TySynonym {tcdName = name, tcdTyVars = tyvars, tcdSynRhs = ty, tcdLoc = src_loc})
  = addSrcLoc src_loc $
    lookupTopBndrRn name			`thenM` \ name' ->
    bindTyVarsRn syn_doc tyvars 		$ \ tyvars' ->
    rnHsTypeFVs syn_doc ty			`thenM` \ (ty', fvs) ->
    returnM (TySynonym {tcdName = name', tcdTyVars = tyvars', 
			tcdSynRhs = ty', tcdLoc = src_loc},
	     delFVs (map hsTyVarName tyvars') fvs)
  where
    syn_doc = text "In the declaration for type synonym" <+> quotes (ppr name)

rnTyClDecl (ClassDecl {tcdCtxt = context, tcdName = cname, 
		       tcdTyVars = tyvars, tcdFDs = fds, tcdSigs = sigs, 
		       tcdMeths = mbinds, tcdLoc = src_loc})
  = addSrcLoc src_loc $
    lookupTopBndrRn cname			`thenM` \ cname' ->

	-- Tyvars scope over superclass context and method signatures
    bindTyVarsRn cls_doc tyvars			( \ tyvars' ->
	rnContext cls_doc context	`thenM` \ context' ->
	rnFds cls_doc fds		`thenM` \ fds' ->
	renameSigs sigs			`thenM` \ sigs' ->
	returnM   (tyvars', context', fds', sigs')
    )	`thenM` \ (tyvars', context', fds', sigs') ->

	-- Check the signatures
	-- First process the class op sigs (op_sigs), then the fixity sigs (non_op_sigs).
    let
	sig_rdr_names_w_locs   = [(op,locn) | Sig op _ locn <- sigs]
    in
    checkDupNames sig_doc sig_rdr_names_w_locs	`thenM_` 
    checkSigs okClsDclSig sigs'				`thenM_`
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
    extendTyVarEnvForMethodBinds tyvars' (
   	 getLocalRdrEnv					`thenM` \ name_env ->
   	 let
 	     meth_rdr_names_w_locs = collectLocatedMonoBinders mbinds
 	     gen_rdr_tyvars_w_locs = [(tv,src_loc) | tv <- extractGenericPatTyVars mbinds,
 						     not (tv `elemLocalRdrEnv` name_env)]
   	 in
   	 checkDupNames meth_doc meth_rdr_names_w_locs	`thenM_`
   	 newLocalsRn gen_rdr_tyvars_w_locs			`thenM` \ gen_tyvars ->
   	 rnMethodBinds cname' gen_tyvars mbinds
    )								`thenM` \ (mbinds', meth_fvs) ->

    returnM (ClassDecl { tcdCtxt = context', tcdName = cname', tcdTyVars = tyvars',
			 tcdFDs = fds', tcdSigs = sigs', tcdMeths = mbinds', 
			 tcdLoc = src_loc},
	     delFVs (map hsTyVarName tyvars')	$
	     extractHsCtxtTyNames context'	    `plusFV`
	     plusFVs (map extractFunDepNames fds')  `plusFV`
	     hsSigsFVs sigs'		  	    `plusFV`
	     meth_fvs)
  where
    meth_doc = text "In the default-methods for class"	<+> ppr cname
    cls_doc  = text "In the declaration for class" 	<+> ppr cname
    sig_doc  = text "In the signatures for class"  	<+> ppr cname
\end{code}

%*********************************************************
%*							*
\subsection{Support code for type/data declarations}
%*							*
%*********************************************************

\begin{code}
conDeclName :: RdrNameConDecl -> (RdrName, SrcLoc)
conDeclName (ConDecl n _ _ _ l) = (n,l)

rnConDecls :: Name -> [RdrNameConDecl] -> RnM [RenamedConDecl]
rnConDecls tycon condecls
  = 	-- Check that there's at least one condecl,
	-- or else we're reading an interface file, or -fglasgow-exts
    (if null condecls then
	doptM Opt_GlasgowExts	`thenM` \ glaExts ->
	checkErr glaExts (emptyConDeclsErr tycon)
     else returnM ()
    )						`thenM_` 
    mappM rnConDecl condecls

rnConDecl :: RdrNameConDecl -> RnM RenamedConDecl
rnConDecl (ConDecl name tvs cxt details locn)
  = addSrcLoc locn $
    checkConName name		`thenM_` 
    lookupTopBndrRn name	`thenM` \ new_name ->

    bindTyVarsRn doc tvs 		$ \ new_tyvars ->
    rnContext doc cxt			`thenM` \ new_context ->
    rnConDetails doc locn details	`thenM` \ new_details -> 
    returnM (ConDecl new_name new_tyvars new_context new_details locn)
  where
    doc = text "In the definition of data constructor" <+> quotes (ppr name)

rnConDetails doc locn (PrefixCon tys)
  = mappM (rnBangTy doc) tys	`thenM` \ new_tys  ->
    returnM (PrefixCon new_tys)

rnConDetails doc locn (InfixCon ty1 ty2)
  = rnBangTy doc ty1  		`thenM` \ new_ty1 ->
    rnBangTy doc ty2  		`thenM` \ new_ty2 ->
    returnM (InfixCon new_ty1 new_ty2)

rnConDetails doc locn (RecCon fields)
  = checkDupNames doc field_names	`thenM_`
    mappM (rnField doc) fields		`thenM` \ new_fields ->
    returnM (RecCon new_fields)
  where
    field_names = [(fld, locn) | (fld, _) <- fields]

rnField doc (name, ty)
  = lookupTopBndrRn name	`thenM` \ new_name ->
    rnBangTy doc ty		`thenM` \ new_ty ->
    returnM (new_name, new_ty) 

rnBangTy doc (BangType s ty)
  = rnHsType doc ty		`thenM` \ new_ty ->
    returnM (BangType s new_ty)

-- This data decl will parse OK
--	data T = a Int
-- treating "a" as the constructor.
-- It is really hard to make the parser spot this malformation.
-- So the renamer has to check that the constructor is legal
--
-- We can get an operator as the constructor, even in the prefix form:
--	data T = :% Int Int
-- from interface files, which always print in prefix form

checkConName name = checkErr (isRdrDataCon name) (badDataCon name)

badDataCon name
   = hsep [ptext SLIT("Illegal data constructor name"), quotes (ppr name)]

emptyConDeclsErr tycon
  = sep [quotes (ppr tycon) <+> ptext SLIT("has no constructors"),
	 nest 4 (ptext SLIT("(-fglasgow-exts permits this)"))]
\end{code}


%*********************************************************
%*							*
\subsection{Support code to rename types}
%*							*
%*********************************************************

\begin{code}
rnFds :: SDoc -> [FunDep RdrName] -> RnM [FunDep Name]

rnFds doc fds
  = mappM rn_fds fds
  where
    rn_fds (tys1, tys2)
      =	rnHsTyVars doc tys1		`thenM` \ tys1' ->
	rnHsTyVars doc tys2		`thenM` \ tys2' ->
	returnM (tys1', tys2')

rnHsTyVars doc tvs  = mappM (rnHsTyvar doc) tvs
rnHsTyvar doc tyvar = lookupOccRn tyvar
\end{code}


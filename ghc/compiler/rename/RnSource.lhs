%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnSource ( 
	rnSrcDecls, checkModDeprec,
	rnTyClDecl, rnIfaceRuleDecl, rnInstDecl, 
	rnBinds, rnBindsAndThen, rnStats,
    ) where

#include "HsVersions.h"

import HsSyn
import RdrName		( RdrName, isRdrDataCon, elemRdrEnv )
import RdrHsSyn		( RdrNameConDecl, RdrNameTyClDecl, 
			  RdrNameDeprecation, RdrNameFixitySig,
			  RdrNameHsBinds,
			  extractGenericPatTyVars
			)
import RnHsSyn
import HsCore
import RnExpr		( rnExpr )
import RnTypes		( rnHsType, rnHsSigType, rnHsTypeFVs, rnContext )

import RnBinds		( rnTopMonoBinds, rnMonoBinds, rnMethodBinds, 
			  rnMonoBindsAndThen, renameSigs, checkSigs )
import RnEnv		( lookupTopBndrRn, lookupOccRn, lookupSysBndr,
			  newLocalsRn, lookupGlobalOccRn,
			  bindLocalsFV, bindPatSigTyVarsFV,
			  bindTyVarsRn, extendTyVarEnvFVRn,
			  bindCoreLocalRn, bindCoreLocalsRn, bindLocalNames,
			  checkDupOrQualNames, checkDupNames, mapFvRn,
			  lookupTopSrcBndr_maybe, lookupTopSrcBndr,
			  dataTcOccs, newIPName, unknownNameErr
			)
import TcRnMonad

import BasicTypes	( FixitySig(..), TopLevelFlag(..)  )
import HscTypes		( ExternalPackageState(..), FixityEnv, 
			  Deprecations(..), plusDeprecs )
import Module		( moduleEnvElts )
import Class		( FunDep, DefMeth (..) )
import TyCon		( DataConDetails(..), visibleDataCons )
import Name		( Name )
import NameSet
import NameEnv
import ErrUtils		( dumpIfSet )
import PrelNames	( newStablePtrName, bindIOName, returnIOName
			  -- dotnet interop
		 	, objectTyConName, 
			, unmarshalObjectName, marshalObjectName
			, unmarshalStringName, marshalStringName
			, checkDotnetResName
			)
import List		( partition )
import Bag		( bagToList )
import Outputable
import SrcLoc		( SrcLoc )
import CmdLineOpts	( DynFlag(..) )
				-- Warn of unused for-all'd tyvars
import Maybes		( maybeToBool, seqMaybe )
import Maybe            ( maybe, catMaybes, isNothing )
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
rnSrcDecls :: HsGroup RdrName -> RnM (TcGblEnv, HsGroup Name, DefUses)

rnSrcDecls (HsGroup { hs_valds  = MonoBind binds sigs _,
		      hs_tyclds = tycl_decls,
		      hs_instds = inst_decls,
		      hs_fixds  = fix_decls,
		      hs_depds  = deprec_decls,
		      hs_fords  = foreign_decls,
		      hs_defds  = default_decls,
		      hs_ruleds = rule_decls,
		      hs_coreds = core_decls })

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
	(rn_val_decls, bind_dus) <- rnTopMonoBinds binds sigs ;

		-- You might think that we could build proper def/use information
		-- for type and class declarations, but they can be involved
		-- in mutual recursion across modules, and we only do the SCC
		-- analysis for them in the type checker.
		-- So we content ourselves with gathering uses only; that
		-- means we'll only report a declaration as unused if it isn't
		-- mentioned at all.  Ah well.
	(rn_tycl_decls,    src_fvs1) <- mapFvRn rnSrcTyClDecl tycl_decls ;
	(rn_inst_decls,    src_fvs2) <- mapFvRn rnSrcInstDecl inst_decls ;
	(rn_rule_decls,    src_fvs3) <- mapFvRn rnHsRuleDecl rule_decls ;
	(rn_foreign_decls, src_fvs4) <- mapFvRn rnHsForeignDecl foreign_decls ;
	(rn_default_decls, src_fvs5) <- mapFvRn rnDefaultDecl default_decls ;
	(rn_core_decls,    src_fvs6) <- mapFvRn rnCoreDecl core_decls ;
	
	let {
	   rn_group = HsGroup { hs_valds  = rn_val_decls,
			    	hs_tyclds = rn_tycl_decls,
			    	hs_instds = rn_inst_decls,
			    	hs_fixds  = [],
			    	hs_depds  = [],
			    	hs_fords  = rn_foreign_decls,
			    	hs_defds  = rn_default_decls,
			    	hs_ruleds = rn_rule_decls,
			    	hs_coreds = rn_core_decls } ;

	   other_fvs = plusFVs [src_fvs1, src_fvs2, src_fvs3, 
				src_fvs4, src_fvs5, src_fvs6] ;
	   src_dus = bind_dus `plusDU` usesOnly other_fvs 
	} ;

	tcg_env <- getGblEnv ;
	return (tcg_env, rn_group, src_dus)
    }}}
\end{code}


%*********************************************************
%*						 	 *
	Source-code fixity declarations
%*							 *
%*********************************************************

\begin{code}
rnSrcFixityDecls :: [RdrNameFixitySig] -> TcRn m FixityEnv
rnSrcFixityDecls fix_decls
  = getGblEnv					`thenM` \ gbl_env ->
    foldlM rnFixityDecl (tcg_fix_env gbl_env) 
	    fix_decls				`thenM` \ fix_env ->
    traceRn (text "fixity env" <+> ppr fix_env)	`thenM_`
    returnM fix_env

rnFixityDecl :: FixityEnv -> RdrNameFixitySig -> TcRn m FixityEnv
rnFixityDecl fix_env (FixitySig rdr_name fixity loc)
  =	-- GHC extension: look up both the tycon and data con 
	-- for con-like things
	-- If neither are in scope, report an error; otherwise
	-- add both to the fixity env
     mappM lookupTopSrcBndr_maybe (dataTcOccs rdr_name)	`thenM` \ maybe_ns ->
     case catMaybes maybe_ns of
	  [] -> addSrcLoc loc 			$
		addErr (unknownNameErr rdr_name)	`thenM_`
	        returnM fix_env
	  ns -> foldlM add fix_env ns
  where
    add fix_env name 
      = case lookupNameEnv fix_env name of
          Just (FixitySig _ _ loc') -> addErr (dupFixityDecl rdr_name loc loc')	`thenM_`
    				       returnM fix_env
    	  Nothing -> returnM (extendNameEnv fix_env name (FixitySig name fixity loc))

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
rnSrcDeprecDecls :: [RdrNameDeprecation] -> TcRn m Deprecations
rnSrcDeprecDecls [] 
  = returnM NoDeprecs

rnSrcDeprecDecls decls
  = mappM rn_deprec decls	`thenM` \ pairs ->
    returnM (DeprecSome (mkNameEnv (catMaybes pairs)))
 where
   rn_deprec (Deprecation rdr_name txt loc)
     = addSrcLoc loc			$
       lookupTopSrcBndr rdr_name	`thenM` \ name ->
       returnM (Just (name, (name,txt)))

checkModDeprec :: Maybe DeprecTxt -> Deprecations
-- Check for a module deprecation; done once at top level
checkModDeprec Nothing    = NoDeprecs
checkModdeprec (Just txt) = DeprecAll txt

badDeprec d
  = sep [ptext SLIT("Illegal deprecation when whole module is deprecated"),
	 nest 4 (ppr d)]
\end{code}

%*********************************************************
%*							*
\subsection{Source code declarations}
%*							*
%*********************************************************

\begin{code}
rnSrcTyClDecl tycl_decl
  = rnTyClDecl tycl_decl			`thenM` \ new_decl ->
    finishSourceTyClDecl tycl_decl new_decl	`thenM` \ (new_decl', fvs) ->
    returnM (new_decl', fvs `plusFV` tyClDeclFVs new_decl')

rnSrcInstDecl inst
  = rnInstDecl inst			`thenM` \ new_inst ->
    finishSourceInstDecl inst new_inst	`thenM` \ (new_inst', fvs) ->
    returnM (new_inst', fvs `plusFV` instDeclFVs new_inst')

rnDefaultDecl (DefaultDecl tys src_loc)
  = addSrcLoc src_loc $
    mapFvRn (rnHsTypeFVs doc_str) tys		`thenM` \ (tys', fvs) ->
    returnM (DefaultDecl tys' src_loc, fvs)
  where
    doc_str = text "In a `default' declaration"


rnCoreDecl (CoreDecl name ty rhs loc)
  = addSrcLoc loc $
    lookupTopBndrRn name		`thenM` \ name' ->
    rnHsTypeFVs doc_str ty		`thenM` \ (ty', ty_fvs) ->
    rnCoreExpr rhs                      `thenM` \ rhs' ->
    returnM (CoreDecl name' ty' rhs' loc, 
	     ty_fvs `plusFV` ufExprFVs rhs')
  where
    doc_str = text "In the Core declaration for" <+> quotes (ppr name)
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
\end{code}


%************************************************************************
%*									*
\subsubsection{@rnIPBinds@s: in implicit parameter bindings}		*
%*									*
%************************************************************************

\begin{code}
rnIPBinds [] = returnM ([], emptyFVs)
rnIPBinds ((n, expr) : binds)
  = newIPName n			`thenM` \ name ->
    rnExpr expr			`thenM` \ (expr',fvExpr) ->
    rnIPBinds binds		`thenM` \ (binds',fvBinds) ->
    returnM ((name, expr') : binds', fvExpr `plusFV` fvBinds)
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
    returnM (ForeignImport name' ty' spec isDeprec src_loc, 
	      fvs `plusFV` extras spec)
  where
    extras (CImport _ _ _ _ CWrapper) 
      = mkFVs [ newStablePtrName
	      , bindIOName
	      , returnIOName
	      ]
    extras (DNImport _)               
      = mkFVs [ bindIOName
              , objectTyConName
	      , unmarshalObjectName
	      , marshalObjectName
	      , marshalStringName
	      , unmarshalStringName
	      , checkDotnetResName
	      ]
    extras _			      = emptyFVs

rnHsForeignDecl (ForeignExport name ty spec isDeprec src_loc)
  = addSrcLoc src_loc 			$
    lookupOccRn name		        	`thenM` \ name' ->
    rnHsTypeFVs (fo_decl_msg name) ty  		`thenM` \ (ty', fvs) ->
    returnM (ForeignExport name' ty' spec isDeprec src_loc, 
	      mkFVs [name', bindIOName, returnIOName] `plusFV` fvs )
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
rnInstDecl (InstDecl inst_ty mbinds uprags maybe_dfun_rdr_name src_loc)
	-- Used for both source and interface file decls
  = addSrcLoc src_loc $
    rnHsSigType (text "an instance decl") inst_ty	`thenM` \ inst_ty' ->

    (case maybe_dfun_rdr_name of
	Nothing		   -> returnM Nothing
	Just dfun_rdr_name -> lookupGlobalOccRn dfun_rdr_name	`thenM` \ dfun_name ->
			      returnM (Just dfun_name)
    )							`thenM` \ maybe_dfun_name ->

    -- The typechecker checks that all the bindings are for the right class.
    returnM (InstDecl inst_ty' EmptyMonoBinds [] maybe_dfun_name src_loc)

-- Compare finishSourceTyClDecl
finishSourceInstDecl (InstDecl _       mbinds uprags _               _      )
		     (InstDecl inst_ty _      _      maybe_dfun_name src_loc)
	-- Used for both source decls only
  = ASSERT( not (maybeToBool maybe_dfun_name) )	-- Source decl!
    let
	meth_doc    = text "In the bindings in an instance declaration"
	meth_names  = collectLocatedMonoBinders mbinds
	(inst_tyvars, _, cls,_) = splitHsInstDeclTy inst_ty
	-- (Slightly strangely) the forall-d tyvars scope over
	-- the method bindings too
    in

	-- Rename the bindings
	-- NB meth_names can be qualified!
    checkDupNames meth_doc meth_names 		`thenM_`
    extendTyVarEnvForMethodBinds inst_tyvars (		
	rnMethodBinds cls [] mbinds
    )						`thenM` \ (mbinds', meth_fvs) ->
    let 
	binders = collectMonoBinders mbinds'
    in
	-- Rename the prags and signatures.
	-- Note that the type variables are not in scope here,
	-- so that	instance Eq a => Eq (T a) where
	--			{-# SPECIALISE instance Eq a => Eq (T [a]) #-}
	-- works OK. 
	--
	-- But the (unqualified) method names are in scope
    bindLocalNames binders (renameSigs uprags)			`thenM` \ uprags' ->
    checkSigs (okInstDclSig (mkNameSet binders)) uprags'	`thenM_`

    returnM (InstDecl inst_ty mbinds' uprags' maybe_dfun_name src_loc,
	      meth_fvs `plusFV` hsSigsFVs uprags')
\end{code}

%*********************************************************
%*							*
\subsection{Rules}
%*							*
%*********************************************************

\begin{code}
rnIfaceRuleDecl (IfaceRule rule_name act vars fn args rhs src_loc)
  = addSrcLoc src_loc	$
    lookupOccRn fn		`thenM` \ fn' ->
    rnCoreBndrs vars		$ \ vars' ->
    mappM rnCoreExpr args	`thenM` \ args' ->
    rnCoreExpr rhs		`thenM` \ rhs' ->
    returnM (IfaceRule rule_name act vars' fn' args' rhs' src_loc)

rnIfaceRuleDecl (IfaceRuleOut fn rule)		-- Builtin rules come this way
  = lookupOccRn fn		`thenM` \ fn' ->
    returnM (IfaceRuleOut fn' rule)

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
rnTyClDecl (IfaceSig {tcdName = name, tcdType = ty, tcdIdInfo = id_infos, tcdLoc = loc})
  = addSrcLoc loc $
    lookupTopBndrRn name		`thenM` \ name' ->
    rnHsType doc_str ty			`thenM` \ ty' ->
    mappM rnIdInfo id_infos		`thenM` \ id_infos' -> 
    returnM (IfaceSig {tcdName = name', tcdType = ty', tcdIdInfo = id_infos', tcdLoc = loc})
  where
    doc_str = text "In the interface signature for" <+> quotes (ppr name)

rnTyClDecl (ForeignType {tcdName = name, tcdFoType = fo_type, tcdExtName = ext_name, tcdLoc = loc})
  = addSrcLoc loc 			$
    lookupTopBndrRn name		`thenM` \ name' ->
    returnM (ForeignType {tcdName = name', tcdFoType = fo_type, tcdExtName = ext_name, tcdLoc = loc})

rnTyClDecl (TyData {tcdND = new_or_data, tcdCtxt = context, tcdName = tycon,
		    tcdTyVars = tyvars, tcdCons = condecls, tcdGeneric = want_generic,
		    tcdDerivs = derivs, tcdLoc = src_loc})
  = addSrcLoc src_loc $
    lookupTopBndrRn tycon		    	`thenM` \ tycon' ->
    bindTyVarsRn data_doc tyvars		$ \ tyvars' ->
    rnContext data_doc context 			`thenM` \ context' ->
    rn_derivs derivs 				`thenM` \ derivs' ->
    checkDupOrQualNames data_doc con_names	`thenM_`

    rnConDecls tycon' condecls			`thenM` \ condecls' ->
    returnM (TyData {tcdND = new_or_data, tcdCtxt = context', tcdName = tycon',
		     tcdTyVars = tyvars', tcdCons = condecls', tcdGeneric = want_generic,
		     tcdDerivs = derivs', tcdLoc = src_loc})
  where
    data_doc = text "In the data type declaration for" <+> quotes (ppr tycon)
    con_names = map conDeclName (visibleDataCons condecls)

    rn_derivs Nothing   = returnM Nothing
    rn_derivs (Just ds) = rnContext data_doc ds	`thenM` \ ds' -> returnM (Just ds')
    
rnTyClDecl (TySynonym {tcdName = name, tcdTyVars = tyvars, tcdSynRhs = ty, tcdLoc = src_loc})
  = addSrcLoc src_loc $
    lookupTopBndrRn name			`thenM` \ name' ->
    bindTyVarsRn syn_doc tyvars 		$ \ tyvars' ->
    rnHsType syn_doc ty				`thenM` \ ty' ->
    returnM (TySynonym {tcdName = name', tcdTyVars = tyvars', tcdSynRhs = ty', tcdLoc = src_loc})
  where
    syn_doc = text "In the declaration for type synonym" <+> quotes (ppr name)

rnTyClDecl (ClassDecl {tcdCtxt = context, tcdName = cname, 
		       tcdTyVars = tyvars, tcdFDs = fds, tcdSigs = sigs, 
		       tcdLoc = src_loc})
	-- Used for both source and interface file decls
  = addSrcLoc src_loc $

    lookupTopBndrRn cname			`thenM` \ cname' ->

	-- Tyvars scope over superclass context and method signatures
    bindTyVarsRn cls_doc tyvars			$ \ tyvars' ->

	-- Check the superclasses
    rnContext cls_doc context			`thenM` \ context' ->

	-- Check the functional dependencies
    rnFds cls_doc fds				`thenM` \ fds' ->

	-- Check the signatures
	-- First process the class op sigs (op_sigs), then the fixity sigs (non_op_sigs).
    let
	(op_sigs, non_op_sigs) = partition isClassOpSig sigs
	sig_rdr_names_w_locs   = [(op,locn) | ClassOpSig op _ _ locn <- sigs]
    in
    checkDupOrQualNames sig_doc sig_rdr_names_w_locs	`thenM_` 
    mappM (rnClassOp cname' fds') op_sigs		`thenM` \ sigs' ->
    renameSigs non_op_sigs				`thenM` \ non_ops' ->
    checkSigs okClsDclSig  non_ops'			`thenM_`
	-- Typechecker is responsible for checking that we only
	-- give default-method bindings for things in this class.
	-- The renamer *could* check this for class decls, but can't
	-- for instance decls.

    returnM (ClassDecl { tcdCtxt = context', tcdName = cname', tcdTyVars = tyvars',
			 tcdFDs = fds', tcdSigs = non_ops' ++ sigs', tcdMeths = Nothing, 
			 tcdLoc = src_loc})
  where
    cls_doc  = text "In the declaration for class" 	<+> ppr cname
    sig_doc  = text "In the signatures for class"  	<+> ppr cname

rnClassOp clas clas_fds sig@(ClassOpSig op dm_stuff ty locn)
  = addSrcLoc locn $
    lookupTopBndrRn op			`thenM` \ op_name ->
    
    	-- Check the signature
    rnHsSigType (quotes (ppr op)) ty	`thenM` \ new_ty ->
    
    	-- Make the default-method name
    (case dm_stuff of 
        DefMeth dm_rdr_name
    	    -> 	-- Imported class that has a default method decl
    	    	lookupSysBndr dm_rdr_name 	`thenM` \ dm_name ->
		returnM (DefMeth dm_name)
	    		-- An imported class decl for a class decl that had an explicit default
	    		-- method, mentions, rather than defines,
	    		-- the default method, so we must arrange to pull it in

        GenDefMeth -> returnM GenDefMeth
        NoDefMeth  -> returnM NoDefMeth
    )						`thenM` \ dm_stuff' ->
    
    returnM (ClassOpSig op_name dm_stuff' new_ty locn)

finishSourceTyClDecl :: RdrNameTyClDecl -> RenamedTyClDecl -> RnM (RenamedTyClDecl, FreeVars)
	-- Used for source file decls only
	-- Renames the default-bindings of a class decl
finishSourceTyClDecl (ClassDecl {tcdMeths = Just mbinds, tcdLoc = src_loc})	-- Get mbinds from here
	 rn_cls_decl@(ClassDecl {tcdName = cls, tcdTyVars = tyvars})		-- Everything else is here
  -- There are some default-method bindings (abeit possibly empty) so 
  -- this is a source-code class declaration
  = 	-- The newLocals call is tiresome: given a generic class decl
	--	class C a where
	--	  op :: a -> a
	--	  op {| x+y |} (Inl a) = ...
	--	  op {| x+y |} (Inr b) = ...
	--	  op {| a*b |} (a*b)   = ...
	-- we want to name both "x" tyvars with the same unique, so that they are
	-- easy to group together in the typechecker.  
	-- Hence the 
    addSrcLoc src_loc				$
    extendTyVarEnvForMethodBinds tyvars			$
    getLocalRdrEnv					`thenM` \ name_env ->
    let
	meth_rdr_names_w_locs = collectLocatedMonoBinders mbinds
	gen_rdr_tyvars_w_locs = [(tv,src_loc) | tv <- extractGenericPatTyVars mbinds,
						not (tv `elemRdrEnv` name_env)]
    in
    checkDupOrQualNames meth_doc meth_rdr_names_w_locs	`thenM_`
    newLocalsRn gen_rdr_tyvars_w_locs			`thenM` \ gen_tyvars ->
    rnMethodBinds cls gen_tyvars mbinds			`thenM` \ (mbinds', meth_fvs) ->
    returnM (rn_cls_decl {tcdMeths = Just mbinds'}, meth_fvs)
  where
    meth_doc = text "In the default-methods for class"	<+> ppr (tcdName rn_cls_decl)

finishSourceTyClDecl _ tycl_decl@(TyData {tcdDerivs = derivings})
  -- Derivings are returned here so that they don't form part of the tyClDeclFVs.
  -- This is important, because tyClDeclFVs should contain only the
  -- FVs that are `needed' by the interface file declaration, and
  -- derivings do not appear in this.  It also means that the tcGroups
  -- are smaller, which turned out to be important for the usage inference. KSW 2002-02.
  = returnM (tycl_decl,
              maybe emptyFVs extractHsCtxtTyNames derivings)

finishSourceTyClDecl _ tycl_decl = returnM (tycl_decl, emptyFVs)
	-- Not a class declaration
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
\subsection{Support code for type/data declarations}
%*							*
%*********************************************************

\begin{code}
conDeclName :: RdrNameConDecl -> (RdrName, SrcLoc)
conDeclName (ConDecl n _ _ _ l) = (n,l)

rnConDecls :: Name -> DataConDetails RdrNameConDecl -> RnM (DataConDetails RenamedConDecl)
rnConDecls tycon Unknown     = returnM Unknown
rnConDecls tycon (HasCons n) = returnM (HasCons n)
rnConDecls tycon (DataCons condecls)
  = 	-- Check that there's at least one condecl,
	-- or else we're reading an interface file, or -fglasgow-exts
    (if null condecls then
	doptM Opt_GlasgowExts	`thenM` \ glaExts ->
	getModeRn		`thenM` \ mode ->
	checkErr (glaExts || isInterfaceMode mode)
		(emptyConDeclsErr tycon)
     else returnM ()
    )						`thenM_` 

    mappM rnConDecl condecls			`thenM` \ condecls' ->
    returnM (DataCons condecls')

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
  = checkDupOrQualNames doc field_names	`thenM_`
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

checkConName name
  = checkErr (isRdrDataCon name) (badDataCon name)
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

%*********************************************************
%*							 *
\subsection{IdInfo}
%*							 *
%*********************************************************

\begin{code}
rnIdInfo (HsWorker worker arity)
  = lookupOccRn worker			`thenM` \ worker' ->
    returnM (HsWorker worker' arity)

rnIdInfo (HsUnfold inline expr)	= rnCoreExpr expr `thenM` \ expr' ->
				  returnM (HsUnfold inline expr')
rnIdInfo (HsStrictness str)     = returnM (HsStrictness str)
rnIdInfo (HsArity arity)	= returnM (HsArity arity)
rnIdInfo HsNoCafRefs		= returnM HsNoCafRefs
\end{code}

@UfCore@ expressions.

\begin{code}
rnCoreExpr (UfType ty)
  = rnHsType (text "unfolding type") ty	`thenM` \ ty' ->
    returnM (UfType ty')

rnCoreExpr (UfVar v)
  = lookupOccRn v 	`thenM` \ v' ->
    returnM (UfVar v')

rnCoreExpr (UfLit l)
  = returnM (UfLit l)

rnCoreExpr (UfFCall cc ty)
  = rnHsType (text "ccall") ty	`thenM` \ ty' ->
    returnM (UfFCall cc ty')

rnCoreExpr (UfTuple (HsTupCon boxity arity) args) 
  = mappM rnCoreExpr args		`thenM` \ args' ->
    returnM (UfTuple (HsTupCon boxity arity) args')

rnCoreExpr (UfApp fun arg)
  = rnCoreExpr fun		`thenM` \ fun' ->
    rnCoreExpr arg		`thenM` \ arg' ->
    returnM (UfApp fun' arg')

rnCoreExpr (UfCase scrut bndr alts)
  = rnCoreExpr scrut			`thenM` \ scrut' ->
    bindCoreLocalRn bndr		$ \ bndr' ->
    mappM rnCoreAlt alts		`thenM` \ alts' ->
    returnM (UfCase scrut' bndr' alts')

rnCoreExpr (UfNote note expr) 
  = rnNote note			`thenM` \ note' ->
    rnCoreExpr expr		`thenM` \ expr' ->
    returnM  (UfNote note' expr')

rnCoreExpr (UfLam bndr body)
  = rnCoreBndr bndr 		$ \ bndr' ->
    rnCoreExpr body		`thenM` \ body' ->
    returnM (UfLam bndr' body')

rnCoreExpr (UfLet (UfNonRec bndr rhs) body)
  = rnCoreExpr rhs		`thenM` \ rhs' ->
    rnCoreBndr bndr 		$ \ bndr' ->
    rnCoreExpr body		`thenM` \ body' ->
    returnM (UfLet (UfNonRec bndr' rhs') body')

rnCoreExpr (UfLet (UfRec pairs) body)
  = rnCoreBndrs bndrs		$ \ bndrs' ->
    mappM rnCoreExpr rhss	`thenM` \ rhss' ->
    rnCoreExpr body		`thenM` \ body' ->
    returnM (UfLet (UfRec (bndrs' `zip` rhss')) body')
  where
    (bndrs, rhss) = unzip pairs
\end{code}

\begin{code}
rnCoreBndr (UfValBinder name ty) thing_inside
  = rnHsType doc ty		`thenM` \ ty' ->
    bindCoreLocalRn name	$ \ name' ->
    thing_inside (UfValBinder name' ty')
  where
    doc = text "unfolding id"
    
rnCoreBndr (UfTyBinder name kind) thing_inside
  = bindCoreLocalRn name		$ \ name' ->
    thing_inside (UfTyBinder name' kind)
    
rnCoreBndrs []     thing_inside = thing_inside []
rnCoreBndrs (b:bs) thing_inside = rnCoreBndr b		$ \ name' ->
				  rnCoreBndrs bs 	$ \ names' ->
				  thing_inside (name':names')
\end{code}    

\begin{code}
rnCoreAlt (con, bndrs, rhs)
  = rnUfCon con 			`thenM` \ con' ->
    bindCoreLocalsRn bndrs		$ \ bndrs' ->
    rnCoreExpr rhs			`thenM` \ rhs' ->
    returnM (con', bndrs', rhs')

rnNote (UfCoerce ty)
  = rnHsType (text "unfolding coerce") ty	`thenM` \ ty' ->
    returnM (UfCoerce ty')

rnNote (UfSCC cc)   = returnM (UfSCC cc)
rnNote UfInlineCall = returnM UfInlineCall
rnNote UfInlineMe   = returnM UfInlineMe
rnNote (UfCoreNote s) = returnM (UfCoreNote s)

rnUfCon UfDefault
  = returnM UfDefault

rnUfCon (UfTupleAlt tup_con)
  = returnM (UfTupleAlt tup_con)

rnUfCon (UfDataAlt con)
  = lookupOccRn con		`thenM` \ con' ->
    returnM (UfDataAlt con')

rnUfCon (UfLitAlt lit)
  = returnM (UfLitAlt lit)
\end{code}

%*********************************************************
%*							*
\subsection{Statistics}
%*							*
%*********************************************************

\begin{code}
rnStats :: [RenamedHsDecl]	-- Imported decls
	-> TcRn m ()
rnStats imp_decls
  = doptM Opt_D_dump_rn_trace 	`thenM` \ dump_rn_trace ->
    doptM Opt_D_dump_rn_stats 	`thenM` \ dump_rn_stats ->
    doptM Opt_D_dump_rn 	`thenM` \ dump_rn ->
    getEps			`thenM` \ eps ->

    ioToTcRn (dumpIfSet (dump_rn_trace || dump_rn_stats || dump_rn)
		        "Renamer statistics"
		        (getRnStats eps imp_decls))	`thenM_`
    returnM ()

getRnStats :: ExternalPackageState -> [RenamedHsDecl] -> SDoc
getRnStats eps imported_decls
  = hcat [text "Renamer stats: ", stats]
  where
    n_mods = length [() | _ <- moduleEnvElts (eps_PIT eps)]
	-- This is really only right for a one-shot compile

    (decls_map, n_decls_slurped) = eps_decls eps
    
    n_decls_left   = length [decl | (avail, True, (_,decl)) <- nameEnvElts decls_map
    			-- Data, newtype, and class decls are in the decls_fm
    			-- under multiple names; the tycon/class, and each
    			-- constructor/class op too.
    			-- The 'True' selects just the 'main' decl
    		     ]
    
    (insts_left, n_insts_slurped) = eps_insts eps
    n_insts_left  = length (bagToList insts_left)
    
    (rules_left, n_rules_slurped) = eps_rules eps
    n_rules_left  = length (bagToList rules_left)
    
    stats = vcat 
    	[int n_mods <+> text "interfaces read",
    	 hsep [ int n_decls_slurped, text "type/class/variable imported, out of", 
    	        int (n_decls_slurped + n_decls_left), text "read"],
    	 hsep [ int n_insts_slurped, text "instance decls imported, out of",  
    	        int (n_insts_slurped + n_insts_left), text "read"],
    	 hsep [ int n_rules_slurped, text "rule decls imported, out of",  
    	        int (n_rules_slurped + n_rules_left), text "read"]
	]
\end{code}    

%*********************************************************
%*							 *
\subsection{Errors}
%*							 *
%*********************************************************

\begin{code}
badDataCon name
   = hsep [ptext SLIT("Illegal data constructor name"), quotes (ppr name)]

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

emptyConDeclsErr tycon
  = sep [quotes (ppr tycon) <+> ptext SLIT("has no constructors"),
	 nest 4 (ptext SLIT("(-fglasgow-exts permits this)"))]

withWarning
  = sep [quotes (ptext SLIT("with")),
	 ptext SLIT("is deprecated, use"),
	 quotes (ptext SLIT("let")),
	 ptext SLIT("instead")]

badIpBinds binds
  = hang (ptext SLIT("Implicit-parameter bindings illegal in 'mdo':")) 4
	 (ppr binds)
\end{code}


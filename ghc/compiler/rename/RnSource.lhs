%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnSource ( rnTyClDecl, rnIfaceRuleDecl, rnInstDecl, rnSourceDecls, 
	) where

#include "HsVersions.h"

import RnExpr
import HsSyn
import HscTypes		( GlobalRdrEnv, AvailEnv )
import RdrName		( RdrName, isRdrDataCon, elemRdrEnv )
import RdrHsSyn		( RdrNameConDecl, RdrNameTyClDecl,
			  extractGenericPatTyVars
			)
import RnHsSyn
import HsCore

import RnTypes		( rnHsType, rnHsSigType, rnHsTypeFVs, rnContext )

import RnBinds		( rnTopBinds, rnMethodBinds, renameSigs, renameSigsFVs )
import RnEnv		( lookupTopBndrRn, lookupOccRn, lookupIfaceName,
			  lookupSysBinder, newLocalsRn,
			  bindLocalsFVRn, bindPatSigTyVars,
			  bindTyVarsRn, extendTyVarEnvFVRn,
			  bindCoreLocalRn, bindCoreLocalsRn, bindLocalNames,
			  checkDupOrQualNames, checkDupNames, mapFvRn
			)
import RnMonad

import Class		( FunDep, DefMeth (..) )
import TyCon		( DataConDetails(..), visibleDataCons )
import DataCon		( dataConWorkId )
import Name		( Name, NamedThing(..) )
import NameSet
import PrelNames	( deRefStablePtrName, newStablePtrName,
			  bindIOName, returnIOName
			)
import TysWiredIn	( tupleCon )
import List		( partition )
import Outputable
import SrcLoc		( SrcLoc )
import CmdLineOpts	( DynFlag(..) )
				-- Warn of unused for-all'd tyvars
import Maybes		( maybeToBool )
import Maybe            ( maybe )
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


%*********************************************************
%*							*
\subsection{Source code declarations}
%*							*
%*********************************************************

\begin{code}
rnSourceDecls :: GlobalRdrEnv -> AvailEnv -> LocalFixityEnv -> RnMode
	      -> [RdrNameHsDecl] 
	      -> RnMG ([RenamedHsDecl], FreeVars)
	-- The decls get reversed, but that's ok

rnSourceDecls gbl_env avails local_fixity_env mode decls
  = initRnMS gbl_env avails emptyRdrEnv local_fixity_env mode (go emptyFVs [] decls)
  where
	-- Fixity and deprecations have been dealt with already; ignore them
    go fvs ds' []             = returnRn (ds', fvs)
    go fvs ds' (FixD _:ds)    = go fvs ds' ds
    go fvs ds' (DeprecD _:ds) = go fvs ds' ds
    go fvs ds' (d:ds)         = rnSourceDecl d	`thenRn` \(d', fvs') ->
			        go (fvs `plusFV` fvs') (d':ds') ds


rnSourceDecl :: RdrNameHsDecl -> RnMS (RenamedHsDecl, FreeVars)

rnSourceDecl (ValD binds) = rnTopBinds binds	`thenRn` \ (new_binds, fvs) ->
			    returnRn (ValD new_binds, fvs)

rnSourceDecl (TyClD tycl_decl)
  = rnTyClDecl tycl_decl			`thenRn` \ new_decl ->
    finishSourceTyClDecl tycl_decl new_decl	`thenRn` \ (new_decl', fvs) ->
    returnRn (TyClD new_decl', fvs `plusFV` tyClDeclFVs new_decl')

rnSourceDecl (InstD inst)
  = rnInstDecl inst			`thenRn` \ new_inst ->
    finishSourceInstDecl inst new_inst	`thenRn` \ (new_inst', fvs) ->
    returnRn (InstD new_inst', fvs `plusFV` instDeclFVs new_inst')

rnSourceDecl (RuleD rule)
  = rnHsRuleDecl rule		`thenRn` \ (new_rule, fvs) ->
    returnRn (RuleD new_rule, fvs)

rnSourceDecl (ForD ford)
  = rnHsForeignDecl ford		`thenRn` \ (new_ford, fvs) ->
    returnRn (ForD new_ford, fvs)

rnSourceDecl (DefD (DefaultDecl tys src_loc))
  = pushSrcLocRn src_loc $
    mapFvRn (rnHsTypeFVs doc_str) tys		`thenRn` \ (tys', fvs) ->
    returnRn (DefD (DefaultDecl tys' src_loc), fvs)
  where
    doc_str = text "In a `default' declaration"
\end{code}


%*********************************************************
%*							*
\subsection{Foreign declarations}
%*							*
%*********************************************************

\begin{code}
rnHsForeignDecl (ForeignImport name ty spec isDeprec src_loc)
  = pushSrcLocRn src_loc 		$
    lookupTopBndrRn name	        `thenRn` \ name' ->
    rnHsTypeFVs (fo_decl_msg name) ty	`thenRn` \ (ty', fvs) ->
    returnRn (ForeignImport name' ty' spec isDeprec src_loc, 
	      fvs `plusFV` extras spec)
  where
    extras (CImport _ _ _ _ CWrapper) = mkFVs [newStablePtrName,
					       deRefStablePtrName,  
					       bindIOName, returnIOName]
    extras _			      = emptyFVs

rnHsForeignDecl (ForeignExport name ty spec isDeprec src_loc)
  = pushSrcLocRn src_loc 			$
    lookupOccRn name		        	`thenRn` \ name' ->
    rnHsTypeFVs (fo_decl_msg name) ty  		`thenRn` \ (ty', fvs) ->
    returnRn (ForeignExport name' ty' spec isDeprec src_loc, 
	      mkFVs [bindIOName, returnIOName] `plusFV` fvs)

fo_decl_msg name = ptext SLIT("The foreign declaration for") <+> ppr name
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************

\begin{code}
rnInstDecl (InstDecl inst_ty mbinds uprags maybe_dfun_rdr_name src_loc)
	-- Used for both source and interface file decls
  = pushSrcLocRn src_loc $
    rnHsSigType (text "an instance decl") inst_ty	`thenRn` \ inst_ty' ->

    (case maybe_dfun_rdr_name of
	Nothing		   -> returnRn Nothing
	Just dfun_rdr_name -> lookupIfaceName dfun_rdr_name	`thenRn` \ dfun_name ->
			      returnRn (Just dfun_name)
    )							`thenRn` \ maybe_dfun_name ->

    -- The typechecker checks that all the bindings are for the right class.
    returnRn (InstDecl inst_ty' EmptyMonoBinds [] maybe_dfun_name src_loc)

-- Compare finishSourceTyClDecl
finishSourceInstDecl (InstDecl _       mbinds uprags _               _      )
		     (InstDecl inst_ty _      _      maybe_dfun_name src_loc)
	-- Used for both source decls only
  = ASSERT( not (maybeToBool maybe_dfun_name) )	-- Source decl!
    let
	meth_doc    = text "In the bindings in an instance declaration"
	meth_names  = collectLocatedMonoBinders mbinds
	(inst_tyvars, (cls,_)) = getHsInstHead inst_ty
	-- (Slightly strangely) the forall-d tyvars scope over
	-- the method bindings too
    in

	-- Rename the bindings
	-- NB meth_names can be qualified!
    checkDupNames meth_doc meth_names 		`thenRn_`
    extendTyVarEnvFVRn (map hsTyVarName inst_tyvars) (		
	rnMethodBinds cls [] mbinds
    )						`thenRn` \ (mbinds', meth_fvs) ->
    let 
	binders    = collectMonoBinders mbinds'
	binder_set = mkNameSet binders
    in
	-- Rename the prags and signatures.
	-- Note that the type variables are not in scope here,
	-- so that	instance Eq a => Eq (T a) where
	--			{-# SPECIALISE instance Eq a => Eq (T [a]) #-}
	-- works OK. 
	--
	-- But the (unqualified) method names are in scope
    bindLocalNames binders (
       renameSigsFVs (okInstDclSig binder_set) uprags
    )							`thenRn` \ (uprags', prag_fvs) ->

    returnRn (InstDecl inst_ty mbinds' uprags' maybe_dfun_name src_loc,
	      meth_fvs `plusFV` prag_fvs)
\end{code}

%*********************************************************
%*							*
\subsection{Rules}
%*							*
%*********************************************************

\begin{code}
rnIfaceRuleDecl (IfaceRule rule_name act vars fn args rhs src_loc)
  = pushSrcLocRn src_loc	$
    lookupOccRn fn		`thenRn` \ fn' ->
    rnCoreBndrs vars		$ \ vars' ->
    mapRn rnCoreExpr args	`thenRn` \ args' ->
    rnCoreExpr rhs		`thenRn` \ rhs' ->
    returnRn (IfaceRule rule_name act vars' fn' args' rhs' src_loc)

rnIfaceRuleDecl (IfaceRuleOut fn rule)		-- Builtin rules come this way
  = lookupOccRn fn		`thenRn` \ fn' ->
    returnRn (IfaceRuleOut fn' rule)

rnHsRuleDecl (HsRule rule_name act vars lhs rhs src_loc)
  = pushSrcLocRn src_loc				$
    bindPatSigTyVars (collectRuleBndrSigTys vars)	$

    bindLocalsFVRn doc (map get_var vars)	$ \ ids ->
    mapFvRn rn_var (vars `zip` ids)		`thenRn` \ (vars', fv_vars) ->

    rnExpr lhs					`thenRn` \ (lhs', fv_lhs) ->
    rnExpr rhs					`thenRn` \ (rhs', fv_rhs) ->
    checkRn (validRuleLhs ids lhs')
	    (badRuleLhsErr rule_name lhs')	`thenRn_`
    let
	bad_vars = [var | var <- ids, not (var `elemNameSet` fv_lhs)]
    in
    mapRn (addErrRn . badRuleVar rule_name) bad_vars	`thenRn_`
    returnRn (HsRule rule_name act vars' lhs' rhs' src_loc,
	      fv_vars `plusFV` fv_lhs `plusFV` fv_rhs)
  where
    doc = text "In the transformation rule" <+> ptext rule_name
  
    get_var (RuleBndr v)      = v
    get_var (RuleBndrSig v _) = v

    rn_var (RuleBndr v, id)	 = returnRn (RuleBndr id, emptyFVs)
    rn_var (RuleBndrSig v t, id) = rnHsTypeFVs doc t	`thenRn` \ (t', fvs) ->
				   returnRn (RuleBndrSig id t', fvs)
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
  = pushSrcLocRn loc $
    lookupTopBndrRn name		`thenRn` \ name' ->
    rnHsType doc_str ty			`thenRn` \ ty' ->
    mapRn rnIdInfo id_infos		`thenRn` \ id_infos' -> 
    returnRn (IfaceSig {tcdName = name', tcdType = ty', tcdIdInfo = id_infos', tcdLoc = loc})
  where
    doc_str = text "In the interface signature for" <+> quotes (ppr name)

rnTyClDecl (CoreDecl {tcdName = name, tcdType = ty, tcdRhs = rhs, tcdLoc = loc})
  = pushSrcLocRn loc $
    lookupTopBndrRn name		`thenRn` \ name' ->
    rnHsType doc_str ty			`thenRn` \ ty' ->
    rnCoreExpr rhs                      `thenRn` \ rhs' ->
    returnRn (CoreDecl {tcdName = name', tcdType = ty', tcdRhs = rhs', tcdLoc = loc})
  where
    doc_str = text "In the Core declaration for" <+> quotes (ppr name)

rnTyClDecl (ForeignType {tcdName = name, tcdFoType = fo_type, tcdExtName = ext_name, tcdLoc = loc})
  = pushSrcLocRn loc 			$
    lookupTopBndrRn name		`thenRn` \ name' ->
    returnRn (ForeignType {tcdName = name', tcdFoType = fo_type, tcdExtName = ext_name, tcdLoc = loc})

rnTyClDecl (TyData {tcdND = new_or_data, tcdCtxt = context, tcdName = tycon,
		    tcdTyVars = tyvars, tcdCons = condecls, 
		    tcdDerivs = derivs, tcdLoc = src_loc, tcdSysNames = sys_names})
  = pushSrcLocRn src_loc $
    lookupTopBndrRn tycon		    	`thenRn` \ tycon' ->
    bindTyVarsRn data_doc tyvars		$ \ tyvars' ->
    rnContext data_doc context 			`thenRn` \ context' ->
    rn_derivs derivs 				`thenRn` \ derivs' ->
    checkDupOrQualNames data_doc con_names	`thenRn_`

    rnConDecls tycon' condecls			`thenRn` \ condecls' ->
    mapRn lookupSysBinder sys_names	        `thenRn` \ sys_names' ->
    returnRn (TyData {tcdND = new_or_data, tcdCtxt = context', tcdName = tycon',
		      tcdTyVars = tyvars', tcdCons = condecls', 
		      tcdDerivs = derivs', tcdLoc = src_loc, tcdSysNames = sys_names'})
  where
    data_doc = text "In the data type declaration for" <+> quotes (ppr tycon)
    con_names = map conDeclName (visibleDataCons condecls)

    rn_derivs Nothing   = returnRn Nothing
    rn_derivs (Just ds) = rnContext data_doc ds	`thenRn` \ ds' -> returnRn (Just ds')
    
rnTyClDecl (TySynonym {tcdName = name, tcdTyVars = tyvars, tcdSynRhs = ty, tcdLoc = src_loc})
  = pushSrcLocRn src_loc $
    lookupTopBndrRn name			`thenRn` \ name' ->
    bindTyVarsRn syn_doc tyvars 		$ \ tyvars' ->
    rnHsType syn_doc ty				`thenRn` \ ty' ->
    returnRn (TySynonym {tcdName = name', tcdTyVars = tyvars', tcdSynRhs = ty', tcdLoc = src_loc})
  where
    syn_doc = text "In the declaration for type synonym" <+> quotes (ppr name)

rnTyClDecl (ClassDecl {tcdCtxt = context, tcdName = cname, 
		       tcdTyVars = tyvars, tcdFDs = fds, tcdSigs = sigs, 
		       tcdSysNames = names, tcdLoc = src_loc})
	-- Used for both source and interface file decls
  = pushSrcLocRn src_loc $

    lookupTopBndrRn cname			`thenRn` \ cname' ->

	-- Deal with the implicit tycon and datacon name
	-- They aren't in scope (because they aren't visible to the user)
	-- and what we want to do is simply look them up in the cache;
	-- we jolly well ought to get a 'hit' there!
    mapRn lookupSysBinder names			`thenRn` \ names' ->

	-- Tyvars scope over bindings and context
    bindTyVarsRn cls_doc tyvars			$ \ tyvars' ->

	-- Check the superclasses
    rnContext cls_doc context			`thenRn` \ context' ->

	-- Check the functional dependencies
    rnFds cls_doc fds				`thenRn` \ fds' ->

	-- Check the signatures
	-- First process the class op sigs (op_sigs), then the fixity sigs (non_op_sigs).
    let
	(op_sigs, non_op_sigs) = partition isClassOpSig sigs
	sig_rdr_names_w_locs   = [(op,locn) | ClassOpSig op _ _ locn <- sigs]
    in
    checkDupOrQualNames sig_doc sig_rdr_names_w_locs	`thenRn_` 
    mapRn (rnClassOp cname' fds') op_sigs		`thenRn` \ sigs' ->
    let
	binders = mkNameSet [ nm | (ClassOpSig nm _ _ _) <- sigs' ]
    in
    renameSigs (okClsDclSig binders) non_op_sigs	  `thenRn` \ non_ops' ->

	-- Typechecker is responsible for checking that we only
	-- give default-method bindings for things in this class.
	-- The renamer *could* check this for class decls, but can't
	-- for instance decls.

    returnRn (ClassDecl { tcdCtxt = context', tcdName = cname', tcdTyVars = tyvars',
			  tcdFDs = fds', tcdSigs = non_ops' ++ sigs', tcdMeths = Nothing, 
			  tcdSysNames = names', tcdLoc = src_loc})
  where
    cls_doc  = text "In the declaration for class" 	<+> ppr cname
    sig_doc  = text "In the signatures for class"  	<+> ppr cname

rnClassOp clas clas_fds sig@(ClassOpSig op dm_stuff ty locn)
  = pushSrcLocRn locn $
    lookupTopBndrRn op			`thenRn` \ op_name ->
    
    	-- Check the signature
    rnHsSigType (quotes (ppr op)) ty	`thenRn` \ new_ty ->
    
    	-- Make the default-method name
    (case dm_stuff of 
        DefMeth dm_rdr_name
    	    -> 	-- Imported class that has a default method decl
    		-- See comments with tname, snames, above
    	    	lookupSysBinder dm_rdr_name 	`thenRn` \ dm_name ->
		returnRn (DefMeth dm_name)
	    		-- An imported class decl for a class decl that had an explicit default
	    		-- method, mentions, rather than defines,
	    		-- the default method, so we must arrange to pull it in

        GenDefMeth -> returnRn GenDefMeth
        NoDefMeth  -> returnRn NoDefMeth
    )						`thenRn` \ dm_stuff' ->
    
    returnRn (ClassOpSig op_name dm_stuff' new_ty locn)

finishSourceTyClDecl :: RdrNameTyClDecl -> RenamedTyClDecl -> RnMS (RenamedTyClDecl, FreeVars)
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
    pushSrcLocRn src_loc				$
    extendTyVarEnvFVRn (map hsTyVarName tyvars)		$
    getLocalNameEnv					`thenRn` \ name_env ->
    let
	meth_rdr_names_w_locs = collectLocatedMonoBinders mbinds
	gen_rdr_tyvars_w_locs = [(tv,src_loc) | tv <- extractGenericPatTyVars mbinds,
						not (tv `elemRdrEnv` name_env)]
    in
    checkDupOrQualNames meth_doc meth_rdr_names_w_locs	`thenRn_`
    newLocalsRn gen_rdr_tyvars_w_locs			`thenRn` \ gen_tyvars ->
    rnMethodBinds cls gen_tyvars mbinds			`thenRn` \ (mbinds', meth_fvs) ->
    returnRn (rn_cls_decl {tcdMeths = Just mbinds'}, meth_fvs)
  where
    meth_doc = text "In the default-methods for class"	<+> ppr (tcdName rn_cls_decl)

finishSourceTyClDecl _ tycl_decl@(TyData {tcdDerivs = derivings})
  -- Derivings are returned here so that they don't form part of the tyClDeclFVs.
  -- This is important, because tyClDeclFVs should contain only the
  -- FVs that are `needed' by the interface file declaration, and
  -- derivings do not appear in this.  It also means that the tcGroups
  -- are smaller, which turned out to be important for the usage inference. KSW 2002-02.
  = returnRn (tycl_decl,
              maybe emptyFVs extractHsCtxtTyNames derivings)

finishSourceTyClDecl _ tycl_decl = returnRn (tycl_decl, emptyFVs)
	-- Not a class declaration
\end{code}


%*********************************************************
%*							*
\subsection{Support code for type/data declarations}
%*							*
%*********************************************************

\begin{code}
conDeclName :: RdrNameConDecl -> (RdrName, SrcLoc)
conDeclName (ConDecl n _ _ _ _ l) = (n,l)

rnConDecls :: Name -> DataConDetails RdrNameConDecl -> RnMS (DataConDetails RenamedConDecl)
rnConDecls tycon Unknown     = returnRn Unknown
rnConDecls tycon (HasCons n) = returnRn (HasCons n)
rnConDecls tycon (DataCons condecls)
  = 	-- Check that there's at least one condecl,
	-- or else we're reading an interface file, or -fglasgow-exts
    (if null condecls then
	doptRn Opt_GlasgowExts	`thenRn` \ glaExts ->
	getModeRn		`thenRn` \ mode ->
	checkRn (glaExts || isInterfaceMode mode)
		(emptyConDeclsErr tycon)
     else returnRn ()
    )						`thenRn_` 

    mapRn rnConDecl condecls			`thenRn` \ condecls' ->
    returnRn (DataCons condecls')

rnConDecl :: RdrNameConDecl -> RnMS RenamedConDecl
rnConDecl (ConDecl name wkr tvs cxt details locn)
  = pushSrcLocRn locn $
    checkConName name		`thenRn_` 
    lookupTopBndrRn name	`thenRn` \ new_name ->

    lookupSysBinder wkr		`thenRn` \ new_wkr ->
	-- See comments with ClassDecl

    bindTyVarsRn doc tvs 		$ \ new_tyvars ->
    rnContext doc cxt			`thenRn` \ new_context ->
    rnConDetails doc locn details	`thenRn` \ new_details -> 
    returnRn (ConDecl new_name new_wkr new_tyvars new_context new_details locn)
  where
    doc = text "In the definition of data constructor" <+> quotes (ppr name)

rnConDetails doc locn (VanillaCon tys)
  = mapRn (rnBangTy doc) tys	`thenRn` \ new_tys  ->
    returnRn (VanillaCon new_tys)

rnConDetails doc locn (InfixCon ty1 ty2)
  = rnBangTy doc ty1  		`thenRn` \ new_ty1 ->
    rnBangTy doc ty2  		`thenRn` \ new_ty2 ->
    returnRn (InfixCon new_ty1 new_ty2)

rnConDetails doc locn (RecCon fields)
  = checkDupOrQualNames doc field_names	`thenRn_`
    mapRn (rnField doc) fields		`thenRn` \ new_fields ->
    returnRn (RecCon new_fields)
  where
    field_names = [(fld, locn) | (flds, _) <- fields, fld <- flds]

rnField doc (names, ty)
  = mapRn lookupTopBndrRn names	`thenRn` \ new_names ->
    rnBangTy doc ty		`thenRn` \ new_ty ->
    returnRn (new_names, new_ty) 

rnBangTy doc (BangType s ty)
  = rnHsType doc ty		`thenRn` \ new_ty ->
    returnRn (BangType s new_ty)

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
  = checkRn (isRdrDataCon name)
	    (badDataCon name)
\end{code}


%*********************************************************
%*							*
\subsection{Support code to rename types}
%*							*
%*********************************************************

\begin{code}
rnFds :: SDoc -> [FunDep RdrName] -> RnMS [FunDep Name]

rnFds doc fds
  = mapRn rn_fds fds
  where
    rn_fds (tys1, tys2)
      =	rnHsTyVars doc tys1		`thenRn` \ tys1' ->
	rnHsTyVars doc tys2		`thenRn` \ tys2' ->
	returnRn (tys1', tys2')

rnHsTyVars doc tvs  = mapRn (rnHsTyvar doc) tvs
rnHsTyvar doc tyvar = lookupOccRn tyvar
\end{code}

%*********************************************************
%*							 *
\subsection{IdInfo}
%*							 *
%*********************************************************

\begin{code}
rnIdInfo (HsWorker worker arity)
  = lookupOccRn worker			`thenRn` \ worker' ->
    returnRn (HsWorker worker' arity)

rnIdInfo (HsUnfold inline expr)	= rnCoreExpr expr `thenRn` \ expr' ->
				  returnRn (HsUnfold inline expr')
rnIdInfo (HsStrictness str)     = returnRn (HsStrictness str)
rnIdInfo (HsArity arity)	= returnRn (HsArity arity)
rnIdInfo HsNoCafRefs		= returnRn HsNoCafRefs
\end{code}

@UfCore@ expressions.

\begin{code}
rnCoreExpr (UfType ty)
  = rnHsType (text "unfolding type") ty	`thenRn` \ ty' ->
    returnRn (UfType ty')

rnCoreExpr (UfVar v)
  = lookupOccRn v 	`thenRn` \ v' ->
    returnRn (UfVar v')

rnCoreExpr (UfLit l)
  = returnRn (UfLit l)

rnCoreExpr (UfLitLit l ty)
  = rnHsType (text "litlit") ty	`thenRn` \ ty' ->
    returnRn (UfLitLit l ty')

rnCoreExpr (UfFCall cc ty)
  = rnHsType (text "ccall") ty	`thenRn` \ ty' ->
    returnRn (UfFCall cc ty')

rnCoreExpr (UfTuple (HsTupCon _ boxity arity) args) 
  = mapRn rnCoreExpr args		`thenRn` \ args' ->
    returnRn (UfTuple (HsTupCon tup_name boxity arity) args')
  where
    tup_name = getName (dataConWorkId (tupleCon boxity arity))
	-- Get the *worker* name and use that

rnCoreExpr (UfApp fun arg)
  = rnCoreExpr fun		`thenRn` \ fun' ->
    rnCoreExpr arg		`thenRn` \ arg' ->
    returnRn (UfApp fun' arg')

rnCoreExpr (UfCase scrut bndr alts)
  = rnCoreExpr scrut			`thenRn` \ scrut' ->
    bindCoreLocalRn bndr		$ \ bndr' ->
    mapRn rnCoreAlt alts		`thenRn` \ alts' ->
    returnRn (UfCase scrut' bndr' alts')

rnCoreExpr (UfNote note expr) 
  = rnNote note			`thenRn` \ note' ->
    rnCoreExpr expr		`thenRn` \ expr' ->
    returnRn  (UfNote note' expr')

rnCoreExpr (UfLam bndr body)
  = rnCoreBndr bndr 		$ \ bndr' ->
    rnCoreExpr body		`thenRn` \ body' ->
    returnRn (UfLam bndr' body')

rnCoreExpr (UfLet (UfNonRec bndr rhs) body)
  = rnCoreExpr rhs		`thenRn` \ rhs' ->
    rnCoreBndr bndr 		$ \ bndr' ->
    rnCoreExpr body		`thenRn` \ body' ->
    returnRn (UfLet (UfNonRec bndr' rhs') body')

rnCoreExpr (UfLet (UfRec pairs) body)
  = rnCoreBndrs bndrs		$ \ bndrs' ->
    mapRn rnCoreExpr rhss	`thenRn` \ rhss' ->
    rnCoreExpr body		`thenRn` \ body' ->
    returnRn (UfLet (UfRec (bndrs' `zip` rhss')) body')
  where
    (bndrs, rhss) = unzip pairs
\end{code}

\begin{code}
rnCoreBndr (UfValBinder name ty) thing_inside
  = rnHsType doc ty		`thenRn` \ ty' ->
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
  = rnUfCon con 			`thenRn` \ con' ->
    bindCoreLocalsRn bndrs		$ \ bndrs' ->
    rnCoreExpr rhs			`thenRn` \ rhs' ->
    returnRn (con', bndrs', rhs')

rnNote (UfCoerce ty)
  = rnHsType (text "unfolding coerce") ty	`thenRn` \ ty' ->
    returnRn (UfCoerce ty')

rnNote (UfSCC cc)   = returnRn (UfSCC cc)
rnNote UfInlineCall = returnRn UfInlineCall
rnNote UfInlineMe   = returnRn UfInlineMe


rnUfCon UfDefault
  = returnRn UfDefault

rnUfCon (UfTupleAlt (HsTupCon _ boxity arity))
  = returnRn (UfTupleAlt (HsTupCon tup_name boxity arity))
  where
    tup_name = getName (tupleCon boxity arity)

rnUfCon (UfDataAlt con)
  = lookupOccRn con		`thenRn` \ con' ->
    returnRn (UfDataAlt con')

rnUfCon (UfLitAlt lit)
  = returnRn (UfLitAlt lit)

rnUfCon (UfLitLitAlt lit ty)
  = rnHsType (text "litlit") ty		`thenRn` \ ty' ->
    returnRn (UfLitLitAlt lit ty')
\end{code}

%*********************************************************
%*							 *
\subsection{Rule shapes}
%*							 *
%*********************************************************

Check the shape of a transformation rule LHS.  Currently
we only allow LHSs of the form @(f e1 .. en)@, where @f@ is
not one of the @forall@'d variables.

\begin{code}
validRuleLhs foralls lhs
  = check lhs
  where
    check (OpApp _ op _ _)		  = check op
    check (HsApp e1 e2) 		  = check e1
    check (HsVar v) | v `notElem` foralls = True
    check other				  = False
\end{code}


%*********************************************************
%*							 *
\subsection{Errors}
%*							 *
%*********************************************************

\begin{code}
badDataCon name
   = hsep [ptext SLIT("Illegal data constructor name"), quotes (ppr name)]

badRuleLhsErr name lhs
  = sep [ptext SLIT("Rule") <+> ptext name <> colon,
	 nest 4 (ptext SLIT("Illegal left-hand side:") <+> ppr lhs)]
    $$
    ptext SLIT("LHS must be of form (f e1 .. en) where f is not forall'd")

badRuleVar name var
  = sep [ptext SLIT("Rule") <+> doubleQuotes (ptext name) <> colon,
	 ptext SLIT("Forall'd variable") <+> quotes (ppr var) <+> 
		ptext SLIT("does not appear on left hand side")]

emptyConDeclsErr tycon
  = sep [quotes (ppr tycon) <+> ptext SLIT("has no constructors"),
	 nest 4 (ptext SLIT("(-fglasgow-exts permits this)"))]
\end{code}

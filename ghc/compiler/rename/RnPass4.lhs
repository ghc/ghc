%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnPass4]{Fourth of the renaming passes}

\begin{code}
#include "HsVersions.h"

module RnPass4 ( rnModule, rnPolyType, rnGenPragmas ) where

import Ubiq{-uitous-}
import RnLoop -- *check* the RnPass4/RnExpr4/RnBinds4 loop-breaking

import HsSyn
import RdrHsSyn
import RnHsSyn
import HsPragmas	-- all of it
import HsCore		-- all of it
import RnMonad4

import Class		( derivableClassKeys )
import Maybes		( maybeToBool, catMaybes )
import Name		( Name(..) )
import Outputable	( Outputable(..), isAvarid )
import Pretty		( ppHang, ppStr, ppCat, ppAboves )
import ProtoName	( eqProtoName, elemProtoNames, ProtoName{-instance-} )
import RnBinds4		( rnTopBinds, rnMethodBinds )
import SrcLoc		( SrcLoc{-instance-} )
import Unique		( Unique{-instances-} )
import UniqSet		( UniqSet(..) )
import Util		( isIn, panic, assertPanic )
\end{code}

This pass `renames' the module+imported info, simultaneously
performing dependency analysis. It also does the following error
checks:
\begin{enumerate}
\item
Checks that tyvars are used properly. This includes checking
for undefined tyvars, and tyvars in contexts that are ambiguous.
\item
Checks that local variables are defined.
\end{enumerate}

\begin{code}
rnModule :: ProtoNameHsModule -> Rn4M RenamedHsModule

rnModule (HsModule mod_name exports _ fixes ty_decls specdata_sigs
	    class_decls inst_decls specinst_sigs defaults
	    binds int_sigs src_loc)

  = pushSrcLocRn4 src_loc			  (

    mapRn4 rnTyDecl	    ty_decls	    `thenRn4` \ new_ty_decls ->
    mapRn4 rnSpecDataSig    specdata_sigs   `thenRn4` \ new_specdata_sigs ->
    mapRn4 rnClassDecl	    class_decls	    `thenRn4` \ new_class_decls ->
    mapRn4 rnInstDecl	    inst_decls	    `thenRn4` \ new_inst_decls ->
    mapRn4 rnSpecInstSig    specinst_sigs   `thenRn4` \ new_specinst_sigs ->
    rnDefaultDecl	    defaults	    `thenRn4` \ new_defaults ->
    rnTopBinds binds			    `thenRn4` \ new_binds ->
    mapRn4 rnIntSig	    int_sigs	    `thenRn4` \ new_int_sigs ->
    rnFixes fixes			    `thenRn4` \ new_fixes ->
    rnExports exports			    `thenRn4` \ new_exports ->

    returnRn4 (HsModule mod_name
		new_exports [{-imports finally clobbered-}] new_fixes
		new_ty_decls new_specdata_sigs new_class_decls
		new_inst_decls new_specinst_sigs new_defaults
		new_binds new_int_sigs src_loc)
    )

rnExports Nothing = returnRn4 Nothing
rnExports (Just exp_list)
  = returnRn4 (Just (_trace "rnExports:trashing exports" []))
\end{code}

%*********************************************************
%*							*
\subsection{Type declarations}
%*							*
%*********************************************************

@rnTyDecl@ uses the `global name function' to create a new type
declaration in which local names have been replaced by their original
names, reporting any unknown names.

Renaming type variables is a pain. Because they now contain uniques,
it is necessary to pass in an association list which maps a parsed
tyvar to its Name representation. In some cases (type signatures of
values), it is even necessary to go over the type first in order to
get the set of tyvars used by it, make an assoc list, and then go over
it again to rename the tyvars! However, we can also do some scoping
checks at the same time.

\begin{code}
rnTyDecl :: ProtoNameTyDecl -> Rn4M RenamedTyDecl

rnTyDecl (TyData context tycon tyvars condecls derivings pragmas src_loc)
  = pushSrcLocRn4 src_loc		        (
    lookupTyCon tycon		      `thenRn4` \ tycon' ->
    mkTyVarNamesEnv src_loc tyvars    `thenRn4` \ (tv_env, tyvars') ->
    rnContext tv_env context	      `thenRn4` \ context' ->
    rnConDecls tv_env False condecls `thenRn4` \ condecls' ->
    rn_derivs tycon' src_loc derivings `thenRn4` \ derivings' ->
    recoverQuietlyRn4 (DataPragmas [] []) (
	rnDataPragmas tv_env pragmas
    )				      `thenRn4` \ pragmas' ->
    returnRn4 (TyData context' tycon' tyvars' condecls' derivings' pragmas' src_loc)
    )

rnTyDecl (TyNew context tycon tyvars condecl derivings pragmas src_loc)
  = pushSrcLocRn4 src_loc		        (
    lookupTyCon tycon		      `thenRn4` \ tycon' ->
    mkTyVarNamesEnv src_loc tyvars    `thenRn4` \ (tv_env, tyvars') ->
    rnContext tv_env context	      `thenRn4` \ context' ->
    rnConDecls tv_env False condecl   `thenRn4` \ condecl' ->
    rn_derivs tycon' src_loc derivings `thenRn4` \ derivings' ->
    recoverQuietlyRn4 (DataPragmas [] []) (
	rnDataPragmas tv_env pragmas
    )				      `thenRn4` \ pragmas' ->
    returnRn4 (TyNew context' tycon' tyvars' condecl' derivings' pragmas' src_loc)
    )

rnTyDecl (TySynonym name tyvars ty src_loc)
  = pushSrcLocRn4 src_loc		      (
    lookupTyCon name		    `thenRn4` \ name' ->
    mkTyVarNamesEnv src_loc tyvars  `thenRn4` \ (tv_env, tyvars') ->
    rnMonoType False{-no invisible types-} tv_env ty
				    `thenRn4` \ ty' ->
    returnRn4 (TySynonym name' tyvars' ty' src_loc)
    )

rn_derivs tycon2 locn Nothing -- derivs not specified
  = returnRn4 Nothing

rn_derivs tycon2 locn (Just ds)
  = mapRn4 (rn_deriv tycon2 locn) ds `thenRn4` \ derivs ->
    returnRn4 (Just derivs)
  where
    rn_deriv tycon2 locn clas
      = lookupClass clas	    `thenRn4` \ clas_name ->
	case clas_name of
	  ClassName key _ _ | key `is_elem` derivableClassKeys
	    -> returnRn4 clas_name
	  _ -> addErrRn4 (derivingNonStdClassErr clas locn) `thenRn4_`
	       returnRn4 clas_name
      where
	is_elem = isIn "rn_deriv"
\end{code}

@rnConDecls@ uses the `global name function' to create a new
constructor in which local names have been replaced by their original
names, reporting any unknown names.

\begin{code}
rnConDecls :: TyVarNamesEnv
	    -> Bool		    -- True <=> allowed to see invisible data-cons
	    -> [ProtoNameConDecl]
	    -> Rn4M [RenamedConDecl]

rnConDecls tv_env invisibles_allowed con_decls
  = mapRn4 rn_decl con_decls
  where
    lookup_fn
      = if invisibles_allowed
	then lookupValueEvenIfInvisible
	else lookupValue

    rn_decl (ConDecl name tys src_loc)
      = pushSrcLocRn4 src_loc			  (
	lookup_fn name		`thenRn4` \ new_name ->
	mapRn4 rn_bang_ty tys	`thenRn4` \ new_tys  ->
	returnRn4 (ConDecl new_name new_tys src_loc)
	)

    rn_decl (ConOpDecl ty1 op ty2 src_loc)
      = pushSrcLocRn4 src_loc			  (
	lookup_fn op	`thenRn4` \ new_op  ->
	rn_bang_ty ty1  `thenRn4` \ new_ty1 ->
	rn_bang_ty ty2  `thenRn4` \ new_ty2 ->
	returnRn4 (ConOpDecl new_ty1 new_op new_ty2 src_loc)
	)

    rn_decl (NewConDecl name ty src_loc)
      = pushSrcLocRn4 src_loc			  (
	lookup_fn name		`thenRn4` \ new_name ->
	rn_mono_ty ty		`thenRn4` \ new_ty  ->
	returnRn4 (NewConDecl new_name new_ty src_loc)
	)

    rn_decl (RecConDecl con fields src_loc)
      = panic "rnConDecls:RecConDecl"

    ----------
    rn_mono_ty = rnMonoType invisibles_allowed tv_env

    rn_bang_ty (Banged ty)
      = rn_mono_ty ty `thenRn4` \ new_ty ->
	returnRn4 (Banged new_ty)
    rn_bang_ty (Unbanged ty)
      = rn_mono_ty ty `thenRn4` \ new_ty ->
	returnRn4 (Unbanged new_ty)
\end{code}

%*********************************************************
%*							*
\subsection{SPECIALIZE data pragmas}
%*							*
%*********************************************************

\begin{code}
rnSpecDataSig :: ProtoNameSpecDataSig
	      -> Rn4M RenamedSpecDataSig

rnSpecDataSig (SpecDataSig tycon ty src_loc)
  = pushSrcLocRn4 src_loc		(
    let
	tyvars = extractMonoTyNames eqProtoName ty
    in
    mkTyVarNamesEnv src_loc tyvars     	`thenRn4` \ (tv_env,_) ->
    lookupTyCon tycon			`thenRn4` \ tycon' ->
    rnMonoType False tv_env ty		`thenRn4` \ ty' ->
    returnRn4 (SpecDataSig tycon' ty' src_loc)
    )
\end{code}

%*********************************************************
%*							*
\subsection{Class declarations}
%*							*
%*********************************************************

@rnClassDecl@ uses the `global name function' to create a new
class declaration in which local names have been replaced by their
original names, reporting any unknown names.

\begin{code}
rnClassDecl :: ProtoNameClassDecl -> Rn4M RenamedClassDecl

rnClassDecl (ClassDecl context cname tyvar sigs mbinds pragmas src_loc)
  = pushSrcLocRn4 src_loc			  (
    mkTyVarNamesEnv src_loc [tyvar]	`thenRn4` \ (tv_env, [tyvar']) ->
    rnContext tv_env context	    	`thenRn4` \ context' ->
    lookupClass cname		    	`thenRn4` \ cname' ->
    mapRn4 (rn_op cname' tv_env) sigs   `thenRn4` \ sigs' ->
    rnMethodBinds cname' mbinds    	`thenRn4` \ mbinds' ->
    recoverQuietlyRn4 NoClassPragmas (
      rnClassPragmas pragmas
    )					`thenRn4` \ pragmas' ->
    returnRn4 (ClassDecl context' cname' tyvar' sigs' mbinds' pragmas' src_loc)
    )
  where
    rn_op clas tv_env (ClassOpSig op ty pragma locn)
      = pushSrcLocRn4 locn	    	      (
	lookupClassOp clas op		`thenRn4` \ op_name ->
	rnPolyType False tv_env ty	`thenRn4` \ new_ty  ->

{-
*** Please check here that tyvar' appears in new_ty ***
*** (used to be in tcClassSig, but it's better here)
***	    not_elem = isn'tIn "tcClassSigs"
***	    -- Check that the class type variable is mentioned
***	checkTc (clas_tyvar `not_elem` extractTyVarTemplatesFromTy local_ty)
***		(methodTypeLacksTyVarErr clas_tyvar (_UNPK_ op_name) src_loc) `thenTc_`
-}
	recoverQuietlyRn4 NoClassOpPragmas (
	    rnClassOpPragmas pragma
	)				`thenRn4` \ new_pragma ->
	returnRn4 (ClassOpSig op_name new_ty new_pragma locn)
	)
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************


@rnInstDecl@ uses the `global name function' to create a new of
instance declaration in which local names have been replaced by their
original names, reporting any unknown names.

\begin{code}
rnInstDecl :: ProtoNameInstDecl -> Rn4M RenamedInstDecl

rnInstDecl (InstDecl cname ty mbinds from_here modname uprags pragmas src_loc)
  = pushSrcLocRn4 src_loc		 	  (
    let
	tyvars = extract_poly_ty_names ty
    in
    mkTyVarNamesEnv src_loc tyvars     	`thenRn4` \ (tv_env,_) ->
    lookupClass cname 		     	`thenRn4` \ cname' ->
    rnPolyType False{-no invisibles-} tv_env ty
					`thenRn4` \ ty' ->
    rnMethodBinds cname' mbinds	`thenRn4` \ mbinds' ->
    mapRn4 (rn_uprag cname') uprags	`thenRn4` \ new_uprags ->
    recoverQuietlyRn4 NoInstancePragmas (
	rnInstancePragmas cname' tv_env pragmas
    )					`thenRn4` \ new_pragmas ->
    returnRn4 (InstDecl cname' ty' mbinds'
			from_here modname new_uprags new_pragmas src_loc)
    )
  where
    rn_uprag class_name (SpecSig op ty using locn)
      = ASSERT(not (maybeToBool using))	-- ToDo: SPEC method with explicit spec_id
	pushSrcLocRn4 src_loc 				(
	lookupClassOp class_name op		`thenRn4` \ op_name ->
	rnPolyType False nullTyVarNamesEnv ty	`thenRn4` \ new_ty ->
	returnRn4 (SpecSig op_name new_ty Nothing locn)
	)
    rn_uprag class_name (InlineSig op locn)
      = pushSrcLocRn4 locn	      	(
	lookupClassOp class_name op	`thenRn4` \ op_name ->
	returnRn4 (InlineSig op_name locn)
	)
    rn_uprag class_name (DeforestSig op locn)
      = pushSrcLocRn4 locn            	(
	lookupClassOp class_name op	`thenRn4` \ op_name ->
	returnRn4 (DeforestSig op_name locn)
	)
    rn_uprag class_name (MagicUnfoldingSig op str locn)
      = pushSrcLocRn4 locn		(
	lookupClassOp class_name op	`thenRn4` \ op_name ->
	returnRn4 (MagicUnfoldingSig op_name str locn)
	)
\end{code}

%*********************************************************
%*							*
\subsection{@SPECIALIZE instance@ user-pragmas}
%*							*
%*********************************************************

\begin{code}
rnSpecInstSig :: ProtoNameSpecInstSig
	      -> Rn4M RenamedSpecInstSig

rnSpecInstSig (SpecInstSig clas ty src_loc)
  = pushSrcLocRn4 src_loc		  (
    let  tyvars = extractMonoTyNames eqProtoName ty  in
    mkTyVarNamesEnv src_loc tyvars     	`thenRn4` \ (tv_env,_) ->
    lookupClass clas			`thenRn4` \ new_clas ->
    rnMonoType False tv_env ty		`thenRn4` \ new_ty ->
    returnRn4 (SpecInstSig new_clas new_ty src_loc)
    )
\end{code}

%*********************************************************
%*							*
\subsection{Default declarations}
%*							*
%*********************************************************

@rnDefaultDecl@ uses the `global name function' to create a new set
of default declarations in which local names have been replaced by
their original names, reporting any unknown names.

\begin{code}
rnDefaultDecl :: [ProtoNameDefaultDecl] -> Rn4M [RenamedDefaultDecl]

rnDefaultDecl [] = returnRn4 []
rnDefaultDecl [DefaultDecl tys src_loc]
  = pushSrcLocRn4 src_loc $
    mapRn4 (rnMonoType False nullTyVarNamesEnv) tys `thenRn4` \ tys' ->
    returnRn4 [DefaultDecl tys' src_loc]
rnDefaultDecl defs@(d:ds)
  = addErrRn4 (dupDefaultDeclErr defs) `thenRn4_`
    rnDefaultDecl [d]
\end{code}

%*************************************************************************
%*									*
\subsection{Type signatures from interfaces}
%*									*
%*************************************************************************

Non-interface type signatures (which may include user-pragmas) are
handled with @HsBinds@.

@ClassOpSigs@ are dealt with in class declarations.

\begin{code}
rnIntSig :: ProtoNameSig -> Rn4M RenamedSig

rnIntSig (Sig name ty pragma src_loc)
  = pushSrcLocRn4 src_loc			      (
    lookupValue name				`thenRn4` \ new_name ->
    rnPolyType False nullTyVarNamesEnv ty 	`thenRn4` \ new_ty   ->
    recoverQuietlyRn4 NoGenPragmas (
	rnGenPragmas pragma
    )					    `thenRn4` \ new_pragma ->
    returnRn4 (Sig new_name new_ty new_pragma src_loc)
    )
\end{code}

%*************************************************************************
%*									*
\subsection{Fixity declarations}
%*									*
%*************************************************************************

\begin{code}
rnFixes :: [ProtoNameFixityDecl]  -> Rn4M [RenamedFixityDecl]

rnFixes fixities
  = mapRn4 rn_fixity fixities `thenRn4` \ fixes_maybe ->
    returnRn4 (catMaybes fixes_maybe)
  where
    rn_fixity (InfixL name i)
      = lookupFixityOp name `thenRn4` \ res ->
	returnRn4 (
	  case res of
	    Just name2 ->  Just (InfixL name2 i)
	    Nothing    ->  Nothing
	)

    rn_fixity (InfixR name i)
      = lookupFixityOp name	`thenRn4` \ res ->
	returnRn4 (
	  case res of
	    Just name2 ->  Just (InfixR name2 i)
	    Nothing    ->  Nothing
	)

    rn_fixity (InfixN name i)
      = lookupFixityOp name	`thenRn4` \ res ->
	returnRn4 (
	  case res of
	    Just name2 ->  Just (InfixN name2 i)
	    Nothing    ->  Nothing
	)
\end{code}

%*********************************************************
%*							*
\subsection{Support code to rename types}
%*							*
%*********************************************************

\begin{code}
rnPolyType :: Bool		-- True <=> "invisible" tycons (in pragmas) allowed
	    -> TyVarNamesEnv
	    -> ProtoNamePolyType
	    -> Rn4M RenamedPolyType

rnPolyType invisibles_allowed tv_env (HsForAllTy tvs ctxt ty)
  = rn_poly_help invisibles_allowed tv_env tvs ctxt ty

rnPolyType invisibles_allowed tv_env poly_ty@(HsPreForAllTy ctxt ty)
  = rn_poly_help invisibles_allowed tv_env forall_tyvars ctxt ty
  where
    mentioned_tyvars = extract_poly_ty_names poly_ty

    forall_tyvars = mentioned_tyvars `minus_list` domTyVarNamesEnv tv_env

	-- URGH! Why is this here?  SLPJ
	-- Because we are doing very delicate comparisons
	-- (eqProtoName and all that); if we got rid of
	-- that, then we could use ListSetOps stuff.  WDP
    minus_list xs ys = [ x | x <- xs, not (x `elemProtoNames` ys)]

------------
extract_poly_ty_names (HsPreForAllTy ctxt ty)
  = extractCtxtTyNames eqProtoName ctxt
    `union_list`
    extractMonoTyNames eqProtoName ty
  where
    -- see comment above
    union_list []     [] = []
    union_list []     b	 = b
    union_list a      [] = a
    union_list (a:as) b
      | a `elemProtoNames` b = union_list as b
      | otherwise            = a : union_list as b

------------
rn_poly_help :: Bool
	     -> TyVarNamesEnv
	     -> [ProtoName]
	     -> ProtoNameContext
	     -> ProtoNameMonoType
	     -> Rn4M RenamedPolyType

rn_poly_help invisibles_allowed tv_env tyvars ctxt ty
  = getSrcLocRn4 				`thenRn4` \ src_loc ->
    mkTyVarNamesEnv src_loc tyvars	 	`thenRn4` \ (tv_env1, new_tyvars) ->
    let
	tv_env2 = catTyVarNamesEnvs tv_env1 tv_env
    in
    rnContext tv_env2 ctxt			`thenRn4` \ new_ctxt ->
    rnMonoType invisibles_allowed tv_env2 ty	`thenRn4` \ new_ty ->
    returnRn4 (HsForAllTy new_tyvars new_ctxt new_ty)
\end{code}

\begin{code}
rnMonoType :: Bool		-- allowed to look at invisible tycons
	    -> TyVarNamesEnv
	    -> ProtoNameMonoType
	    -> Rn4M RenamedMonoType

rnMonoType invisibles_allowed  tv_env (MonoTyVar tyvar)
  = lookupTyVarName tv_env tyvar 	`thenRn4` \ tyvar' ->
    returnRn4 (MonoTyVar tyvar')

rnMonoType invisibles_allowed  tv_env (MonoListTy ty)
  = rnMonoType invisibles_allowed tv_env ty	`thenRn4` \ ty' ->
    returnRn4 (MonoListTy ty')

rnMonoType invisibles_allowed  tv_env (MonoFunTy ty1 ty2)
  = andRn4 MonoFunTy (rnMonoType invisibles_allowed tv_env ty1)
		     (rnMonoType invisibles_allowed tv_env ty2)

rnMonoType invisibles_allowed  tv_env (MonoTupleTy tys)
  = mapRn4 (rnMonoType invisibles_allowed tv_env) tys `thenRn4` \ tys' ->
    returnRn4 (MonoTupleTy tys')

rnMonoType invisibles_allowed tv_env (MonoTyApp name tys)
  = let
	lookup_fn = if isAvarid (getOccurrenceName name) 
		    then lookupTyVarName tv_env
		    else if invisibles_allowed
		         then lookupTyConEvenIfInvisible
		         else lookupTyCon
    in
    lookup_fn name					`thenRn4` \ name' ->
    mapRn4 (rnMonoType invisibles_allowed tv_env) tys	`thenRn4` \ tys' ->
    returnRn4 (MonoTyApp name' tys')

-- for unfoldings only:

rnMonoType invisibles_allowed tv_env (MonoForAllTy tyvars_w_kinds ty)
  = getSrcLocRn4 				`thenRn4` \ src_loc ->
    mkTyVarNamesEnv src_loc tyvars	 	`thenRn4` \ (tv_env1, new_tyvars) ->
    let
	tv_env2 = catTyVarNamesEnvs tv_env1 tv_env
    in
    rnMonoType invisibles_allowed tv_env2 ty	`thenRn4` \ ty' ->
    returnRn4 (MonoForAllTy (new_tyvars `zip` kinds) ty')
  where
    (tyvars, kinds) = unzip tyvars_w_kinds

rnMonoType invisibles_allowed tv_env (MonoDictTy clas ty)
  = lookupClass clas		`thenRn4` \ new_clas ->
    rnMonoType invisibles_allowed tv_env ty	`thenRn4` \ new_ty ->
    returnRn4 (MonoDictTy new_clas new_ty)
\end{code}

\begin{code}
rnContext :: TyVarNamesEnv -> ProtoNameContext -> Rn4M RenamedContext

rnContext tv_env ctxt
  = mapRn4 rn_ctxt ctxt
  where
    rn_ctxt (clas, tyvar)
     = lookupClass clas	    	    `thenRn4` \ clas_name ->
       lookupTyVarName tv_env tyvar `thenRn4` \ tyvar_name ->
       returnRn4 (clas_name, tyvar_name)
\end{code}

%*********************************************************
%*							*
\subsection{Support code to rename various pragmas}
%*							*
%*********************************************************

\begin{code}
rnDataPragmas tv_env (DataPragmas cons specs)
  = rnConDecls tv_env True{-invisibles allowed-} cons `thenRn4` \ new_cons ->
    mapRn4 types_n_spec specs			       `thenRn4` \ new_specs ->
    returnRn4 (DataPragmas new_cons new_specs)
  where
    types_n_spec ty_maybes
      = mapRn4 (rn_ty_maybe nullTyVarNamesEnv) ty_maybes
\end{code}

\begin{code}
rnClassOpPragmas NoClassOpPragmas = returnRn4 NoClassOpPragmas

rnClassOpPragmas (ClassOpPragmas dsel defm)
  = recoverQuietlyRn4 NoGenPragmas (rnGenPragmas dsel) `thenRn4` \ new_dsel ->
    recoverQuietlyRn4 NoGenPragmas (rnGenPragmas defm) `thenRn4` \ new_defm ->
    returnRn4 (ClassOpPragmas new_dsel new_defm)
\end{code}

\begin{code}
rnClassPragmas NoClassPragmas = returnRn4 NoClassPragmas

rnClassPragmas (SuperDictPragmas sds)
  = mapRn4 rnGenPragmas sds	`thenRn4` \ new_sds ->
    returnRn4 (SuperDictPragmas new_sds)
\end{code}

NB: In various cases around here, we don't @recoverQuietlyRn4@ around
calls to @rnGenPragmas@; not really worth it.

\begin{code}
rnInstancePragmas _ _ NoInstancePragmas = returnRn4 NoInstancePragmas

rnInstancePragmas _ _ (SimpleInstancePragma dfun)
  = rnGenPragmas dfun	`thenRn4` \ new_dfun ->
    returnRn4 (SimpleInstancePragma new_dfun)

rnInstancePragmas clas tv_env (ConstantInstancePragma dfun constms)
  = recoverQuietlyRn4 NoGenPragmas (
	rnGenPragmas dfun
    )				`thenRn4` \ new_dfun ->
    mapRn4 name_n_gen constms	`thenRn4` \ new_constms ->
    returnRn4 (ConstantInstancePragma new_dfun new_constms)
  where
    name_n_gen (op, gen)
      = lookupClassOp clas op	`thenRn4` \ new_op ->
	rnGenPragmas gen	`thenRn4` \ new_gen ->
	returnRn4 (new_op, new_gen)

rnInstancePragmas clas tv_env (SpecialisedInstancePragma dfun specs)
  = recoverQuietlyRn4 NoGenPragmas (
	rnGenPragmas dfun
    )				`thenRn4` \ new_dfun ->
    mapRn4 types_n_spec specs	`thenRn4` \ new_specs ->
    returnRn4 (SpecialisedInstancePragma new_dfun new_specs)
  where
    types_n_spec (ty_maybes, dicts_to_ignore, inst)
      = mapRn4 (rn_ty_maybe tv_env) ty_maybes	`thenRn4` \ new_tys ->
	rnInstancePragmas clas tv_env inst 	`thenRn4` \ new_inst ->
	returnRn4 (new_tys, dicts_to_ignore, new_inst)
\end{code}

And some general pragma stuff: (Not sure what, if any, of this would
benefit from a TyVarNamesEnv passed in.... [ToDo])
\begin{code}
rnGenPragmas :: ProtoNameGenPragmas -> Rn4M RenamedGenPragmas

rnGenPragmas NoGenPragmas = returnRn4 NoGenPragmas

rnGenPragmas (GenPragmas arity upd def strict unfold specs)
  = recoverQuietlyRn4 NoImpUnfolding (
	rn_unfolding  unfold
    )				`thenRn4` \ new_unfold ->
    rn_strictness strict	`thenRn4` \ new_strict ->
    recoverQuietlyRn4 [] (
	mapRn4 types_n_gen specs
    )				`thenRn4` \ new_specs ->
    returnRn4 (GenPragmas arity upd def new_strict new_unfold new_specs)
  where
    rn_unfolding NoImpUnfolding = returnRn4 NoImpUnfolding

    rn_unfolding (ImpMagicUnfolding str) = returnRn4 (ImpMagicUnfolding str)

    rn_unfolding (ImpUnfolding guidance core)
      = rn_core nullTyVarNamesEnv core	`thenRn4` \ new_core ->
    	returnRn4 (ImpUnfolding guidance new_core)

    ------------
    rn_strictness NoImpStrictness = returnRn4 NoImpStrictness

    rn_strictness (ImpStrictness is_bot ww_info wrkr_info)
      = recoverQuietlyRn4 NoGenPragmas (
	    rnGenPragmas wrkr_info
	) 			`thenRn4` \ new_wrkr_info ->
	returnRn4 (ImpStrictness is_bot ww_info new_wrkr_info)

    -------------
    types_n_gen (ty_maybes, dicts_to_ignore, gen)
      = mapRn4 (rn_ty_maybe no_env) ty_maybes	`thenRn4` \ new_tys ->
	recoverQuietlyRn4 NoGenPragmas (
	    rnGenPragmas gen
	)				`thenRn4` \ new_gen ->
	returnRn4 (new_tys, dicts_to_ignore, new_gen)
      where
	no_env = nullTyVarNamesEnv

------------
rn_ty_maybe tv_env Nothing = returnRn4 Nothing

rn_ty_maybe tv_env (Just ty)
  = rnMonoType True{-invisibles OK-} tv_env ty  `thenRn4` \ new_ty ->
    returnRn4 (Just new_ty)

------------
rn_core tvenv (UfVar v)
  = rn_uf_id tvenv v	`thenRn4` \ vname ->
    returnRn4 (UfVar vname)

rn_core tvenv (UfLit lit)
  = returnRn4 (UfLit lit)

rn_core tvenv (UfCon con tys as)
  = lookupValueEvenIfInvisible con	`thenRn4` \ new_con ->
    mapRn4 (rn_core_type tvenv) tys	`thenRn4` \ new_tys ->
    mapRn4 (rn_atom tvenv) as   	`thenRn4` \ new_as ->
    returnRn4 (UfCon new_con new_tys new_as)

rn_core tvenv (UfPrim op tys as)
  = rn_core_primop tvenv op		`thenRn4` \ new_op ->
    mapRn4 (rn_core_type tvenv) tys	`thenRn4` \ new_tys ->
    mapRn4 (rn_atom tvenv) as   	`thenRn4` \ new_as ->
    returnRn4 (UfPrim new_op new_tys new_as)

rn_core tvenv (UfLam binder body)
  = rn_binder tvenv binder `thenRn4` \ (b,ty) ->
    extendSS [b] (rn_core tvenv body) `thenRn4` \ new_body ->
    returnRn4 (UfLam (b,ty) new_body)

rn_core tvenv (UfApp fun arg)
  = rn_core tvenv fun	`thenRn4` \ new_fun ->
    rn_atom tvenv arg	`thenRn4` \ new_arg ->
    returnRn4 (UfApp new_fun new_arg)

rn_core tvenv (UfCase expr alts)
  = rn_core tvenv expr	    `thenRn4` \ new_expr ->
    rn_alts 	  alts	    `thenRn4` \ new_alts ->
    returnRn4 (UfCase new_expr new_alts)
  where
    rn_alts (UfCoAlgAlts alg_alts deflt)
      = mapRn4 rn_alg_alt alg_alts  `thenRn4` \ new_alts ->
	rn_deflt deflt	    	    `thenRn4` \ new_deflt ->
	returnRn4 (UfCoAlgAlts new_alts new_deflt)
      where
	rn_alg_alt (con, params, rhs)
	  = lookupValueEvenIfInvisible con  `thenRn4` \ new_con ->
	    mapRn4 (rn_binder tvenv) params `thenRn4` \ new_params ->
	    let
		bs = [ b | (b, ty) <- new_params ]
	    in
	    extendSS bs (rn_core tvenv rhs) `thenRn4` \ new_rhs ->
	    returnRn4 (new_con, new_params, new_rhs)

    rn_alts (UfCoPrimAlts prim_alts deflt)
      = mapRn4 rn_prim_alt prim_alts  `thenRn4` \ new_alts ->
	rn_deflt deflt		      `thenRn4` \ new_deflt ->
	returnRn4 (UfCoPrimAlts new_alts new_deflt)
      where
	rn_prim_alt (lit, rhs)
	  = rn_core tvenv rhs	`thenRn4` \ new_rhs ->
	    returnRn4 (lit, new_rhs)

    rn_deflt UfCoNoDefault = returnRn4 UfCoNoDefault
    rn_deflt (UfCoBindDefault b rhs)
      = rn_binder tvenv b		      `thenRn4` \ new_b@(binder, ty) ->
	extendSS [binder] (rn_core tvenv rhs) `thenRn4` \ new_rhs ->
	returnRn4 (UfCoBindDefault new_b new_rhs)

rn_core tvenv (UfLet bind body)
  = rn_bind bind			      `thenRn4` \ (new_bind, new_binders) ->
    extendSS new_binders (rn_core tvenv body) `thenRn4` \ new_body ->
    returnRn4 (UfLet new_bind new_body)
  where
    rn_bind (UfCoNonRec b rhs)
      = rn_binder tvenv b	`thenRn4` \ new_b@(binder, ty) ->
	rn_core   tvenv rhs 	`thenRn4` \ new_rhs ->
	returnRn4 (UfCoNonRec new_b new_rhs, [binder])

    rn_bind (UfCoRec pairs)
      = -- conjure up Names; we do this differently than
	-- elsewhere for Core, because of the recursion here;
	-- no deep issue.
	-- [BEFORE IT WAS "FIXED"... 94/05...]
 	-- [Andy -- It *was* a 'deep' issue to me...]
	-- [Will -- Poor wee soul.]

	getSrcLocRn4			    `thenRn4` \ locn ->
	namesFromProtoNames "core variable"
	  [ (b, locn) | ((b,_),_) <- pairs] `thenRn4` \ binders ->

	extendSS binders (mapRn4 rn_pair (pairs `zip` binders)) `thenRn4` \ new_pairs ->
	returnRn4 (UfCoRec new_pairs, binders)
      where
	rn_pair (((b, ty), rhs), new_b)
	  = rn_core_type tvenv ty	`thenRn4` \ new_ty ->
	    rn_core      tvenv rhs	`thenRn4` \ new_rhs ->
	    returnRn4 ((new_b, new_ty), new_rhs)

rn_core tvenv (UfSCC uf_cc body)
  = rn_cc uf_cc		`thenRn4` \ new_cc ->
    rn_core tvenv body	`thenRn4` \ new_body ->
    returnRn4 (UfSCC new_cc new_body)
  where
    rn_cc (UfAutoCC id m g is_dupd is_caf)
      = rn_uf_id tvenv id	`thenRn4` \ new_id ->
	returnRn4 (UfAutoCC new_id m g is_dupd is_caf)

    rn_cc (UfDictCC id m g is_caf is_dupd)
      = rn_uf_id tvenv id	`thenRn4` \ new_id ->
	returnRn4 (UfDictCC new_id m g is_dupd is_caf)

    -- the rest are boring:
    rn_cc (UfPreludeDictsCC d) = returnRn4 (UfPreludeDictsCC d)
    rn_cc (UfAllDictsCC m g d) = returnRn4 (UfAllDictsCC m g d)
    rn_cc (UfUserCC n m g d c) = returnRn4 (UfUserCC n m g d c)

------------
rn_core_primop tvenv (UfCCallOp str is_casm may_gc arg_tys res_ty)
  = mapRn4 (rn_core_type tvenv) arg_tys	`thenRn4` \ new_arg_tys ->
    rn_core_type tvenv res_ty		`thenRn4` \ new_res_ty  ->
    returnRn4 (UfCCallOp str is_casm may_gc new_arg_tys new_res_ty)
rn_core_primop tvenv (UfOtherOp op)
  = returnRn4 (UfOtherOp op)

------------
rn_uf_id tvenv (BoringUfId v)
  = lookupValueEvenIfInvisible v    `thenRn4` \ vname ->
    returnRn4 (BoringUfId vname)

rn_uf_id tvenv (SuperDictSelUfId c sc)
  = lookupClass{-EvenIfInvisible-} c	`thenRn4` \ new_c ->
    lookupClass{-EvenIfInvisible-} sc	`thenRn4` \ new_sc ->
    returnRn4 (SuperDictSelUfId new_c new_sc)

rn_uf_id tvenv (ClassOpUfId c op)
  = lookupClass{-EvenIfInvisible-} c	    	`thenRn4` \ new_c ->
    lookupClassOp{-EvenIfInvisible-} new_c op	`thenRn4` \ new_op ->
    returnRn4 (ClassOpUfId new_c new_op)

rn_uf_id tvenv (DictFunUfId c ty)
  = lookupClass{-EvenIfInvisible-} c	`thenRn4` \ new_c ->
    rn_core_type tvenv ty   	    	`thenRn4` \ new_ty ->
    returnRn4 (DictFunUfId new_c new_ty)

rn_uf_id tvenv (ConstMethodUfId c op ty)
  = lookupClass{-EvenIfInvisible-} c	      `thenRn4` \ new_c ->
    lookupClassOp{-EvenIfInvisible-} new_c op `thenRn4` \ new_op ->
    rn_core_type tvenv ty   	    	      `thenRn4` \ new_ty ->
    returnRn4 (ConstMethodUfId new_c new_op new_ty)

rn_uf_id tvenv (DefaultMethodUfId c op)
  = lookupClass{-EvenIfInvisible-} c	    	`thenRn4` \ new_c ->
    lookupClassOp{-EvenIfInvisible-} new_c op	`thenRn4` \ new_op ->
    returnRn4 (DefaultMethodUfId new_c new_op)

rn_uf_id tvenv (SpecUfId unspec ty_maybes)
  = rn_uf_id tvenv unspec		 `thenRn4` \ new_unspec ->
    mapRn4 (rn_ty_maybe tvenv) ty_maybes `thenRn4` \ new_ty_maybes ->
    returnRn4 (SpecUfId new_unspec new_ty_maybes)

rn_uf_id tvenv (WorkerUfId unwrkr)
  = rn_uf_id tvenv unwrkr	`thenRn4` \ new_unwrkr ->
    returnRn4 (WorkerUfId new_unwrkr)

------------
rn_binder tvenv (b, ty)
  = getSrcLocRn4			`thenRn4` \ src_loc ->
    namesFromProtoNames "binder in core unfolding" [(b, src_loc)]
					`thenRn4` \ [new_b] ->
    rn_core_type tvenv ty		`thenRn4` \ new_ty ->
    returnRn4 (new_b, new_ty)

------------
rn_atom tvenv (UfCoLitAtom l) = returnRn4 (UfCoLitAtom l)
rn_atom tvenv (UfCoVarAtom v)
  = rn_uf_id tvenv v			`thenRn4` \ vname ->
    returnRn4 (UfCoVarAtom vname)

------------
rn_core_type_maybe tvenv Nothing = returnRn4 Nothing
rn_core_type_maybe tvenv (Just ty)
  = rn_core_type tvenv ty `thenRn4` \ new_ty ->
    returnRn4 (Just new_ty)

------------
rn_core_type tvenv ty
  = rnPolyType True{-invisible tycons OK-} tvenv ty
\end{code}


\begin{code}
derivingNonStdClassErr clas locn sty
  = ppHang (ppStr "Non-standard class in deriving")
         4 (ppCat [ppr sty clas, ppr sty locn])

dupDefaultDeclErr defs sty
  = ppHang (ppStr "Duplicate default declarations")
         4 (ppAboves (map pp_def_loc defs))
  where
    pp_def_loc (DefaultDecl _ src_loc) = ppr sty src_loc
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Rename4]{Fourth of the renaming passes}

\begin{code}
#include "HsVersions.h"

module Rename4 (
	rnModule4, rnPolyType4, rnGenPragmas4,

	initRn4, Rn4M(..), TyVarNamesEnv(..),  -- re-exported from the monad

	-- for completeness

	Module, Bag, InPat, ProtoNamePat(..), RenamedPat(..),
	PolyType, Maybe, Name, ProtoName, GlobalNameFun(..),
	SrcLoc, SplitUniqSupply, Error(..), PprStyle,
	Pretty(..), PrettyRep
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Outputable
import Pretty

import AbsSyn
import AbsUniType	( derivableClassKeys )
import Errors
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import Maybes		( catMaybes, maybeToBool, Maybe(..) )
import ProtoName	( eqProtoName, elemProtoNames )
import RenameBinds4	( rnTopBinds4, rnMethodBinds4 )
import RenameMonad4
import Util
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
rnModule4 :: ProtoNameModule -> Rn4M RenamedModule

rnModule4 (Module mod_name exports _ fixes ty_decls absty_sigs
	    class_decls inst_decls specinst_sigs defaults
	    binds int_sigs src_loc)

  = pushSrcLocRn4 src_loc			  (

    mapRn4 rnTyDecl4 ty_decls	    	`thenRn4` \ new_ty_decls ->

    mapRn4 rnTySig4 absty_sigs		`thenRn4` \ new_absty_sigs ->

    mapRn4 rnClassDecl4 class_decls	`thenRn4` \ new_class_decls ->

    mapRn4 rnInstDecl4 inst_decls	`thenRn4` \ new_inst_decls ->

    mapRn4 rnInstSpecSig4 specinst_sigs `thenRn4` \ new_specinst_sigs ->

    mapRn4 rnDefaultDecl4 defaults	`thenRn4` \ new_defaults ->

    rnTopBinds4 binds		    	`thenRn4` \ new_binds ->

    mapRn4 rnIntSig4 int_sigs	    	`thenRn4` \ new_int_sigs ->

    rnFixes4 fixes			`thenRn4` \ new_fixes ->

    returnRn4 (Module mod_name
		exports [{-imports finally clobbered-}] new_fixes
		new_ty_decls new_absty_sigs new_class_decls
		new_inst_decls new_specinst_sigs new_defaults
		new_binds new_int_sigs src_loc)
    )
\end{code}


%*********************************************************
%*							*
\subsection{Type declarations}
%*							*
%*********************************************************

@rnTyDecl4@ uses the `global name function' to create a new type
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
rnTyDecl4 :: ProtoNameTyDecl -> Rn4M RenamedTyDecl

rnTyDecl4 (TyData context tycon tyvars condecls derivings pragmas src_loc)
  = pushSrcLocRn4 src_loc		        (
    lookupTyCon tycon		      `thenRn4` \ tycon' ->
    mkTyVarNamesEnv src_loc tyvars    `thenRn4` \ (tv_env, tyvars') ->
    rnContext4 tv_env context	      `thenRn4` \ context' ->
    rnConDecls4 tv_env False condecls `thenRn4` \ condecls' ->
    mapRn4 (rn_deriv tycon' src_loc) derivings `thenRn4` \ derivings' ->
    recoverQuietlyRn4 (DataPragmas [] []) (
	rnDataPragmas4 tv_env pragmas
    )				      `thenRn4` \ pragmas' ->
    returnRn4 (TyData context' tycon' tyvars' condecls' derivings' pragmas' src_loc)
    )
  where
    rn_deriv tycon2 locn deriv
      = lookupClass deriv	    `thenRn4` \ clas_name ->
	case clas_name of
	  PreludeClass key _ | key `is_elem` derivableClassKeys
	    -> returnRn4 clas_name
	  _ -> addErrRn4 (derivingNonStdClassErr tycon2 deriv locn) `thenRn4_`
	       returnRn4 clas_name
      where
	is_elem = isIn "rn_deriv"

rnTyDecl4 (TySynonym name tyvars ty pragmas src_loc)
  = pushSrcLocRn4 src_loc		      (
    lookupTyCon name		    `thenRn4` \ name' ->
    mkTyVarNamesEnv src_loc tyvars  `thenRn4` \ (tv_env, tyvars') ->
    rnMonoType4 False{-no invisible types-} tv_env ty
				    `thenRn4` \ ty' ->
    returnRn4 (TySynonym name' tyvars' ty' pragmas src_loc)
    )
\end{code}

@rnConDecls4@ uses the `global name function' to create a new
constructor in which local names have been replaced by their original
names, reporting any unknown names.

\begin{code}
rnConDecls4 :: TyVarNamesEnv
	    -> Bool		    -- True <=> allowed to see invisible data-cons
	    -> [ProtoNameConDecl]
	    -> Rn4M [RenamedConDecl]

rnConDecls4 tv_env invisibles_allowed con_decls
  = mapRn4 rn_decl con_decls
  where
    lookup_fn
      = if invisibles_allowed
	then lookupValueEvenIfInvisible
	else lookupValue

    rn_decl (ConDecl name tys src_loc)
      = pushSrcLocRn4 src_loc			  (
	lookup_fn name			`thenRn4` \ new_name ->
	mapRn4 (rnMonoType4 invisibles_allowed tv_env) tys
					`thenRn4` \ new_tys  ->

	returnRn4 (ConDecl new_name new_tys src_loc)
	)
\end{code}

%*********************************************************
%*							*
\subsection{ABSTRACT type-synonym pragmas}
%*							*
%*********************************************************

\begin{code}
rnTySig4 :: ProtoNameDataTypeSig
	    -> Rn4M RenamedDataTypeSig

rnTySig4 (AbstractTypeSig tycon src_loc)
  = pushSrcLocRn4 src_loc		  (
    lookupTyCon tycon		`thenRn4` \ tycon' ->
    returnRn4 (AbstractTypeSig tycon' src_loc)
    )

rnTySig4 (SpecDataSig tycon ty src_loc)
  = pushSrcLocRn4 src_loc		(
    let
	tyvars = extractMonoTyNames eqProtoName ty
    in
    mkTyVarNamesEnv src_loc tyvars     	`thenRn4` \ (tv_env,_) ->
    lookupTyCon tycon			`thenRn4` \ tycon' ->
    rnMonoType4 False tv_env ty		`thenRn4` \ ty' ->
    returnRn4 (SpecDataSig tycon' ty' src_loc)
    )
\end{code}

%*********************************************************
%*							*
\subsection{Class declarations}
%*							*
%*********************************************************

@rnClassDecl4@ uses the `global name function' to create a new
class declaration in which local names have been replaced by their
original names, reporting any unknown names.

\begin{code}
rnClassDecl4 :: ProtoNameClassDecl -> Rn4M RenamedClassDecl

rnClassDecl4 (ClassDecl context cname tyvar sigs mbinds pragmas src_loc)
  = pushSrcLocRn4 src_loc			  (
    mkTyVarNamesEnv src_loc [tyvar]	`thenRn4` \ (tv_env, [tyvar']) ->
    rnContext4 tv_env context	    	`thenRn4` \ context' ->
    lookupClass cname		    	`thenRn4` \ cname' ->
    mapRn4 (rn_op cname' tv_env) sigs   `thenRn4` \ sigs' ->
    rnMethodBinds4 cname' mbinds    	`thenRn4` \ mbinds' ->
    recoverQuietlyRn4 NoClassPragmas (
      rnClassPragmas4 pragmas
    )					`thenRn4` \ pragmas' ->
    returnRn4 (ClassDecl context' cname' tyvar' sigs' mbinds' pragmas' src_loc)
    )
  where
    rn_op clas tv_env (ClassOpSig op ty pragma locn)
      = pushSrcLocRn4 locn	    	      (
	lookupClassOp clas op		 `thenRn4` \ op_name ->
	rnPolyType4 False True tv_env ty `thenRn4` \ new_ty  ->
	recoverQuietlyRn4 NoClassOpPragmas (
	    rnClassOpPragmas4 pragma
	)			    `thenRn4` \ new_pragma ->
	returnRn4 (ClassOpSig op_name new_ty new_pragma locn)
	)
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************


@rnInstDecl4@ uses the `global name function' to create a new of
instance declaration in which local names have been replaced by their
original names, reporting any unknown names.

\begin{code}
rnInstDecl4 :: ProtoNameInstDecl -> Rn4M RenamedInstDecl

rnInstDecl4 (InstDecl context cname ty mbinds from_here modname imod uprags pragmas src_loc)
  = pushSrcLocRn4 src_loc		 	  (
    let  tyvars = extractMonoTyNames eqProtoName ty  in
    mkTyVarNamesEnv src_loc tyvars     	`thenRn4` \ (tv_env,_) ->
    rnContext4 tv_env context 	    	`thenRn4` \ context' ->
    lookupClass cname 		     	`thenRn4` \ cname' ->
    rnMonoType4 False{-no invisibles-} tv_env ty
					`thenRn4` \ ty' ->
    rnMethodBinds4 cname' mbinds	`thenRn4` \ mbinds' ->
    mapRn4 (rn_uprag cname') uprags	`thenRn4` \ new_uprags ->
    recoverQuietlyRn4 NoInstancePragmas (
	rnInstancePragmas4 cname' tv_env pragmas
    )					`thenRn4` \ new_pragmas ->
    returnRn4 (InstDecl context' cname' ty' mbinds'
			from_here modname imod new_uprags new_pragmas src_loc)
    )
  where
    rn_uprag class_name (SpecSig op ty using locn)
      = ASSERT(not (maybeToBool using))	-- ToDo: SPEC method with explicit spec_id
	pushSrcLocRn4 src_loc 				(
	lookupClassOp class_name op			`thenRn4` \ op_name ->
        rnPolyType4 False True nullTyVarNamesEnv ty	`thenRn4` \ new_ty ->
	returnRn4 (SpecSig op_name new_ty Nothing locn)
	)
    rn_uprag class_name (InlineSig op guide locn)
      = pushSrcLocRn4 locn	      	(
	lookupClassOp class_name op	`thenRn4` \ op_name ->
	returnRn4 (InlineSig op_name guide locn)
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
rnInstSpecSig4 :: ProtoNameSpecialisedInstanceSig
		-> Rn4M RenamedSpecialisedInstanceSig

rnInstSpecSig4 (InstSpecSig clas ty src_loc)
  = pushSrcLocRn4 src_loc		  (
    let  tyvars = extractMonoTyNames eqProtoName ty  in
    mkTyVarNamesEnv src_loc tyvars     	`thenRn4` \ (tv_env,_) ->
    lookupClass clas			`thenRn4` \ new_clas ->
    rnMonoType4 False tv_env ty		`thenRn4` \ new_ty ->
    returnRn4 (InstSpecSig new_clas new_ty src_loc)
    )
\end{code}

%*********************************************************
%*							*
\subsection{Default declarations}
%*							*
%*********************************************************

@rnDefaultDecl4@ uses the `global name function' to create a new set
of default declarations in which local names have been replaced by
their original names, reporting any unknown names.

\begin{code}
rnDefaultDecl4 :: ProtoNameDefaultDecl -> Rn4M RenamedDefaultDecl

rnDefaultDecl4 (DefaultDecl tys src_loc)
  = pushSrcLocRn4 src_loc		            	 (
    mapRn4 (rnMonoType4 False nullTyVarNamesEnv) tys `thenRn4` \ tys' ->
    returnRn4 (DefaultDecl tys' src_loc)
    )
\end{code}

%*************************************************************************
%*									*
\subsection{Type signatures from interfaces}
%*									*
%*************************************************************************

Non-interface type signatures (which may include user-pragmas) are
handled with @Binds@.

@ClassOpSigs@ are dealt with in class declarations.

\begin{code}
rnIntSig4 :: ProtoNameSig -> Rn4M RenamedSig

rnIntSig4 (Sig name ty pragma src_loc)
  = pushSrcLocRn4 src_loc			      (
    lookupValue name				`thenRn4` \ new_name ->
    rnPolyType4 False True nullTyVarNamesEnv ty `thenRn4` \ new_ty   ->
    recoverQuietlyRn4 NoGenPragmas (
	rnGenPragmas4 pragma
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
rnFixes4 :: [ProtoNameFixityDecl]  -> Rn4M [RenamedFixityDecl]

rnFixes4 fixities
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
rnPolyType4 :: Bool		-- True <=> "invisible" tycons (in pragmas) allowed 
	    -> Bool		-- True <=> snaffle tyvars from ty and
				--  stuff them in tyvar env; True for
				--  signatures and things; False for type
				--  synonym defns and things.
	    -> TyVarNamesEnv
	    -> ProtoNamePolyType
	    -> Rn4M RenamedPolyType

rnPolyType4 invisibles_allowed snaffle_tyvars tv_env (UnoverloadedTy ty)
  = rn_poly_help invisibles_allowed snaffle_tyvars tv_env [] ty `thenRn4` \ (_, new_ty) ->
    returnRn4 (UnoverloadedTy new_ty)

rnPolyType4 invisibles_allowed snaffle_tyvars tv_env (OverloadedTy ctxt ty)
  = rn_poly_help invisibles_allowed snaffle_tyvars tv_env ctxt ty `thenRn4` \ (new_ctxt, new_ty) ->
    returnRn4 (OverloadedTy new_ctxt new_ty)

rnPolyType4 invisibles_allowed snaffle_tyvars tv_env (ForAllTy tvs ty)
  = getSrcLocRn4 		`thenRn4` \ src_loc ->
    mkTyVarNamesEnv src_loc tvs `thenRn4` \ (tvenv2, new_tvs) ->
    let
	new_tvenv = catTyVarNamesEnvs tvenv2 tv_env
    in
    rnMonoType4 invisibles_allowed new_tvenv ty `thenRn4` \ new_ty ->
    returnRn4 (ForAllTy new_tvs new_ty)

------------
rn_poly_help invisibles_allowed snaffle_tyvars tv_env ctxt ty
  = getSrcLocRn4 		`thenRn4` \ src_loc ->
    let
	-- ToDo: this randomly-grabbing-tyvar names out
	-- of the type seems a little weird to me
	-- (WDP 94/11)

	new_tyvars
	  = extractMonoTyNames eqProtoName ty
	    `minus_list` domTyVarNamesEnv tv_env
    in
    mkTyVarNamesEnv src_loc new_tyvars 	`thenRn4` \ (tv_env2, _) ->
    let
	tv_env3 = if snaffle_tyvars
		  then catTyVarNamesEnvs tv_env2 tv_env
		  else tv_env -- leave it alone
    in
    rnContext4 tv_env3 ctxt		`thenRn4` \ new_ctxt ->
    rnMonoType4 invisibles_allowed tv_env3 ty
					`thenRn4` \ new_ty ->
    returnRn4 (new_ctxt, new_ty)
  where
    minus_list xs ys = [ x | x <- xs, not (x `elemProtoNames` ys)]
\end{code}

\begin{code}
rnMonoType4 :: Bool		-- allowed to look at invisible tycons
	    -> TyVarNamesEnv
	    -> ProtoNameMonoType
	    -> Rn4M RenamedMonoType

rnMonoType4 invisibles_allowed  tv_env (MonoTyVar tyvar)
  = lookupTyVarName tv_env tyvar 	`thenRn4` \ tyvar' ->
    returnRn4 (MonoTyVar tyvar')

rnMonoType4 invisibles_allowed  tv_env (ListMonoTy ty)
  = rnMonoType4 invisibles_allowed tv_env ty	`thenRn4` \ ty' ->
    returnRn4 (ListMonoTy ty')

rnMonoType4 invisibles_allowed  tv_env (FunMonoTy ty1 ty2)
  = andRn4 FunMonoTy (rnMonoType4 invisibles_allowed tv_env ty1)
		     (rnMonoType4 invisibles_allowed tv_env ty2)

rnMonoType4 invisibles_allowed  tv_env (TupleMonoTy tys)
  = mapRn4 (rnPolyType4 invisibles_allowed False tv_env) tys `thenRn4` \ tys' ->
    returnRn4 (TupleMonoTy tys')

rnMonoType4 invisibles_allowed tv_env (MonoTyCon name tys)
  = let
	lookup_fn = if invisibles_allowed
		    then lookupTyConEvenIfInvisible
		    else lookupTyCon
    in
    lookup_fn name			`thenRn4` \ tycon_name' ->
    mapRn4 (rnMonoType4 invisibles_allowed tv_env) tys	`thenRn4` \ tys' ->
    returnRn4 (MonoTyCon tycon_name' tys')

-- for unfoldings only:

rnMonoType4 invisibles_allowed tv_env (MonoTyVarTemplate name)
  = --pprTrace "rnMonoType4:MonoTyVarTemplate:" (ppAbove (ppr PprDebug name) (ppr PprDebug tv_env)) (
    lookupTyVarName tv_env name 	`thenRn4` \ new_name ->
    returnRn4 (MonoTyVarTemplate new_name)
    --)

rnMonoType4 invisibles_allowed tv_env (MonoDict clas ty)
  = lookupClass clas		`thenRn4` \ new_clas ->
    rnMonoType4 invisibles_allowed tv_env ty	`thenRn4` \ new_ty ->
    returnRn4 (MonoDict new_clas new_ty)

#ifdef DPH
rnMonoType4 invisibles_allowed tv_env (MonoTyProc tys ty)
  = mapRn4 (rnMonoType4 invisibles_allowed  tv_env) tys	`thenRn4` \ tys' ->
    rnMonoType4 invisibles_allowed   tv_env ty		`thenRn4` \ ty'  ->
    returnRn4 (MonoTyProc tys' ty')

rnMonoType4 invisibles_allowed tv_env (MonoTyPod ty)
  = rnMonoType4 invisibles_allowed   tv_env ty  `thenRn4` \ ty'  ->
    returnRn4 (MonoTyPod ty')
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
rnContext4 :: TyVarNamesEnv -> ProtoNameContext -> Rn4M RenamedContext

rnContext4 tv_env ctxt
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
rnDataPragmas4 tv_env (DataPragmas cons specs)
  = rnConDecls4 tv_env True{-invisibles allowed-} cons `thenRn4` \ new_cons ->
    mapRn4 types_n_spec specs			       `thenRn4` \ new_specs ->
    returnRn4 (DataPragmas new_cons new_specs)
  where
    types_n_spec ty_maybes
      = mapRn4 (rn_ty_maybe nullTyVarNamesEnv) ty_maybes
\end{code}

\begin{code}
rnClassOpPragmas4 NoClassOpPragmas = returnRn4 NoClassOpPragmas

rnClassOpPragmas4 (ClassOpPragmas dsel defm)
  = recoverQuietlyRn4 NoGenPragmas (rnGenPragmas4 dsel) `thenRn4` \ new_dsel ->
    recoverQuietlyRn4 NoGenPragmas (rnGenPragmas4 defm) `thenRn4` \ new_defm ->
    returnRn4 (ClassOpPragmas new_dsel new_defm)
\end{code}

\begin{code}
rnClassPragmas4 NoClassPragmas = returnRn4 NoClassPragmas

rnClassPragmas4 (SuperDictPragmas sds)
  = mapRn4 rnGenPragmas4 sds	`thenRn4` \ new_sds ->
    returnRn4 (SuperDictPragmas new_sds)
\end{code}

NB: In various cases around here, we don't @recoverQuietlyRn4@ around
calls to @rnGenPragmas4@; not really worth it.

\begin{code}
rnInstancePragmas4 _ _ NoInstancePragmas = returnRn4 NoInstancePragmas

rnInstancePragmas4 _ _ (SimpleInstancePragma dfun)
  = rnGenPragmas4 dfun	`thenRn4` \ new_dfun ->
    returnRn4 (SimpleInstancePragma new_dfun)

rnInstancePragmas4 clas tv_env (ConstantInstancePragma dfun constms)
  = recoverQuietlyRn4 NoGenPragmas (
	rnGenPragmas4 dfun
    )				`thenRn4` \ new_dfun ->
    mapRn4 name_n_gen constms	`thenRn4` \ new_constms ->
    returnRn4 (ConstantInstancePragma new_dfun new_constms)
  where
    name_n_gen (op, gen)
      = lookupClassOp clas op	`thenRn4` \ new_op ->
	rnGenPragmas4 gen	`thenRn4` \ new_gen ->
	returnRn4 (new_op, new_gen)

rnInstancePragmas4 clas tv_env (SpecialisedInstancePragma dfun specs)
  = recoverQuietlyRn4 NoGenPragmas (
	rnGenPragmas4 dfun
    )				`thenRn4` \ new_dfun ->
    mapRn4 types_n_spec specs	`thenRn4` \ new_specs ->
    returnRn4 (SpecialisedInstancePragma new_dfun new_specs)
  where
    types_n_spec (ty_maybes, dicts_to_ignore, inst)
      = mapRn4 (rn_ty_maybe tv_env) ty_maybes	`thenRn4` \ new_tys ->
	rnInstancePragmas4 clas tv_env inst 	`thenRn4` \ new_inst ->
	returnRn4 (new_tys, dicts_to_ignore, new_inst)
\end{code}

And some general pragma stuff: (Not sure what, if any, of this would
benefit from a TyVarNamesEnv passed in.... [ToDo])
\begin{code}
rnGenPragmas4 NoGenPragmas = returnRn4 NoGenPragmas

rnGenPragmas4 (GenPragmas arity upd def strict unfold specs)
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
	    rnGenPragmas4 wrkr_info
	) 			`thenRn4` \ new_wrkr_info ->
	returnRn4 (ImpStrictness is_bot ww_info new_wrkr_info)

    -------------
    types_n_gen (ty_maybes, dicts_to_ignore, gen)
      = mapRn4 (rn_ty_maybe no_env) ty_maybes	`thenRn4` \ new_tys ->
	recoverQuietlyRn4 NoGenPragmas (
	    rnGenPragmas4 gen
	)				`thenRn4` \ new_gen ->
	returnRn4 (new_tys, dicts_to_ignore, new_gen)
      where
	no_env = nullTyVarNamesEnv

------------
rn_ty_maybe tv_env Nothing = returnRn4 Nothing

rn_ty_maybe tv_env (Just ty)
  = rnMonoType4 True{-invisibles OK-} tv_env ty  `thenRn4` \ new_ty ->
    returnRn4 (Just new_ty)

------------
rn_core tvenv (UfCoVar v)
  = rn_uf_id tvenv v	`thenRn4` \ vname ->
    returnRn4 (UfCoVar vname)

rn_core tvenv (UfCoLit lit)
  = returnRn4 (UfCoLit lit)

rn_core tvenv (UfCoCon con tys as)
  = lookupValueEvenIfInvisible con	`thenRn4` \ new_con ->
    mapRn4 (rn_core_type tvenv) tys	`thenRn4` \ new_tys ->
    mapRn4 (rn_atom tvenv) as   	`thenRn4` \ new_as ->
    returnRn4 (UfCoCon new_con new_tys new_as)

rn_core tvenv (UfCoPrim op tys as)
  = rn_core_primop tvenv op		`thenRn4` \ new_op ->
    mapRn4 (rn_core_type tvenv) tys	`thenRn4` \ new_tys ->
    mapRn4 (rn_atom tvenv) as   	`thenRn4` \ new_as ->
    returnRn4 (UfCoPrim new_op new_tys new_as)

rn_core tvenv (UfCoLam binders body)
  = mapRn4 (rn_binder tvenv) binders `thenRn4` \ new_binders ->
    let
	bs = [ b | (b, ty) <- new_binders ]
    in
    extendSS bs (rn_core tvenv body) `thenRn4` \ new_body ->
    returnRn4 (UfCoLam new_binders new_body)

rn_core tvenv (UfCoTyLam tv body)
  = getSrcLocRn4 		    	`thenRn4` \ src_loc ->
    mkTyVarNamesEnv src_loc [tv] 	`thenRn4` \ (tvenv2, [new_tv]) ->
    let
	new_tvenv = catTyVarNamesEnvs tvenv2 tvenv
    in
    rn_core new_tvenv body		`thenRn4` \ new_body ->
    returnRn4 (UfCoTyLam new_tv new_body)

rn_core tvenv (UfCoApp fun arg)
  = rn_core tvenv fun	`thenRn4` \ new_fun ->
    rn_atom tvenv arg	`thenRn4` \ new_arg ->
    returnRn4 (UfCoApp new_fun new_arg)

rn_core tvenv (UfCoTyApp expr ty)
  = rn_core tvenv expr	    `thenRn4` \ new_expr ->
    rn_core_type tvenv ty   `thenRn4` \ new_ty ->
    returnRn4 (UfCoTyApp new_expr new_ty)

rn_core tvenv (UfCoCase expr alts)
  = rn_core tvenv expr	    `thenRn4` \ new_expr ->
    rn_alts 	  alts	    `thenRn4` \ new_alts ->
    returnRn4 (UfCoCase new_expr new_alts)
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

rn_core tvenv (UfCoLet bind body)
  = rn_bind bind			      `thenRn4` \ (new_bind, new_binders) ->
    extendSS new_binders (rn_core tvenv body) `thenRn4` \ new_body ->
    returnRn4 (UfCoLet new_bind new_body)
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

rn_core tvenv (UfCoSCC uf_cc body)
  = rn_cc uf_cc		`thenRn4` \ new_cc ->
    rn_core tvenv body	`thenRn4` \ new_body ->
    returnRn4 (UfCoSCC new_cc new_body)
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
  = rnPolyType4 True{-invisible tycons OK-} False tvenv ty
\end{code}

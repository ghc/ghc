%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
module TcMonoType ( tcHsType, tcHsSigType, tcHsBoxedSigType, 
		    tcContext, tcClassContext,

			-- Kind checking
		    kcHsTyVar, kcHsTyVars, mkTyClTyVars,
		    kcHsType, kcHsSigType, kcHsBoxedSigType, kcHsContext,
		    tcTyVars, tcHsTyVars, mkImmutTyVars,

		    TcSigInfo(..), tcTySig, mkTcSig, maybeSig,
		    checkSigTyVars, sigCtxt, sigPatCtxt
	          ) where

#include "HsVersions.h"

import HsSyn		( HsType(..), HsTyVarBndr(..), HsUsageAnn(..),
                          Sig(..), HsPred(..), pprParendHsType, HsTupCon(..), hsTyVarNames )
import RnHsSyn		( RenamedHsType, RenamedHsPred, RenamedContext, RenamedSig )
import TcHsSyn		( TcId )

import TcMonad
import TcEnv		( tcExtendTyVarEnv, tcLookupTy, tcGetValueEnv, tcGetInScopeTyVars,
                          tcExtendUVarEnv, tcLookupUVar,
			  tcGetGlobalTyVars, valueEnvIds, 
		 	  TyThing(..), tcExtendKindEnv
			)
import TcType		( TcType, TcKind, TcTyVar, TcThetaType, TcTauType,
			  newKindVar, tcInstSigVar,
			  zonkKindEnv, zonkTcType, zonkTcTyVars, zonkTcTyVar
			)
import Inst		( Inst, InstOrigin(..), newMethodWithGivenTy, instToIdBndr,
			  instFunDeps, instFunDepsOfTheta )
import FunDeps		( tyVarFunDep, oclose )
import TcUnify		( unifyKind, unifyOpenTypeKind )
import Type		( Type, Kind, PredType(..), ThetaType, UsageAnn(..),
			  mkTyVarTy, mkTyVarTys, mkFunTy, mkSynTy, mkUsgTy,
                          mkUsForAllTy, zipFunTys, hoistForAllTys,
			  mkSigmaTy, mkPredTy, mkTyConApp,
			  mkAppTys, splitForAllTys, splitRhoTy, mkRhoTy,
			  boxedTypeKind, unboxedTypeKind, mkArrowKind,
			  mkArrowKinds, getTyVar_maybe, getTyVar, splitFunTy_maybe,
		  	  tidyOpenType, tidyOpenTypes, tidyTyVar, tidyTyVars,
			  tyVarsOfType, tyVarsOfPred, mkForAllTys,
			  classesOfPreds, isUnboxedTupleType, isForAllTy
			)
import PprType		( pprType, pprPred )
import Subst		( mkTopTyVarSubst, substTy )
import Id		( mkVanillaId, idName, idType, idFreeTyVars )
import Var		( TyVar, mkTyVar, tyVarKind, mkNamedUVar )
import VarEnv
import VarSet
import ErrUtils		( Message )
import TyCon		( TyCon, isSynTyCon, tyConArity, tyConKind, tyConName )
import Class		( ClassContext, classArity, classTyCon )
import Name		( Name, isLocallyDefined )
import TysWiredIn	( mkListTy, mkTupleTy, genUnitTyCon )
import UniqFM		( elemUFM )
import BasicTypes	( Boxity(..) )
import SrcLoc		( SrcLoc )
import Util		( mapAccumL, isSingleton )
import Outputable

\end{code}


%************************************************************************
%*									*
\subsection{Kind checking}
%*									*
%************************************************************************

Kind checking
~~~~~~~~~~~~~
When we come across the binding site for some type variables, we
proceed in two stages

1. Figure out what kind each tyvar has

2. Create suitably-kinded tyvars, 
   extend the envt, 
   and typecheck the body

To do step 1, we proceed thus:

1a. Bind each type variable to a kind variable
1b. Apply the kind checker
1c. Zonk the resulting kinds

The kind checker is passed to tcHsTyVars as an argument.  

For example, when we find
	(forall a m. m a -> m a)
we bind a,m to kind varibles and kind-check (m a -> m a).  This
makes a get kind *, and m get kind *->*.  Now we typecheck (m a -> m a)
in an environment that binds a and m suitably.

The kind checker passed to tcHsTyVars needs to look at enough to
establish the kind of the tyvar:
  * For a group of type and class decls, it's just the group, not
	the rest of the program
  * For a tyvar bound in a pattern type signature, its the types
	mentioned in the other type signatures in that bunch of patterns
  * For a tyvar bound in a RULE, it's the type signatures on other
	universally quantified variables in the rule

Note that this may occasionally give surprising results.  For example:

	data T a b = MkT (a b)

Here we deduce			a::*->*, b::*.
But equally valid would be
				a::(*->*)-> *, b::*->*

\begin{code}
tcHsTyVars :: [HsTyVarBndr Name] 
	   -> TcM s a				-- The kind checker
	   -> ([TyVar] -> TcM s b)
	   -> TcM s b

tcHsTyVars [] kind_check thing_inside = thing_inside []
	-- A useful short cut for a common case!
  
tcHsTyVars tv_names kind_check thing_inside
  = kcHsTyVars tv_names 				`thenNF_Tc` \ tv_names_w_kinds ->
    tcExtendKindEnv tv_names_w_kinds kind_check		`thenTc_`
    zonkKindEnv tv_names_w_kinds			`thenNF_Tc` \ tvs_w_kinds ->
    let
	tyvars = mkImmutTyVars tvs_w_kinds
    in
    tcExtendTyVarEnv tyvars (thing_inside tyvars)

tcTyVars :: [Name] 
	     -> TcM s a				-- The kind checker
	     -> TcM s [TyVar]
tcTyVars [] kind_check = returnTc []

tcTyVars tv_names kind_check
  = mapNF_Tc newNamedKindVar tv_names		`thenTc` \ kind_env ->
    tcExtendKindEnv kind_env kind_check		`thenTc_`
    zonkKindEnv kind_env			`thenNF_Tc` \ tvs_w_kinds ->
    listNF_Tc [tcNewSigTyVar name kind | (name,kind) <- tvs_w_kinds]
\end{code}
    

\begin{code}
kcHsTyVar  :: HsTyVarBndr name   -> NF_TcM s (name, TcKind)
kcHsTyVars :: [HsTyVarBndr name] -> NF_TcM s [(name, TcKind)]

kcHsTyVar (UserTyVar name)       = newNamedKindVar name
kcHsTyVar (IfaceTyVar name kind) = returnNF_Tc (name, kind)

kcHsTyVars tvs = mapNF_Tc kcHsTyVar tvs

newNamedKindVar name = newKindVar	`thenNF_Tc` \ kind ->
		       returnNF_Tc (name, kind)

---------------------------
kcBoxedType :: RenamedHsType -> TcM s ()
	-- The type ty must be a *boxed* *type*
kcBoxedType ty
  = kcHsType ty				`thenTc` \ kind ->
    tcAddErrCtxt (typeKindCtxt ty)	$
    unifyKind boxedTypeKind kind
    
---------------------------
kcTypeType :: RenamedHsType -> TcM s ()
	-- The type ty must be a *type*, but it can be boxed or unboxed.
kcTypeType ty
  = kcHsType ty				`thenTc` \ kind ->
    tcAddErrCtxt (typeKindCtxt ty)	$
    unifyOpenTypeKind kind

---------------------------
kcHsSigType, kcHsBoxedSigType :: RenamedHsType -> TcM s ()
	-- Used for type signatures
kcHsSigType  	 = kcTypeType
kcHsBoxedSigType = kcBoxedType

---------------------------
kcHsType :: RenamedHsType -> TcM s TcKind
kcHsType (HsTyVar name)	      = kcTyVar name
kcHsType (HsUsgTy _ ty)       = kcHsType ty
kcHsType (HsUsgForAllTy _ ty) = kcHsType ty

kcHsType (HsListTy ty)
  = kcBoxedType ty		`thenTc` \ tau_ty ->
    returnTc boxedTypeKind

kcHsType (HsTupleTy (HsTupCon _ Boxed) tys)
  = mapTc kcBoxedType tys	`thenTc_` 
    returnTc boxedTypeKind

kcHsType ty@(HsTupleTy (HsTupCon _ Unboxed) tys)
  = failWithTc (unboxedTupleErr ty)
	-- Unboxed tuples are illegal everywhere except
	-- just after a function arrow (see kcFunResType)

kcHsType (HsFunTy ty1 ty2)
  = kcTypeType ty1	`thenTc_`
    kcFunResType ty2	`thenTc_`
    returnTc boxedTypeKind

kcHsType ty@(HsOpTy ty1 op ty2)
  = kcTyVar op				`thenTc` \ op_kind ->
    kcHsType ty1			`thenTc` \ ty1_kind ->
    kcHsType ty2			`thenTc` \ ty2_kind ->
    tcAddErrCtxt (appKindCtxt (ppr ty))	$
    kcAppKind op_kind  ty1_kind		`thenTc` \ op_kind' ->
    kcAppKind op_kind' ty2_kind
   
kcHsType (HsPredTy pred)
  = kcHsPred pred		`thenTc_`
    returnTc boxedTypeKind

kcHsType ty@(HsAppTy ty1 ty2)
  = kcHsType ty1			`thenTc` \ tc_kind ->
    kcHsType ty2			`thenTc` \ arg_kind ->
    tcAddErrCtxt (appKindCtxt (ppr ty))	$
    kcAppKind tc_kind arg_kind

kcHsType (HsForAllTy (Just tv_names) context ty)
  = kcHsTyVars tv_names		`thenNF_Tc` \ kind_env ->
    tcExtendKindEnv kind_env	$
    kcHsContext context		`thenTc_`
 
	-- Context behaves like a function type
	-- This matters.  Return-unboxed-tuple analysis can
	-- give overloaded functions like
	--	f :: forall a. Num a => (# a->a, a->a #)
	-- And we want these to get through the type checker
    if null context then
	kcHsType ty
    else
	kcFunResType ty		`thenTc_`
	returnTc boxedTypeKind

---------------------------
kcTyVar name
  = tcLookupTy name	`thenTc` \ thing ->
    case thing of
	ATyVar tv -> returnTc (tyVarKind tv)
	ATyCon tc -> returnTc (tyConKind tc)
	AThing k  -> returnTc k
	other	  -> failWithTc (wrongThingErr "type" thing name)

---------------------------
kcFunResType :: RenamedHsType -> TcM s TcKind
-- The only place an unboxed tuple type is allowed
-- is at the right hand end of an arrow
kcFunResType (HsTupleTy (HsTupCon _ Unboxed) tys)
  = mapTc kcTypeType tys	`thenTc_` 
    returnTc unboxedTypeKind

kcFunResType ty = kcHsType ty

---------------------------
kcAppKind fun_kind arg_kind
  = case splitFunTy_maybe fun_kind of 
	Just (arg_kind', res_kind)
		-> unifyKind arg_kind arg_kind'	`thenTc_`
		   returnTc res_kind

	Nothing -> newKindVar 						`thenNF_Tc` \ res_kind ->
		   unifyKind fun_kind (mkArrowKind arg_kind res_kind)	`thenTc_`
		   returnTc res_kind


---------------------------
kcHsContext ctxt = mapTc_ kcHsPred ctxt

kcHsPred :: RenamedHsPred -> TcM s ()
kcHsPred pred@(HsPIParam name ty)
  = tcAddErrCtxt (appKindCtxt (ppr pred))	$
    kcBoxedType ty

kcHsPred pred@(HsPClass cls tys)
  = tcAddErrCtxt (appKindCtxt (ppr pred))	$
    tcLookupTy cls 				`thenNF_Tc` \ thing -> 
    (case thing of
	AClass cls  -> returnTc (tyConKind (classTyCon cls))
	AThing kind -> returnTc kind
	other -> failWithTc (wrongThingErr "class" thing cls))	`thenTc` \ kind ->
    mapTc kcHsType tys						`thenTc` \ arg_kinds ->
    unifyKind kind (mkArrowKinds arg_kinds boxedTypeKind)
\end{code}

%************************************************************************
%*									*
\subsection{Checking types}
%*									*
%************************************************************************

tcHsSigType and tcHsBoxedSigType
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tcHsSigType and tcHsBoxedSigType are used for type signatures written by the programmer

  * We hoist any inner for-alls to the top

  * Notice that we kind-check first, because the type-check assumes
	that the kinds are already checked.

  * They are only called when there are no kind vars in the environment
  	so the kind returned is indeed a Kind not a TcKind

\begin{code}
tcHsSigType :: RenamedHsType -> TcM s TcType
tcHsSigType ty
  = kcTypeType ty	`thenTc_`
    tcHsType ty		`thenTc` \ ty' ->
    returnTc (hoistForAllTys ty')

tcHsBoxedSigType :: RenamedHsType -> TcM s Type
tcHsBoxedSigType ty
  = kcBoxedType ty	`thenTc_`
    tcHsType ty		`thenTc` \ ty' ->
    returnTc (hoistForAllTys ty')
\end{code}


tcHsType, the main work horse
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
tcHsType :: RenamedHsType -> TcM s Type
tcHsType ty@(HsTyVar name)
  = tc_app ty []

tcHsType (HsListTy ty)
  = tcHsArgType ty		`thenTc` \ tau_ty ->
    returnTc (mkListTy tau_ty)

tcHsType (HsTupleTy (HsTupCon _ Boxed) tys)
  = mapTc tcHsArgType tys	`thenTc` \ tau_tys ->
    returnTc (mkTupleTy Boxed (length tys) tau_tys)

tcHsType (HsTupleTy (HsTupCon _ Unboxed) tys)
  =	-- Unboxed tuples can have polymorphic args.
	-- This happens in the workers for functions returning
	-- product types with polymorphic components
    mapTc tcHsType tys		`thenTc` \ tau_tys ->
    returnTc (mkTupleTy Unboxed (length tys) tau_tys)

tcHsType (HsFunTy ty1 ty2)
  = tcHsType ty1	`thenTc` \ tau_ty1 ->
    tcHsType ty2	`thenTc` \ tau_ty2 ->
    returnTc (mkFunTy tau_ty1 tau_ty2)

tcHsType (HsNumTy n)
  = ASSERT(n== 1)
    returnTc (mkTyConApp genUnitTyCon [])

tcHsType (HsOpTy ty1 op ty2)
  = tcHsArgType ty1 `thenTc` \ tau_ty1 ->
    tcHsArgType ty2 `thenTc` \ tau_ty2 ->
    tc_fun_type op [tau_ty1,tau_ty2]

tcHsType (HsAppTy ty1 ty2)
  = tc_app ty1 [ty2]

tcHsType (HsPredTy pred)
  = tcClassAssertion True pred	`thenTc` \ pred' ->
    returnTc (mkPredTy pred')

tcHsType (HsUsgTy usg ty)
  = newUsg usg			`thenTc` \ usg' ->
    tcHsType ty			`thenTc` \ tc_ty ->
    returnTc (mkUsgTy usg' tc_ty)
  where
    newUsg usg = case usg of
                   HsUsOnce        -> returnTc UsOnce
                   HsUsMany        -> returnTc UsMany
                   HsUsVar uv_name -> tcLookupUVar uv_name `thenTc` \ uv ->
                                      returnTc (UsVar uv)

tcHsType (HsUsgForAllTy uv_name ty)
  = let
        uv = mkNamedUVar uv_name
    in
    tcExtendUVarEnv uv_name uv $
    tcHsType ty                     `thenTc` \ tc_ty ->
    returnTc (mkUsForAllTy uv tc_ty)

tcHsType full_ty@(HsForAllTy (Just tv_names) ctxt ty)
  = let
	kind_check = kcHsContext ctxt `thenTc_` kcFunResType ty
    in
    tcHsTyVars tv_names kind_check		$ \ tyvars ->
    tcContext ctxt				`thenTc` \ theta ->
    tcHsType ty					`thenTc` \ tau ->
    checkAmbiguity full_ty tyvars theta tau	`thenTc_`
    returnTc (mkSigmaTy tyvars theta tau)

  -- Check for ambiguity
  --   forall V. P => tau
  -- is ambiguous if P contains generic variables
  -- (i.e. one of the Vs) that are not mentioned in tau
  --
  -- However, we need to take account of functional dependencies
  -- when we speak of 'mentioned in tau'.  Example:
  --	class C a b | a -> b where ...
  -- Then the type
  --	forall x y. (C x y) => x
  -- is not ambiguous because x is mentioned and x determines y
  --
  -- NOTE: In addition, GHC insists that at least one type variable
  -- in each constraint is in V.  So we disallow a type like
  --	forall a. Eq b => b -> b
  -- even in a scope where b is in scope.
  -- This is the is_free test below.

checkAmbiguity full_ty forall_tyvars theta tau
  = mapTc check_pred theta
  where
    tau_vars	      = tyVarsOfType tau
    fds		      = instFunDepsOfTheta theta
    tvFundep	      = tyVarFunDep fds
    extended_tau_vars = oclose tvFundep tau_vars

    is_ambig ct_var   = (ct_var `elem` forall_tyvars) &&
		        not (ct_var `elemUFM` extended_tau_vars)
    is_free ct_var    = not (ct_var `elem` forall_tyvars)
    
    check_pred pred = checkTc (not any_ambig) (ambigErr pred full_ty) `thenTc_`
	    	      checkTc (not all_free)  (freeErr  pred full_ty)
             where 
	    	ct_vars	  = varSetElems (tyVarsOfPred pred)
	    	all_free  = all is_free ct_vars
	    	any_ambig = is_source_polytype && any is_ambig ct_vars
    
    -- Notes on the 'is_source_polytype' test above
    -- Check ambiguity only for source-program types, not
    -- for types coming from inteface files.  The latter can
    -- legitimately have ambiguous types. Example
    --    class S a where s :: a -> (Int,Int)
    --    instance S Char where s _ = (1,1)
    --    f:: S a => [a] -> Int -> (Int,Int)
    --    f (_::[a]) x = (a*x,b)
    --	where (a,b) = s (undefined::a)
    -- Here the worker for f gets the type
    --	fw :: forall a. S a => Int -> (# Int, Int #)
    --
    -- If the list of tv_names is empty, we have a monotype,
    -- and then we don't need to check for ambiguity either,
    -- because the test can't fail (see is_ambig).
    is_source_polytype 
	= case full_ty of
	    HsForAllTy (Just (UserTyVar _ : _)) _ _ -> True
    	    other			  	    -> False
\end{code}

Help functions for type applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
tc_app :: RenamedHsType -> [RenamedHsType] -> TcM s Type
tc_app (HsAppTy ty1 ty2) tys
  = tc_app ty1 (ty2:tys)

tc_app ty tys
  = tcAddErrCtxt (appKindCtxt pp_app)	$
    mapTc tcHsArgType tys		`thenTc` \ arg_tys ->
    case ty of
	HsTyVar fun -> tc_fun_type fun arg_tys
	other	    -> tcHsType ty		`thenTc` \ fun_ty ->
		       returnNF_Tc (mkAppTys fun_ty arg_tys)
  where
    pp_app = ppr ty <+> sep (map pprParendHsType tys)

tcHsArgType arg_ty	-- Check that the argument of a type appplication
			-- isn't a for-all type
  = tcHsType arg_ty				`thenTc` \ arg_ty' ->
    checkTc (not (isForAllTy arg_ty'))
	    (argTyErr arg_ty)			`thenTc_`
    returnTc arg_ty'

-- (tc_fun_type ty arg_tys) returns (mkAppTys ty arg_tys)
-- But not quite; for synonyms it checks the correct arity, and builds a SynTy
-- 	hence the rather strange functionality.

tc_fun_type name arg_tys
  = tcLookupTy name			`thenTc` \ thing ->
    case thing of
	ATyVar tv -> returnTc (mkAppTys (mkTyVarTy tv) arg_tys)

	ATyCon tc | isSynTyCon tc ->  checkTc arity_ok err_msg	`thenTc_`
				      returnTc (mkAppTys (mkSynTy tc (take arity arg_tys))
							 (drop arity arg_tys))

		  | otherwise	  ->  returnTc (mkTyConApp tc arg_tys)
		  where

		    arity_ok = arity <= n_args 
		    arity = tyConArity tc
			-- It's OK to have an *over-applied* type synonym
			--	data Tree a b = ...
			--	type Foo a = Tree [a]
			--	f :: Foo a b -> ...
		    err_msg = arityErr "Type synonym" name arity n_args
		    n_args  = length arg_tys

	other -> failWithTc (wrongThingErr "type constructor" thing name)
\end{code}


Contexts
~~~~~~~~
\begin{code}
tcClassContext :: RenamedContext -> TcM s ClassContext
	-- Used when we are expecting a ClassContext (i.e. no implicit params)
tcClassContext context
  = tcContext context 	`thenTc` \ theta ->
    returnTc (classesOfPreds theta)

tcContext :: RenamedContext -> TcM s ThetaType
tcContext context = mapTc (tcClassAssertion False) context

tcClassAssertion ccall_ok assn@(HsPClass class_name tys)
  = tcAddErrCtxt (appKindCtxt (ppr assn))	$
    mapTc tcHsArgType tys			`thenTc` \ arg_tys ->
    tcLookupTy class_name			`thenTc` \ thing ->
    case thing of
	AClass clas -> checkTc (arity == n_tys) err				`thenTc_`
		       returnTc (Class clas arg_tys)
	    where
		arity = classArity clas
		n_tys = length tys
		err   = arityErr "Class" class_name arity n_tys

	other -> failWithTc (wrongThingErr "class" thing class_name)

tcClassAssertion ccall_ok assn@(HsPIParam name ty)
  = tcAddErrCtxt (appKindCtxt (ppr assn))	$
    tcHsType ty					`thenTc` \ arg_ty ->
    returnTc (IParam name arg_ty)
\end{code}


%************************************************************************
%*									*
\subsection{Type variables, with knot tying!}
%*									*
%************************************************************************

\begin{code}
mkImmutTyVars :: [(Name,Kind)] -> [TyVar]
mkImmutTyVars pairs = [mkTyVar name kind | (name, kind) <- pairs]

mkTyClTyVars :: Kind 			-- Kind of the tycon or class
	     -> [HsTyVarBndr Name]
	     -> [TyVar]
mkTyClTyVars kind tyvar_names
  = mkImmutTyVars tyvars_w_kinds
  where
    (tyvars_w_kinds, _) = zipFunTys (hsTyVarNames tyvar_names) kind
\end{code}


%************************************************************************
%*									*
\subsection{Signatures}
%*									*
%************************************************************************

@tcSigs@ checks the signatures for validity, and returns a list of
{\em freshly-instantiated} signatures.  That is, the types are already
split up, and have fresh type variables installed.  All non-type-signature
"RenamedSigs" are ignored.

The @TcSigInfo@ contains @TcTypes@ because they are unified with
the variable's type, and after that checked to see whether they've
been instantiated.

\begin{code}
data TcSigInfo
  = TySigInfo	    
	Name			-- N, the Name in corresponding binding

	TcId			-- *Polymorphic* binder for this value...
				-- Has name = N

	[TcTyVar]		-- tyvars
	TcThetaType		-- theta
	TcTauType		-- tau

	TcId			-- *Monomorphic* binder for this value
				-- Does *not* have name = N
				-- Has type tau

	[Inst]			-- Empty if theta is null, or
				-- (method mono_id) otherwise

	SrcLoc			-- Of the signature

instance Outputable TcSigInfo where
    ppr (TySigInfo nm id tyvars theta tau _ inst loc) =
	ppr nm <+> ptext SLIT("::") <+> ppr tyvars <+> ppr theta <+> ptext SLIT("=>") <+> ppr tau

maybeSig :: [TcSigInfo] -> Name -> Maybe (TcSigInfo)
	-- Search for a particular signature
maybeSig [] name = Nothing
maybeSig (sig@(TySigInfo sig_name _ _ _ _ _ _ _) : sigs) name
  | name == sig_name = Just sig
  | otherwise	     = maybeSig sigs name
\end{code}


\begin{code}
tcTySig :: RenamedSig -> TcM s TcSigInfo

tcTySig (Sig v ty src_loc)
 = tcAddSrcLoc src_loc				$ 
   tcAddErrCtxt (tcsigCtxt v) 			$
   tcHsSigType ty				`thenTc` \ sigma_tc_ty ->
   mkTcSig (mkVanillaId v sigma_tc_ty) src_loc	`thenNF_Tc` \ sig -> 
   returnTc sig

mkTcSig :: TcId -> SrcLoc -> NF_TcM s TcSigInfo
mkTcSig poly_id src_loc
  = 	-- Instantiate this type
	-- It's important to do this even though in the error-free case
	-- we could just split the sigma_tc_ty (since the tyvars don't
	-- unified with anything).  But in the case of an error, when
	-- the tyvars *do* get unified with something, we want to carry on
	-- typechecking the rest of the program with the function bound
	-- to a pristine type, namely sigma_tc_ty
   let
	(tyvars, rho) = splitForAllTys (idType poly_id)
   in
   mapNF_Tc tcInstSigVar tyvars		`thenNF_Tc` \ tyvars' ->
	-- Make *signature* type variables

   let
     tyvar_tys' = mkTyVarTys tyvars'
     rho' = substTy (mkTopTyVarSubst tyvars tyvar_tys') rho
	-- mkTopTyVarSubst because the tyvars' are fresh
     (theta', tau') = splitRhoTy rho'
	-- This splitRhoTy tries hard to make sure that tau' is a type synonym
	-- wherever possible, which can improve interface files.
   in
   newMethodWithGivenTy SignatureOrigin 
		poly_id
		tyvar_tys'
		theta' tau'			`thenNF_Tc` \ inst ->
	-- We make a Method even if it's not overloaded; no harm
   instFunDeps SignatureOrigin theta'		`thenNF_Tc` \ fds ->
	
   returnNF_Tc (TySigInfo name poly_id tyvars' theta' tau' (instToIdBndr inst) (inst : fds) src_loc)
  where
    name = idName poly_id
\end{code}



%************************************************************************
%*									*
\subsection{Checking signature type variables}
%*									*
%************************************************************************

@checkSigTyVars@ is used after the type in a type signature has been unified with
the actual type found.  It then checks that the type variables of the type signature
are
	(a) Still all type variables
		eg matching signature [a] against inferred type [(p,q)]
		[then a will be unified to a non-type variable]

	(b) Still all distinct
		eg matching signature [(a,b)] against inferred type [(p,p)]
		[then a and b will be unified together]

	(c) Not mentioned in the environment
		eg the signature for f in this:

			g x = ... where
					f :: a->[a]
					f y = [x,y]

		Here, f is forced to be monorphic by the free occurence of x.

	(d) Not (unified with another type variable that is) in scope.
		eg f x :: (r->r) = (\y->y) :: forall a. a->r
	    when checking the expression type signature, we find that
	    even though there is nothing in scope whose type mentions r,
	    nevertheless the type signature for the expression isn't right.

	    Another example is in a class or instance declaration:
		class C a where
		   op :: forall b. a -> b
		   op x = x
	    Here, b gets unified with a

Before doing this, the substitution is applied to the signature type variable.

We used to have the notion of a "DontBind" type variable, which would
only be bound to itself or nothing.  Then points (a) and (b) were 
self-checking.  But it gave rise to bogus consequential error messages.
For example:

   f = (*)	-- Monomorphic

   g :: Num a => a -> a
   g x = f x x

Here, we get a complaint when checking the type signature for g,
that g isn't polymorphic enough; but then we get another one when
dealing with the (Num x) context arising from f's definition;
we try to unify x with Int (to default it), but find that x has already
been unified with the DontBind variable "a" from g's signature.
This is really a problem with side-effecting unification; we'd like to
undo g's effects when its type signature fails, but unification is done
by side effect, so we can't (easily).

So we revert to ordinary type variables for signatures, and try to
give a helpful message in checkSigTyVars.

\begin{code}
checkSigTyVars :: [TcTyVar]		-- Universally-quantified type variables in the signature
	       -> TcTyVarSet		-- Tyvars that are free in the type signature
					-- These should *already* be in the global-var set, and are
					-- used here only to improve the error message
	       -> TcM s [TcTyVar]	-- Zonked signature type variables

checkSigTyVars [] free = returnTc []

checkSigTyVars sig_tyvars free_tyvars
  = zonkTcTyVars sig_tyvars		`thenNF_Tc` \ sig_tys ->
    tcGetGlobalTyVars			`thenNF_Tc` \ globals ->

    checkTcM (all_ok sig_tys globals)
	     (complain sig_tys globals)	`thenTc_`

    returnTc (map (getTyVar "checkSigTyVars") sig_tys)

  where
    all_ok []       acc = True
    all_ok (ty:tys) acc = case getTyVar_maybe ty of
			    Nothing 		          -> False	-- Point (a)
			    Just tv | tv `elemVarSet` acc -> False	-- Point (b) or (c)
				    | otherwise           -> all_ok tys (acc `extendVarSet` tv)
    

    complain sig_tys globals
      = -- For the in-scope ones, zonk them and construct a map
	-- from the zonked tyvar to the in-scope one
	-- If any of the in-scope tyvars zonk to a type, then ignore them;
	-- that'll be caught later when we back up to their type sig
	tcGetInScopeTyVars			`thenNF_Tc` \ in_scope_tvs ->
	zonkTcTyVars in_scope_tvs		`thenNF_Tc` \ in_scope_tys ->
	let
	    in_scope_assoc = [ (zonked_tv, in_scope_tv) 
			     | (z_ty, in_scope_tv) <- in_scope_tys `zip` in_scope_tvs,
			       Just zonked_tv <- [getTyVar_maybe z_ty]
    			     ]
	    in_scope_env = mkVarEnv in_scope_assoc
	in

	-- "check" checks each sig tyvar in turn
        foldlNF_Tc check
		   (env2, in_scope_env, [])
		   (tidy_tvs `zip` tidy_tys)	`thenNF_Tc` \ (env3, _, msgs) ->

        failWithTcM (env3, main_msg $$ nest 4 (vcat msgs))
      where
	(env1, tidy_tvs) = mapAccumL tidyTyVar emptyTidyEnv sig_tyvars
	(env2, tidy_tys) = tidyOpenTypes env1 sig_tys

	main_msg = ptext SLIT("Inferred type is less polymorphic than expected")

	check (env, acc, msgs) (sig_tyvar,ty)
		-- sig_tyvar is from the signature;
		-- ty is what you get if you zonk sig_tyvar and then tidy it
		--
		-- acc maps a zonked type variable back to a signature type variable
	  = case getTyVar_maybe ty of {
	      Nothing ->			-- Error (a)!
			returnNF_Tc (env, acc, unify_msg sig_tyvar (ppr ty) : msgs) ;

	      Just tv ->

	    case lookupVarEnv acc tv of {
		Just sig_tyvar' -> 	-- Error (b) or (d)!
			returnNF_Tc (env, acc, unify_msg sig_tyvar (ppr sig_tyvar') : msgs) ;

		Nothing ->

	    if tv `elemVarSet` globals	-- Error (c)! Type variable escapes
					-- The least comprehensible, so put it last
	    then   tcGetValueEnv 					`thenNF_Tc` \ ve ->
        	   find_globals tv env  [] (valueEnvIds ve)		`thenNF_Tc` \ (env1, globs) ->
        	   find_frees   tv env1 [] (varSetElems free_tyvars)	`thenNF_Tc` \ (env2, frees) ->
		   returnNF_Tc (env2, acc, escape_msg sig_tyvar tv globs frees : msgs)

	    else 	-- All OK
	    returnNF_Tc (env, extendVarEnv acc tv sig_tyvar, msgs)
	    }}

-- find_globals looks at the value environment and finds values
-- whose types mention the offending type variable.  It has to be 
-- careful to zonk the Id's type first, so it has to be in the monad.
-- We must be careful to pass it a zonked type variable, too.
find_globals tv tidy_env acc []
  = returnNF_Tc (tidy_env, acc)

find_globals tv tidy_env acc (id:ids) 
  | not (isLocallyDefined id) ||
    isEmptyVarSet (idFreeTyVars id)
  = find_globals tv tidy_env acc ids

  | otherwise
  = zonkTcType (idType id)	`thenNF_Tc` \ id_ty ->
    if tv `elemVarSet` tyVarsOfType id_ty then
    	let 
    	   (tidy_env', id_ty') = tidyOpenType tidy_env id_ty
	   acc'		       = (idName id, id_ty') : acc
    	in
    	find_globals tv tidy_env' acc' ids
    else
    	find_globals tv tidy_env  acc  ids

find_frees tv tidy_env acc []
  = returnNF_Tc (tidy_env, acc)
find_frees tv tidy_env acc (ftv:ftvs)
  = zonkTcTyVar ftv	`thenNF_Tc` \ ty ->
    if tv `elemVarSet` tyVarsOfType ty then
	let
	    (tidy_env', ftv') = tidyTyVar tidy_env ftv
	in
	find_frees tv tidy_env' (ftv':acc) ftvs
    else
	find_frees tv tidy_env  acc        ftvs


escape_msg sig_tv tv globs frees
  = mk_msg sig_tv <+> ptext SLIT("escapes") $$
    if not (null globs) then
	vcat [pp_it <+> ptext SLIT("is mentioned in the environment"),
	      ptext SLIT("The following variables in the environment mention") <+> quotes (ppr tv),
	      nest 2 (vcat_first 10 [ppr name <+> dcolon <+> ppr ty | (name,ty) <- globs])
	]
     else if not (null frees) then
	vcat [ptext SLIT("It is reachable from the type variable(s)") <+> pprQuotedList frees,
	      nest 2 (ptext SLIT("which") <+> is_are <+> ptext SLIT("free in the signature"))
	]
     else
	empty	-- Sigh.  It's really hard to give a good error message
		-- all the time.   One bad case is an existential pattern match
  where
    is_are | isSingleton frees = ptext SLIT("is")
	   | otherwise         = ptext SLIT("are")
    pp_it | sig_tv /= tv = ptext SLIT("It unifies with") <+> quotes (ppr tv) <> comma <+> ptext SLIT("which")
	  | otherwise    = ptext SLIT("It")

    vcat_first :: Int -> [SDoc] -> SDoc
    vcat_first n []     = empty
    vcat_first 0 (x:xs) = text "...others omitted..."
    vcat_first n (x:xs) = x $$ vcat_first (n-1) xs

unify_msg tv thing = mk_msg tv <+> ptext SLIT("is unified with") <+> quotes thing
mk_msg tv          = ptext SLIT("Quantified type variable") <+> quotes (ppr tv)
\end{code}

These two context are used with checkSigTyVars
    
\begin{code}
sigCtxt :: Message -> [TcTyVar] -> TcThetaType -> TcTauType
	-> TidyEnv -> NF_TcM s (TidyEnv, Message)
sigCtxt when sig_tyvars sig_theta sig_tau tidy_env
  = zonkTcType sig_tau		`thenNF_Tc` \ actual_tau ->
    let
	(env1, tidy_sig_tyvars)  = tidyTyVars tidy_env sig_tyvars
	(env2, tidy_sig_rho)	 = tidyOpenType env1 (mkRhoTy sig_theta sig_tau)
	(env3, tidy_actual_tau)  = tidyOpenType env2 actual_tau
	msg = vcat [ptext SLIT("Signature type:    ") <+> pprType (mkForAllTys tidy_sig_tyvars tidy_sig_rho),
		    ptext SLIT("Type to generalise:") <+> pprType tidy_actual_tau,
		    when
		   ]
    in
    returnNF_Tc (env3, msg)

sigPatCtxt bound_tvs bound_ids tidy_env
  = returnNF_Tc (env1,
		 sep [ptext SLIT("When checking a pattern that binds"),
		      nest 4 (vcat (zipWith ppr_id show_ids tidy_tys))])
  where
    show_ids = filter is_interesting bound_ids
    is_interesting id = any (`elemVarSet` idFreeTyVars id) bound_tvs

    (env1, tidy_tys) = tidyOpenTypes tidy_env (map idType show_ids)
    ppr_id id ty     = ppr id <+> dcolon <+> ppr ty
	-- Don't zonk the types so we get the separate, un-unified versions
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
tcsigCtxt v   = ptext SLIT("In a type signature for") <+> quotes (ppr v)

typeKindCtxt :: RenamedHsType -> Message
typeKindCtxt ty = sep [ptext SLIT("When checking that"),
	  	       nest 2 (quotes (ppr ty)),
		       ptext SLIT("is a type")]

appKindCtxt :: SDoc -> Message
appKindCtxt pp = ptext SLIT("When checking kinds in") <+> quotes pp

wrongThingErr expected actual name
  = pp_actual actual <+> quotes (ppr name) <+> ptext SLIT("used as a") <+> text expected
  where
    pp_actual (ATyCon _) = ptext SLIT("Type constructor")
    pp_actual (AClass _) = ptext SLIT("Class")
    pp_actual (ATyVar _) = ptext SLIT("Type variable")
    pp_actual (AThing _) = ptext SLIT("Utterly bogus")

ambigErr pred ty
  = sep [ptext SLIT("Ambiguous constraint") <+> quotes (pprPred pred),
	 nest 4 (ptext SLIT("for the type:") <+> ppr ty),
	 nest 4 (ptext SLIT("Each forall'd type variable mentioned by the constraint must appear after the =>"))]

freeErr pred ty
  = sep [ptext SLIT("The constraint") <+> quotes (pprPred pred) <+>
		   ptext SLIT("does not mention any of the universally quantified type variables"),
	 nest 4 (ptext SLIT("in the type") <+> quotes (ppr ty))
    ]

unboxedTupleErr ty
  = sep [ptext (SLIT("Illegal unboxed tuple as a function or contructor argument:")), nest 4 (ppr ty)]

argTyErr ty = ptext SLIT("Illegal polymorphic type as argument:") <+> ppr ty
\end{code}

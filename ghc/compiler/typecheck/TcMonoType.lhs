%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
module TcMonoType ( tcHsType, tcHsSigType, tcHsTypeKind, tcHsTopType, tcHsTopBoxedType, tcHsTopTypeKind,
		    tcContext, tcHsTyVar, kcHsTyVar, kcHsType,
		    tcExtendTyVarScope, tcExtendTopTyVarScope,
		    TcSigInfo(..), tcTySig, mkTcSig, maybeSig,
		    checkSigTyVars, sigCtxt, sigPatCtxt
	          ) where

#include "HsVersions.h"

import HsSyn		( HsType(..), HsTyVarBndr(..), HsUsageAnn(..),
                          Sig(..), HsPred(..), pprParendHsType, HsTupCon(..) )
import RnHsSyn		( RenamedHsType, RenamedContext, RenamedSig )
import TcHsSyn		( TcId )

import TcMonad
import TcEnv		( tcExtendTyVarEnv, tcLookupTy, tcGetValueEnv, tcGetInScopeTyVars,
                          tcExtendUVarEnv, tcLookupUVar,
			  tcGetGlobalTyVars, valueEnvIds, TcTyThing(..)
			)
import TcType		( TcType, TcKind, TcTyVar, TcThetaType, TcTauType,
			  typeToTcType, kindToTcKind,
			  newKindVar, tcInstSigVar,
			  zonkTcKindToKind, zonkTcTypeToType, zonkTcTyVars, zonkTcType, zonkTcTyVar
			)
import Inst		( Inst, InstOrigin(..), newMethodWithGivenTy, instToIdBndr )
import TcUnify		( unifyKind, unifyKinds, unifyTypeKind )
import Type		( Type, PredType(..), ThetaType, UsageAnn(..),
			  mkTyVarTy, mkTyVarTys, mkFunTy, mkSynTy, mkUsgTy,
                          mkUsForAllTy, zipFunTys, hoistForAllTys,
			  mkSigmaTy, mkDictTy, mkPredTy, mkTyConApp,
			  mkAppTys, splitForAllTys, splitRhoTy, mkRhoTy,
			  boxedTypeKind, unboxedTypeKind, tyVarsOfType,
			  mkArrowKinds, getTyVar_maybe, getTyVar,
		  	  tidyOpenType, tidyOpenTypes, tidyTyVar, tidyTyVars,
			  tyVarsOfType, tyVarsOfTypes, mkForAllTys
			)
import PprType		( pprConstraint, pprType )
import Subst		( mkTopTyVarSubst, substTy )
import Id		( mkVanillaId, idName, idType, idFreeTyVars )
import Var		( TyVar, mkTyVar, mkNamedUVar, varName )
import VarEnv
import VarSet
import Bag		( bagToList )
import ErrUtils		( Message )
import TyCon		( TyCon )
import Name		( Name, OccName, isLocallyDefined )
import TysWiredIn	( mkListTy, mkTupleTy )
import UniqFM		( elemUFM, foldUFM )
import BasicTypes	( Boxity(..) )
import SrcLoc		( SrcLoc )
import Unique		( Unique, Uniquable(..) )
import Util		( mapAccumL, isSingleton, removeDups )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{Checking types}
%*									*
%************************************************************************

tcHsType and tcHsTypeKind
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tcHsType checks that the type really is of kind Type!

\begin{code}
kcHsType :: RenamedHsType -> TcM c ()
  -- Kind-check the type
kcHsType ty = tc_type ty	`thenTc_`
	      returnTc ()

tcHsSigType :: RenamedHsType -> TcM s TcType
  -- Used for type sigs written by the programmer
  -- Hoist any inner for-alls to the top
tcHsSigType ty
  = tcHsType ty		`thenTc` \ ty' ->
    returnTc (hoistForAllTys ty')

tcHsType :: RenamedHsType -> TcM s TcType
tcHsType ty
  = -- tcAddErrCtxt (typeCtxt ty)		$
    tc_type ty

tcHsTypeKind    :: RenamedHsType -> TcM s (TcKind, TcType)
tcHsTypeKind ty 
  = -- tcAddErrCtxt (typeCtxt ty)		$
    tc_type_kind ty

-- Type-check a type, *and* then lazily zonk it.  The important
-- point is that this zonks all the uncommitted *kind* variables
-- in kinds of any any nested for-all tyvars.
-- There won't be any mutable *type* variables at all.
--
-- NOTE the forkNF_Tc.  This makes the zonking lazy, which is
-- absolutely necessary.  During the type-checking of a recursive
-- group of tycons/classes (TcTyClsDecls.tcGroup) we use an
-- environment in which we aren't allowed to look at the actual
-- tycons/classes returned from a lookup. Because tc_app does
-- look at the tycon to build the type, we can't look at the type
-- either, until we get out of the loop.   The fork delays the
-- zonking till we've completed the loop.  Sigh.

tcHsTopType :: RenamedHsType -> TcM s Type
tcHsTopType ty
  = -- tcAddErrCtxt (typeCtxt ty)		$
    tc_type ty				`thenTc` \ ty' ->
    forkNF_Tc (zonkTcTypeToType ty')	`thenTc` \ ty'' ->
    returnTc (hoistForAllTys ty'')

tcHsTopBoxedType :: RenamedHsType -> TcM s Type
tcHsTopBoxedType ty
  = -- tcAddErrCtxt (typeCtxt ty)		$
    tc_boxed_type ty			`thenTc` \ ty' ->
    forkNF_Tc (zonkTcTypeToType ty')	`thenTc` \ ty'' ->
    returnTc (hoistForAllTys ty'')

tcHsTopTypeKind :: RenamedHsType -> TcM s (TcKind, Type)
tcHsTopTypeKind ty
  = -- tcAddErrCtxt (typeCtxt ty)		$
    tc_type_kind ty				`thenTc` \ (kind, ty') ->
    forkNF_Tc (zonkTcTypeToType ty')		`thenTc` \ zonked_ty ->
    returnNF_Tc (kind, hoistForAllTys zonked_ty)
\end{code}


The main work horse
~~~~~~~~~~~~~~~~~~~

\begin{code}
tc_boxed_type :: RenamedHsType -> TcM s Type
tc_boxed_type ty
  = tc_type_kind ty					`thenTc` \ (actual_kind, tc_ty) ->
    tcAddErrCtxt (typeKindCtxt ty)
		 (unifyKind boxedTypeKind actual_kind)	`thenTc_`
    returnTc tc_ty

tc_type :: RenamedHsType -> TcM s Type
tc_type ty
	-- The type ty must be a *type*, but it can be boxed
	-- or unboxed.  So we check that is is of form (Type bv)
	-- using unifyTypeKind
  = tc_type_kind ty				`thenTc` \ (actual_kind, tc_ty) ->
    tcAddErrCtxt (typeKindCtxt ty)
		 (unifyTypeKind actual_kind)	`thenTc_`
    returnTc tc_ty

tc_type_kind :: RenamedHsType -> TcM s (TcKind, Type)
tc_type_kind ty@(HsTyVar name)
  = tc_app ty []

tc_type_kind (HsListTy ty)
  = tc_boxed_type ty		`thenTc` \ tau_ty ->
    returnTc (boxedTypeKind, mkListTy tau_ty)

tc_type_kind (HsTupleTy (HsTupCon _ Boxed) tys)
  = mapTc tc_boxed_type tys	`thenTc` \ tau_tys ->
    returnTc (boxedTypeKind, mkTupleTy Boxed (length tys) tau_tys)

tc_type_kind (HsTupleTy (HsTupCon _ Unboxed) tys)
  = mapTc tc_type tys			`thenTc` \ tau_tys ->
    returnTc (unboxedTypeKind, mkTupleTy Unboxed (length tys) tau_tys)

tc_type_kind (HsFunTy ty1 ty2)
  = tc_type ty1	`thenTc` \ tau_ty1 ->
    tc_type ty2	`thenTc` \ tau_ty2 ->
    returnTc (boxedTypeKind, mkFunTy tau_ty1 tau_ty2)

tc_type_kind (HsAppTy ty1 ty2)
  = tc_app ty1 [ty2]

tc_type_kind (HsPredTy pred)
  = tcClassAssertion True pred	`thenTc` \ pred' ->
    returnTc (boxedTypeKind, mkPredTy pred')

tc_type_kind (HsUsgTy usg ty)
  = newUsg usg                          `thenTc` \ usg' ->
    tc_type_kind ty                     `thenTc` \ (kind, tc_ty) ->
    returnTc (kind, mkUsgTy usg' tc_ty)
  where
    newUsg usg = case usg of
                   HsUsOnce        -> returnTc UsOnce
                   HsUsMany        -> returnTc UsMany
                   HsUsVar uv_name -> tcLookupUVar uv_name `thenTc` \ uv ->
                                        returnTc (UsVar uv)

tc_type_kind (HsUsgForAllTy uv_name ty)
  = let
        uv = mkNamedUVar uv_name
    in
    tcExtendUVarEnv uv_name uv $
      tc_type_kind ty                     `thenTc` \ (kind, tc_ty) ->
      returnTc (kind, mkUsForAllTy uv tc_ty)

tc_type_kind (HsForAllTy (Just tv_names) context ty)
  = tcExtendTyVarScope tv_names		$ \ tyvars ->
    tcContext context			`thenTc` \ theta ->
    tc_type_kind ty			`thenTc` \ (kind, tau) ->
    tcGetInScopeTyVars			`thenTc` \ in_scope_vars ->
    let
	body_kind | null theta = kind
		  | otherwise  = boxedTypeKind
		-- Context behaves like a function type
		-- This matters.  Return-unboxed-tuple analysis can
		-- give overloaded functions like
		--	f :: forall a. Num a => (# a->a, a->a #)
		-- And we want these to get through the type checker
        check ct@(Class c tys) | ambiguous = failWithTc (ambigErr (c,tys) tau)
	  where ct_vars       = tyVarsOfTypes tys
		forall_tyvars = map varName in_scope_vars
		tau_vars      = tyVarsOfType tau
		ambig ct_var  = (varName ct_var `elem` forall_tyvars) &&
			        not (ct_var `elemUFM` tau_vars)
		ambiguous     = foldUFM ((||) . ambig) False ct_vars
	check _ = returnTc ()
    in
    mapTc check theta			`thenTc_`
    returnTc (body_kind, mkSigmaTy tyvars theta tau)
\end{code}

Help functions for type applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
tc_app (HsAppTy ty1 ty2) tys
  = tc_app ty1 (ty2:tys)

tc_app ty tys
  | null tys
  = tc_fun_type ty []

  | otherwise
  = tcAddErrCtxt (appKindCtxt pp_app)	$
    mapAndUnzipTc tc_type_kind tys	`thenTc` \ (arg_kinds, arg_tys) ->
    tc_fun_type ty arg_tys		`thenTc` \ (fun_kind, result_ty) ->

	-- Check argument compatibility
    newKindVar					`thenNF_Tc` \ result_kind ->
    unifyKind fun_kind (mkArrowKinds arg_kinds result_kind)
					`thenTc_`
    returnTc (result_kind, result_ty)
  where
    pp_app = ppr ty <+> sep (map pprParendHsType tys)

-- (tc_fun_type ty arg_tys) returns (kind-of ty, mkAppTys ty arg_tys)
-- But not quite; for synonyms it checks the correct arity, and builds a SynTy
-- 	hence the rather strange functionality.

tc_fun_type (HsTyVar name) arg_tys
  = tcLookupTy name			`thenTc` \ (tycon_kind, thing) ->
    case thing of
	ATyVar tv     -> returnTc (tycon_kind, mkAppTys (mkTyVarTy tv) arg_tys)
	AClass clas _ -> failWithTc (classAsTyConErr name)

	ADataTyCon tc ->  -- Data or newtype
			  returnTc (tycon_kind, mkTyConApp tc arg_tys)

	ASynTyCon tc arity ->  	-- Type synonym
			          checkTc (arity <= n_args) err_msg	`thenTc_`
	  		          returnTc (tycon_kind, result_ty)
			   where
				-- It's OK to have an *over-applied* type synonym
				--	data Tree a b = ...
				--	type Foo a = Tree [a]
				--	f :: Foo a b -> ...
			      result_ty = mkAppTys (mkSynTy tc (take arity arg_tys))
						   (drop arity arg_tys)
			      err_msg = arityErr "type synonym" name arity n_args
			      n_args  = length arg_tys

tc_fun_type ty arg_tys
  = tc_type_kind ty		`thenTc` \ (fun_kind, fun_ty) ->
    returnTc (fun_kind, mkAppTys fun_ty arg_tys)
\end{code}


Contexts
~~~~~~~~
\begin{code}

tcContext :: RenamedContext -> TcM s ThetaType
tcContext context = mapTc (tcClassAssertion False) context

tcClassAssertion ccall_ok assn@(HsPClass class_name tys)
  = tcAddErrCtxt (appKindCtxt (ppr assn))	$
    mapAndUnzipTc tc_type_kind tys		`thenTc` \ (arg_kinds, arg_tys) ->
    tcLookupTy class_name			`thenTc` \ (kind, thing) ->
    case thing of
	AClass clas arity ->
		    	-- Check with kind mis-match
		checkTc (arity == n_tys) err				`thenTc_`
		unifyKind kind (mkArrowKinds arg_kinds boxedTypeKind)	`thenTc_`
		returnTc (Class clas arg_tys)
	    where
		n_tys = length tys
		err   = arityErr "Class" class_name arity n_tys
	other -> failWithTc (tyVarAsClassErr class_name)

tcClassAssertion ccall_ok assn@(HsPIParam name ty)
  = tcAddErrCtxt (appKindCtxt (ppr assn))	$
    tc_type_kind ty	`thenTc` \ (arg_kind, arg_ty) ->
    returnTc (IParam name arg_ty)
\end{code}


%************************************************************************
%*									*
\subsection{Type variables, with knot tying!}
%*									*
%************************************************************************

\begin{code}
tcExtendTopTyVarScope :: TcKind -> [HsTyVarBndr Name]
		      -> ([TcTyVar] -> TcKind -> TcM s a)
		      -> TcM s a
tcExtendTopTyVarScope kind tyvar_names thing_inside
  = let
	(tyvars_w_kinds, result_kind) = zipFunTys tyvar_names kind
	tyvars 			      = map mk_tv tyvars_w_kinds
    in
    tcExtendTyVarEnv tyvars (thing_inside tyvars result_kind)	
  where
    mk_tv (UserTyVar name,    kind) = mkTyVar name kind
    mk_tv (IfaceTyVar name _, kind) = mkTyVar name kind
	-- NB: immutable tyvars, but perhaps with mutable kinds

tcExtendTyVarScope :: [HsTyVarBndr Name] 
		   -> ([TcTyVar] -> TcM s a) -> TcM s a
tcExtendTyVarScope tv_names thing_inside
  = mapNF_Tc tcHsTyVar tv_names 	`thenNF_Tc` \ tyvars ->
    tcExtendTyVarEnv tyvars		$
    thing_inside tyvars
    
tcHsTyVar :: HsTyVarBndr Name -> NF_TcM s TcTyVar
tcHsTyVar (UserTyVar name)       = newKindVar		`thenNF_Tc` \ kind ->
			           tcNewMutTyVar name kind
	-- NB: mutable kind => mutable tyvar, so that zonking can bind
	-- the tyvar to its immutable form

tcHsTyVar (IfaceTyVar name kind) = returnNF_Tc (mkTyVar name (kindToTcKind kind))

kcHsTyVar :: HsTyVarBndr name -> NF_TcM s TcKind
kcHsTyVar (UserTyVar name)       = newKindVar
kcHsTyVar (IfaceTyVar name kind) = returnNF_Tc (kindToTcKind kind)
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

	Inst			-- Empty if theta is null, or 
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
 = tcAddSrcLoc src_loc $
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
	
   returnNF_Tc (TySigInfo name poly_id tyvars' theta' tau' (instToIdBndr inst) inst src_loc)
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
	(env3, tidy_actual_tau)  = tidyOpenType env1 actual_tau
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
typeCtxt ty = ptext SLIT("In the type") <+> quotes (ppr ty)

typeKindCtxt :: RenamedHsType -> Message
typeKindCtxt ty = sep [ptext SLIT("When checking that"),
	  	       nest 2 (quotes (ppr ty)),
		       ptext SLIT("is a type")]

appKindCtxt :: SDoc -> Message
appKindCtxt pp = ptext SLIT("When checking kinds in") <+> quotes pp

classAsTyConErr name
  = ptext SLIT("Class used as a type constructor:") <+> ppr name

tyConAsClassErr name
  = ptext SLIT("Type constructor used as a class:") <+> ppr name

tyVarAsClassErr name
  = ptext SLIT("Type variable used as a class:") <+> ppr name

ambigErr (c, ts) ty
  = sep [ptext SLIT("Ambiguous constraint") <+> quotes (pprConstraint c ts),
	 nest 4 (ptext SLIT("for the type:") <+> ppr ty),
	 nest 4 (ptext SLIT("Each forall'd type variable mentioned by the constraint must appear after the =>"))]
\end{code}

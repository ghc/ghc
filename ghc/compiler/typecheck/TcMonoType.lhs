%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
module TcMonoType ( tcHsType, tcHsTypeKind, tcContext, tcTyVarScope ) where

#include "HsVersions.h"

import HsSyn		( HsType(..), HsTyVar(..), pprContext )
import RnHsSyn		( RenamedHsType(..), RenamedContext(..) )

import TcMonad
import TcEnv		( tcLookupTyVar, tcLookupClass, tcLookupTyCon, tcExtendTyVarEnv	)
import TcKind		( TcKind, mkBoxedTypeKind, mkTypeKind, mkArrowKind,
			  unifyKind, unifyKinds, newKindVar,
			  kindToTcKind, tcDefaultKind
			)
import Type		( Type, ThetaType, 
			  mkTyVarTy, mkFunTy, mkSynTy,
			  mkSigmaTy, mkDictTy, mkTyConApp, mkAppTys
			)
import TyVar		( TyVar, mkTyVar )
import PrelInfo		( cCallishClassKeys )
import TyCon		( TyCon )
import Name		( Name, OccName, isTvOcc, getOccName )
import TysWiredIn	( mkListTy, mkTupleTy )
import Unique		( Unique, Uniquable(..) )
import Util		( zipWithEqual, zipLazy )
import Outputable
\end{code}


tcHsType and tcHsTypeKind
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tcHsType checks that the type really is of kind Type!

\begin{code}
tcHsType :: RenamedHsType -> TcM s Type

tcHsType ty
  = tcAddErrCtxt (typeCtxt ty)		$
    tc_hs_type ty

tc_hs_type ty
  = tc_hs_type_kind ty			`thenTc` \ (kind,ty) ->
	-- Check that it really is a type
    unifyKind mkTypeKind kind		`thenTc_`
    returnTc ty
\end{code}

tcHsTypeKind does the real work.  It returns a kind and a type.

\begin{code}
tcHsTypeKind :: RenamedHsType -> TcM s (TcKind s, Type)

tcHsTypeKind ty
  = tcAddErrCtxt (typeCtxt ty)		$
    tc_hs_type_kind ty


	-- This equation isn't needed (the next one would handle it fine)
	-- but it's rather a common case, so we handle it directly
tc_hs_type_kind (MonoTyVar name)
  | isTvOcc (getOccName name)
  = tcLookupTyVar name			`thenNF_Tc` \ (kind,tyvar) ->
    returnTc (kind, mkTyVarTy tyvar)

tc_hs_type_kind ty@(MonoTyVar name)
  = tcFunType ty []
    
tc_hs_type_kind (MonoListTy _ ty)
  = tc_hs_type ty	`thenTc` \ tau_ty ->
    returnTc (mkBoxedTypeKind, mkListTy tau_ty)

tc_hs_type_kind (MonoTupleTy _ tys)
  = mapTc tc_hs_type  tys	`thenTc` \ tau_tys ->
    returnTc (mkBoxedTypeKind, mkTupleTy (length tys) tau_tys)

tc_hs_type_kind (MonoFunTy ty1 ty2)
  = tc_hs_type ty1	`thenTc` \ tau_ty1 ->
    tc_hs_type ty2	`thenTc` \ tau_ty2 ->
    returnTc (mkBoxedTypeKind, mkFunTy tau_ty1 tau_ty2)

tc_hs_type_kind (MonoTyApp ty1 ty2)
  = tcTyApp ty1 [ty2]

tc_hs_type_kind (HsForAllTy tv_names context ty)
  = tcTyVarScope tv_names		 	$ \ tyvars ->
	tcContext context			`thenTc` \ theta ->
	tc_hs_type ty				`thenTc` \ tau ->
		-- For-all's are of kind type!
	returnTc (mkBoxedTypeKind, mkSigmaTy tyvars theta tau)

-- for unfoldings, and instance decls, only:
tc_hs_type_kind (MonoDictTy class_name tys)
  = mapAndUnzipTc tc_hs_type_kind tys	`thenTc` \ (arg_kinds, arg_tys) ->
    tcLookupClass class_name		`thenTc` \ (class_kinds, clas) ->
    let
	arity  = length class_kinds
	n_args = length arg_kinds
	err = arityErr "Class" class_name arity n_args
    in
    checkTc (arity == n_args) err	`thenTc_`
    unifyKinds class_kinds arg_kinds	`thenTc_`
    returnTc (mkBoxedTypeKind, mkDictTy clas arg_tys)
\end{code}

Help functions for type applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcTyApp (MonoTyApp ty1 ty2) tys
  = tcTyApp ty1 (ty2:tys)

tcTyApp ty tys
  | null tys
  = tcFunType ty []

  | otherwise
  = mapAndUnzipTc tc_hs_type_kind tys	`thenTc` \ (arg_kinds, arg_tys) ->
    tcFunType ty arg_tys		`thenTc` \ (fun_kind, result_ty) ->

	-- Check argument compatibility
    newKindVar				`thenNF_Tc` \ result_kind ->
    unifyKind fun_kind (foldr mkArrowKind result_kind arg_kinds)
					`thenTc_`
    returnTc (result_kind, result_ty)

-- (tcFunType ty arg_tys) returns (kind-of ty, mkAppTys ty arg_tys)
-- But not quite; for synonyms it checks the correct arity, and builds a SynTy
-- 	hence the rather strange functionality.

tcFunType (MonoTyVar name) arg_tys
  | isTvOcc (getOccName name)	-- Must be a type variable
  = tcLookupTyVar name			`thenNF_Tc` \ (kind,tyvar) ->
    returnTc (kind, mkAppTys (mkTyVarTy tyvar) arg_tys)

  | otherwise		 	-- Must be a type constructor
  = tcLookupTyCon name			`thenTc` \ (tycon_kind,maybe_arity, tycon) ->
    case maybe_arity of
	Nothing    -> 	-- Data type or newtype 
		      returnTc (tycon_kind, mkTyConApp tycon arg_tys)

	Just arity -> 	-- Type synonym
		      checkTc (arity <= n_args) err_msg	`thenTc_`
		      returnTc (tycon_kind, result_ty)
		   where
			-- It's OK to have an *over-applied* type synonym
			--	data Tree a b = ...
			--	type Foo a = Tree [a]
			--	f :: Foo a b -> ...
		      result_ty = mkAppTys (mkSynTy tycon (take arity arg_tys))
					   (drop arity arg_tys)
		      err_msg = arityErr "Type synonym constructor" name arity n_args
		      n_args  = length arg_tys

tcFunType ty arg_tys
  = tc_hs_type_kind ty		`thenTc` \ (fun_kind, fun_ty) ->
    returnTc (fun_kind, mkAppTys fun_ty arg_tys)
\end{code}


Contexts
~~~~~~~~
\begin{code}

tcContext :: RenamedContext -> TcM s ThetaType
tcContext context = tcAddErrCtxt (thetaCtxt context) $
		    mapTc tcClassAssertion context

tcClassAssertion (class_name, tys)
  = checkTc (canBeUsedInContext class_name)
	    (naughtyCCallContextErr class_name)	`thenTc_`

    tcLookupClass class_name		`thenTc` \ (class_kinds, clas) ->
    mapAndUnzipTc tc_hs_type_kind tys	`thenTc` \ (ty_kinds, tc_tys) ->

    unifyKinds class_kinds ty_kinds	`thenTc_`

    returnTc (clas, tc_tys)
\end{code}

HACK warning: Someone discovered that @CCallable@ and @CReturnable@
could be used in contexts such as:
\begin{verbatim}
foo :: CCallable a => a -> PrimIO Int
\end{verbatim}

Doing this utterly wrecks the whole point of introducing these
classes so we specifically check that this isn't being done.

\begin{code}
canBeUsedInContext :: Name -> Bool
canBeUsedInContext n = not (uniqueOf n `elem` cCallishClassKeys)
\end{code}

Type variables, with knot tying!
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcTyVarScope
	:: [HsTyVar Name]		-- Names of some type variables
	-> ([TyVar] -> TcM s a)		-- Thing to type check in their scope
	-> TcM s a			-- Result

tcTyVarScope tyvar_names thing_inside
  = mapAndUnzipNF_Tc tcHsTyVar tyvar_names	`thenNF_Tc` \ (names, kinds) ->

    fixTc (\ ~(rec_tyvars, _) ->
		-- Ok to look at names, kinds, but not tyvars!

	tcExtendTyVarEnv names (kinds `zipLazy` rec_tyvars)
			 (thing_inside rec_tyvars)		`thenTc` \ result ->
 
		-- Get the tyvar's Kinds from their TcKinds
	mapNF_Tc tcDefaultKind kinds				`thenNF_Tc` \ kinds' ->

		-- Construct the real TyVars
	let
	  tyvars = zipWithEqual "tcTyVarScope" mkTyVar names kinds'
	in
	returnTc (tyvars, result)
    )					`thenTc` \ (_,result) ->
    returnTc result

tcHsTyVar (UserTyVar name)
  = newKindVar		`thenNF_Tc` \ tc_kind ->
    returnNF_Tc (name, tc_kind)
tcHsTyVar (IfaceTyVar name kind)
  = returnNF_Tc (name, kindToTcKind kind)
\end{code}

Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
naughtyCCallContextErr clas_name
  = sep [ptext SLIT("Can't use class"), quotes (ppr clas_name), ptext SLIT("in a context")]

typeCtxt ty = ptext SLIT("In the type") <+> quotes (ppr ty)

thetaCtxt theta = ptext SLIT("In the context") <+> quotes (pprContext theta)
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
#include "HsVersions.h"

module TcMonoType ( tcPolyType, tcMonoType, tcMonoTypeKind, tcContext ) where

import Ubiq{-uitous-}

import HsSyn		( PolyType(..), MonoType(..), Fake )
import RnHsSyn		( RenamedPolyType(..), RenamedMonoType(..), 
			  RenamedContext(..)
			)


import TcMonad
import TcEnv		( tcLookupTyVar, tcLookupClass, tcLookupTyCon, 
			  tcExtendTyVarEnv, tcTyVarScope
			)
import TcKind		( TcKind, mkTcTypeKind, mkBoxedTypeKind,
			  mkTcArrowKind, unifyKind, newKindVar,
			  kindToTcKind
			)
import ErrUtils		( arityErr )
import Type		( GenType, Type(..), ThetaType(..), 
			  mkTyVarTy, mkTyConTy, mkFunTy, mkAppTy,
			  mkSigmaTy
			)
import TyVar		( GenTyVar, TyVar(..), mkTyVar )
import PrelInfo		( mkListTy, mkTupleTy )
import Type		( mkDictTy )
import Class		( cCallishClassKeys )
import Unique		( Unique )
import Name		( Name(..), getNameShortName, isTyConName, getSynNameArity )
import PprStyle
import Pretty
import Util		( zipWithEqual, panic )
\end{code}


tcMonoType and tcMonoTypeKind
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tcMonoType checks that the type really is of kind Type!

\begin{code}
tcMonoType :: RenamedMonoType -> TcM s Type

tcMonoType ty
  = tcMonoTypeKind ty			`thenTc` \ (kind,ty) ->
    unifyKind kind mkTcTypeKind		`thenTc_`
    returnTc ty
\end{code}

tcMonoTypeKind does the real work.  It returns a kind and a type.

\begin{code}
tcMonoTypeKind :: RenamedMonoType -> TcM s (TcKind s, Type)

tcMonoTypeKind (MonoTyVar name)
  = tcLookupTyVar name	`thenNF_Tc` \ (kind,tyvar) ->
    returnTc (kind, mkTyVarTy tyvar)
    

tcMonoTypeKind (MonoListTy ty)
  = tcMonoType ty	`thenTc` \ tau_ty ->
    returnTc (mkTcTypeKind, mkListTy tau_ty)

tcMonoTypeKind (MonoTupleTy tys)
  = mapTc tcMonoType  tys	`thenTc` \ tau_tys ->
    returnTc (mkTcTypeKind, mkTupleTy (length tys) tau_tys)

tcMonoTypeKind (MonoFunTy ty1 ty2)
  = tcMonoType ty1	`thenTc` \ tau_ty1 ->
    tcMonoType ty2	`thenTc` \ tau_ty2 ->
    returnTc (mkTcTypeKind, mkFunTy tau_ty1 tau_ty2)

tcMonoTypeKind (MonoTyApp name tys)
  = mapAndUnzipTc tcMonoTypeKind tys	`thenTc`    \ (arg_kinds, arg_tys) ->

    tc_mono_name name			`thenNF_Tc` \ (fun_kind, fun_ty) ->

    newKindVar				`thenNF_Tc` \ result_kind ->
    unifyKind fun_kind (foldr mkTcArrowKind result_kind arg_kinds)	`thenTc_`

	-- Check for saturated application in the special case of
	-- type synoyms.  Here the renamer has kindly attached the
	-- arity to the Name.
    synArityCheck name (length tys)	`thenTc_`

    returnTc (result_kind, foldl mkAppTy fun_ty arg_tys)

-- for unfoldings only:
tcMonoTypeKind (MonoForAllTy tyvars_w_kinds ty)
  = tcExtendTyVarEnv tyvar_names (tc_kinds `zip` tyvars) (
	tcMonoTypeKind ty		`thenTc` \ (kind, ty') ->
	unifyKind kind mkTcTypeKind	`thenTc_`
	returnTc (mkTcTypeKind, ty')
    )
  where
    (tyvar_names, kinds) = unzip tyvars_w_kinds
    tyvars   = zipWithEqual mk_tyvar tyvar_names kinds
    tc_kinds = map kindToTcKind kinds
    mk_tyvar name kind = mkTyVar (getNameShortName name) (getItsUnique name) kind

-- for unfoldings only:
tcMonoTypeKind (MonoDictTy class_name ty)
  = tcMonoTypeKind ty			`thenTc` \ (arg_kind, arg_ty) ->
    tcLookupClass class_name		`thenNF_Tc` \ (class_kind, clas) ->
    unifyKind class_kind arg_kind	`thenTc_`
    returnTc (mkTcTypeKind, mkDictTy clas arg_ty)


tc_mono_name :: Name -> NF_TcM s (TcKind s, Type)
tc_mono_name name@(Short _ _) 		-- Must be a type variable
  = tcLookupTyVar name			`thenNF_Tc` \ (kind,tyvar) ->
    returnNF_Tc (kind, mkTyVarTy tyvar)

tc_mono_name name | isTyConName name 	-- Must be a type constructor
  = tcLookupTyCon name			`thenNF_Tc` \ (kind,tycon) ->
    returnNF_Tc (kind, mkTyConTy tycon)
	
tc_mono_name name 			-- Renamer should have got it right
  = panic ("tc_mono_name:" ++ ppShow 1000 (ppr PprDebug name))
\end{code}


Contexts
~~~~~~~~
\begin{code}

tcContext :: RenamedContext -> TcM s ThetaType
tcContext context = mapTc tcClassAssertion context

tcClassAssertion (class_name, tyvar_name)
  = checkTc (canBeUsedInContext class_name)
	    (naughtyCCallContextErr class_name)	`thenTc_`

    tcLookupClass class_name		`thenNF_Tc` \ (class_kind, clas) ->
    tcLookupTyVar tyvar_name		`thenNF_Tc` \ (tyvar_kind, tyvar) ->

    unifyKind class_kind tyvar_kind	`thenTc_`

    returnTc (clas, mkTyVarTy tyvar)
\end{code}

HACK warning: Someone discovered that @_CCallable@ and @_CReturnable@
could be used in contexts such as:
\begin{verbatim}
foo :: _CCallable a => a -> PrimIO Int
\end{verbatim}

Doing this utterly wrecks the whole point of introducing these
classes so we specifically check that this isn't being done.

\begin{code}
canBeUsedInContext :: Name -> Bool
canBeUsedInContext (ClassName uniq _ _) = not (uniq `elem` cCallishClassKeys)
canBeUsedInContext other 	        = True
\end{code}


Polytypes
~~~~~~~~~
\begin{code}
tcPolyType :: RenamedPolyType -> TcM s Type
tcPolyType (HsForAllTy tyvar_names context ty)
  = tcTyVarScope tyvar_names (\ tyvars ->
	tcContext context	`thenTc` \ theta ->
	tcMonoType ty		`thenTc` \ tau ->
	returnTc (mkSigmaTy tyvars theta tau)
    )
\end{code}

Auxilliary functions
~~~~~~~~~~~~~~~~~~~~
\begin{code}
synArityCheck :: Name -> Int -> TcM s ()
synArityCheck name n_args
  = case getSynNameArity name of
	Just arity | arity /= n_args -> failTc (err arity)
	other			     -> returnTc ()
  where
    err arity = arityErr "Type synonym constructor" name arity n_args
\end{code}

Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
naughtyCCallContextErr clas_name sty
  = ppSep [ppStr "Can't use class", ppr sty clas_name, ppStr "in a context"]
\end{code}

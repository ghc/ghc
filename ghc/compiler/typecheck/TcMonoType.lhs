%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
module TcMonoType ( tcHsType, tcHsTcType, tcHsTypeKind, tcContext, 
		    tcTyVarScope,
		    TcSigInfo(..), tcTySig, mkTcSig, noSigs, maybeSig,
		    checkSigTyVars, sigCtxt, existentialPatCtxt
	          ) where

#include "HsVersions.h"

import HsSyn		( HsType(..), HsTyVar(..), Sig(..), pprContext )
import RnHsSyn		( RenamedHsType, RenamedContext, RenamedSig )
import TcHsSyn		( TcIdBndr, TcIdOcc(..) )

import TcMonad
import TcEnv		( tcLookupTyVar, tcLookupClass, tcLookupTyCon, tcExtendTyVarEnv,
			  tcGetGlobalTyVars, tidyTypes, tidyTyVar
			)
import TcType		( TcType, TcKind, TcTyVar, TcThetaType, TcTauType,
			  typeToTcType, tcInstTcType, kindToTcKind,
			  newKindVar, 
			  zonkTcKindToKind, zonkTcTyVars, zonkTcType
			)
import Inst		( Inst, InstOrigin(..), newMethodWithGivenTy, instToIdBndr )
import TcUnify		( unifyKind, unifyKinds	)
import Type		( Type, ThetaType, 
			  mkTyVarTy, mkTyVarTys, mkFunTy, mkSynTy,
			  mkSigmaTy, mkDictTy, mkTyConApp, mkAppTys, splitRhoTy,
			  boxedTypeKind, unboxedTypeKind, openTypeKind, 
			  mkArrowKind, getTyVar_maybe, getTyVar
			)
import Id		( mkUserId, idName, idType, idFreeTyVars )
import Var		( TyVar, mkTyVar )
import VarEnv
import VarSet
import Bag		( bagToList )
import PrelInfo		( cCallishClassKeys )
import TyCon		( TyCon )
import Name		( Name, OccName, isTvOcc, getOccName )
import TysWiredIn	( mkListTy, mkTupleTy, mkUnboxedTupleTy )
import SrcLoc		( SrcLoc )
import Unique		( Unique, Uniquable(..) )
import Util		( zipWithEqual, zipLazy, mapAccumL )
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
tcHsType :: RenamedHsType -> TcM s Type
tcHsType ty
  = tcAddErrCtxt (typeCtxt ty)		$
    tc_hs_type ty

-- Version for when we need a TcType returned
tcHsTcType :: RenamedHsType -> TcM s (TcType s)	
tcHsTcType ty
  = tcHsType ty		`thenTc` \ ty' ->
    returnTc (typeToTcType ty')

tc_hs_type ty
  = tc_hs_type_kind ty			`thenTc` \ (kind,ty) ->
	-- Check that it really is a type
    unifyKind openTypeKind kind		`thenTc_`
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
    
tc_hs_type_kind (MonoListTy ty)
  = tc_hs_type ty	`thenTc` \ tau_ty ->
    returnTc (boxedTypeKind, mkListTy tau_ty)

tc_hs_type_kind (MonoTupleTy tys True{-boxed-})
  = mapTc tc_hs_type  tys	`thenTc` \ tau_tys ->
    returnTc (boxedTypeKind, mkTupleTy (length tys) tau_tys)

tc_hs_type_kind (MonoTupleTy tys False{-unboxed-})
  = mapTc tc_hs_type  tys	`thenTc` \ tau_tys ->
    returnTc (unboxedTypeKind, mkUnboxedTupleTy (length tys) tau_tys)

tc_hs_type_kind (MonoFunTy ty1 ty2)
  = tc_hs_type ty1	`thenTc` \ tau_ty1 ->
    tc_hs_type ty2	`thenTc` \ tau_ty2 ->
    returnTc (boxedTypeKind, mkFunTy tau_ty1 tau_ty2)

tc_hs_type_kind (MonoTyApp ty1 ty2)
  = tcTyApp ty1 [ty2]

tc_hs_type_kind (HsForAllTy tv_names context ty)
  = tcTyVarScope tv_names		 	$ \ tyvars ->
	tcContext context			`thenTc` \ theta ->
	tc_hs_type ty				`thenTc` \ tau ->
		-- For-all's are of kind type!
	returnTc (boxedTypeKind, mkSigmaTy tyvars theta tau)

-- for unfoldings, and instance decls, only:
tc_hs_type_kind (MonoDictTy class_name tys)
  = tcClassAssertion (class_name, tys)	`thenTc` \ (clas, arg_tys) ->
    returnTc (boxedTypeKind, mkDictTy clas arg_tys)
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
tcContext context
  = tcAddErrCtxt (thetaCtxt context) $

  	--Someone discovered that @CCallable@ and @CReturnable@
	-- could be used in contexts such as:
	--	foo :: CCallable a => a -> PrimIO Int
	-- Doing this utterly wrecks the whole point of introducing these
	-- classes so we specifically check that this isn't being done.
	--
	-- We *don't* do this check in tcClassAssertion, because that's
	-- called when checking a HsDictTy, and we don't want to reject
	--	instance CCallable Int 
	-- etc. Ugh!
    mapTc check_naughty context `thenTc_`

    mapTc tcClassAssertion context

 where
   check_naughty (class_name, _) 
     = checkTc (not (getUnique class_name `elem` cCallishClassKeys))
	       (naughtyCCallContextErr class_name)

tcClassAssertion (class_name, tys)
  = tcLookupClass class_name		`thenTc` \ (class_kinds, clas) ->
    mapAndUnzipTc tc_hs_type_kind tys	`thenTc` \ (ty_kinds, tc_tys) ->

	-- Check with kind mis-match
    let
	arity = length class_kinds
	n_tys = length ty_kinds
	err   = arityErr "Class" class_name arity n_tys
    in
    checkTc (arity == n_tys) err	`thenTc_`
    unifyKinds class_kinds ty_kinds	`thenTc_`

    returnTc (clas, tc_tys)
\end{code}


%************************************************************************
%*									*
\subsection{Type variables, with knot tying!}
%*									*
%************************************************************************

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
	mapNF_Tc zonkTcKindToKind kinds				`thenNF_Tc` \ kinds' ->

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
data TcSigInfo s
  = TySigInfo	    
	Name			-- N, the Name in corresponding binding

	(TcIdBndr s)		-- *Polymorphic* binder for this value...
				-- Has name = N

	[TcTyVar s]		-- tyvars
	(TcThetaType s)		-- theta
	(TcTauType s)		-- tau

	(TcIdBndr s)		-- *Monomorphic* binder for this value
				-- Does *not* have name = N
				-- Has type tau

	(Inst s)		-- Empty if theta is null, or 
				-- (method mono_id) otherwise

	SrcLoc			-- Of the signature


maybeSig :: [TcSigInfo s] -> Name -> Maybe (TcSigInfo s)
	-- Search for a particular signature
maybeSig [] name = Nothing
maybeSig (sig@(TySigInfo sig_name _ _ _ _ _ _ _) : sigs) name
  | name == sig_name = Just sig
  | otherwise	     = maybeSig sigs name

-- This little helper is useful to pass to tcPat
noSigs :: Name -> Maybe (TcIdBndr s)
noSigs name = Nothing
\end{code}


\begin{code}
tcTySig :: RenamedSig
	-> TcM s (TcSigInfo s)

tcTySig (Sig v ty src_loc)
 = tcAddSrcLoc src_loc $
   tcHsTcType ty				`thenTc` \ sigma_tc_ty ->
   mkTcSig (mkUserId v sigma_tc_ty) src_loc	`thenNF_Tc` \ sig -> 
   returnTc sig

mkTcSig :: TcIdBndr s -> SrcLoc -> NF_TcM s (TcSigInfo s)
mkTcSig poly_id src_loc
  = 	-- Instantiate this type
	-- It's important to do this even though in the error-free case
	-- we could just split the sigma_tc_ty (since the tyvars don't
	-- unified with anything).  But in the case of an error, when
	-- the tyvars *do* get unified with something, we want to carry on
	-- typechecking the rest of the program with the function bound
	-- to a pristine type, namely sigma_tc_ty
   tcInstTcType (idType poly_id)		`thenNF_Tc` \ (tyvars, rho) ->
   let
     (theta, tau) = splitRhoTy rho
	-- This splitSigmaTy tries hard to make sure that tau' is a type synonym
	-- wherever possible, which can improve interface files.
   in
   newMethodWithGivenTy SignatureOrigin 
		(TcId poly_id)
		(mkTyVarTys tyvars) 
		theta tau			`thenNF_Tc` \ inst ->
	-- We make a Method even if it's not overloaded; no harm
	
   returnNF_Tc (TySigInfo name poly_id tyvars theta tau (instToIdBndr inst) inst src_loc)
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
	(a) still all type variables
		eg matching signature [a] against inferred type [(p,q)]
		[then a will be unified to a non-type variable]

	(b) still all distinct
		eg matching signature [(a,b)] against inferred type [(p,p)]
		[then a and b will be unified together]

	(c) not mentioned in the environment
		eg the signature for f in this:

			g x = ... where
					f :: a->[a]
					f y = [x,y]

		Here, f is forced to be monorphic by the free occurence of x.

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
checkSigTyVars :: [TcTyVar s]		-- The original signature type variables
	       -> TcM s [TcTyVar s]	-- Zonked signature type variables

checkSigTyVars [] = returnTc []

checkSigTyVars sig_tyvars
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
      = failWithTcM (env2, main_msg)
      where
	(env1, tidy_tys) = tidyTypes emptyTidyEnv sig_tys
	(env2, tidy_tvs) = mapAccumL tidyTyVar env1 sig_tyvars

	msgs = check (tidy_tvs `zip` tidy_tys) emptyVarEnv

	main_msg = ptext SLIT("Inferred type is less polymorphic than expected")
		   $$
		   nest 4 (vcat msgs)

	check [] acc = []
	check ((sig_tyvar,ty):prs) acc
	  = case getTyVar_maybe ty of
	      Nothing				-- Error (a)!
		-> unify_msg sig_tyvar (ppr ty) : check prs acc

	      Just tv
		| tv `elemVarSet` globals	-- Error (c)! Type variable escapes
		-> escape_msg tv : check prs acc

		| otherwise
		-> case lookupVarEnv acc tv of
			Nothing  		-- All OK
				-> check prs (extendVarEnv acc tv sig_tyvar)	-- All OK
			Just sig_tyvar' 	-- Error (b)!
				-> unify_msg sig_tyvar (ppr sig_tyvar') : check prs acc


escape_msg tv      = mk_msg tv <+> ptext SLIT("escapes; i.e. unifies with something more global")
unify_msg tv thing = mk_msg tv <+> ptext SLIT("is unified with") <+> quotes thing
mk_msg tv          = ptext SLIT("Quantified type variable") <+> quotes (ppr tv)
\end{code}

These two context are used with checkSigTyVars
    
\begin{code}
sigCtxt thing sig_tau tidy_env
  = zonkTcType sig_tau	`thenNF_Tc` \ zonked_sig_tau ->
    let
	(env1, [tidy_tau, tidy_zonked_tau]) = tidyTypes tidy_env [sig_tau, zonked_sig_tau]
	
	msg = vcat [ptext SLIT("When checking the type signature for") <+> thing,
		    nest 4 (ptext SLIT("Signature:") <+> ppr tidy_tau),
		    nest 4 (ptext SLIT("Inferred: ") <+> ppr tidy_zonked_tau)]
    in
    returnNF_Tc (env1, msg)

existentialPatCtxt bound_tvs bound_ids tidy_env
  = returnNF_Tc (env1,
		 sep [ptext SLIT("When checking an existential pattern that binds"),
		      nest 4 (vcat (zipWith ppr_id show_ids tidy_tys))])
  where
    tv_list  = bagToList bound_tvs
    show_ids = filter is_interesting (map snd (bagToList bound_ids))
    is_interesting id = any (`elemVarSet` idFreeTyVars id) tv_list

    (env1, tidy_tys) = tidyTypes tidy_env (map idType show_ids)
    ppr_id id ty     = ppr id <+> ptext SLIT("::") <+> ppr ty
	-- Don't zonk the types so we get the separate, un-unified versions
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
naughtyCCallContextErr clas_name
  = sep [ptext SLIT("Can't use class"), quotes (ppr clas_name), ptext SLIT("in a context")]

typeCtxt ty = ptext SLIT("In the type") <+> quotes (ppr ty)

thetaCtxt theta = ptext SLIT("In the context") <+> quotes (pprContext theta)
\end{code}

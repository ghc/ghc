
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
module TcHsType (
	tcHsSigType, tcHsPred,
	UserTypeCtxt(..), 

		-- Kind checking
	kcHsTyVars, kcHsSigType, kcHsLiftedSigType, 
	kcCheckHsType, kcHsContext,
	
		-- Typechecking kinded types
	tcHsKindedContext, tcHsKindedType, tcTyVarBndrs, dsHsType, 

	tcAddScopedTyVars, 
	
	TcSigInfo(..), tcTySig, mkTcSig, maybeSig, tcSigPolyId, tcSigMonoId
   ) where

#include "HsVersions.h"

import HsSyn		( HsType(..), HsTyVarBndr(..), HsContext, Sig(..), HsPred(..) )
import RnHsSyn		( RenamedHsType, RenamedContext, RenamedSig, extractHsTyVars )
import TcHsSyn		( TcId )

import TcRnMonad
import TcEnv		( tcExtendTyVarEnv, tcExtendTyVarKindEnv,
			  tcLookup, tcLookupClass, tcLookupTyCon,
		 	  TyThing(..), TcTyThing(..), 
			  getInLocalScope
			)
import TcMType		( newKindVar, newOpenTypeKind, tcInstType, newMutTyVar, 
			  zonkTcType, zonkTcKindToKind,
			  checkValidType, UserTypeCtxt(..), pprHsSigCtxt
			)
import TcUnify		( unifyKind, unifyFunKind )
import TcType		( Type, PredType(..), ThetaType, TyVarDetails(..),
			  TcTyVar, TcKind, TcThetaType, TcTauType,
			  mkTyVarTy, mkTyVarTys, mkFunTy, isTypeKind,
		 	  mkForAllTys, mkFunTys, tcEqType, isPredTy,
			  mkSigmaTy, mkPredTy, mkGenTyConApp, mkTyConApp, mkAppTys, 
			  liftedTypeKind, unliftedTypeKind, eqKind,
			  tcSplitFunTy_maybe, tcSplitForAllTys, tcSplitSigmaTy
			)
import PprType		( pprKind, pprThetaArrow )
import qualified Type	( splitFunTys )
import Inst		( Inst, InstOrigin(..), newMethod, instToId )

import Id		( mkLocalId, idName, idType )
import Var		( TyVar, mkTyVar, tyVarKind )
import ErrUtils		( Message )
import TyCon		( TyCon, tyConKind )
import Class		( classTyCon )
import Name		( Name )
import NameSet
import PrelNames	( genUnitTyConName )
import Subst		( deShadowTy )
import TysWiredIn	( mkListTy, mkPArrTy, mkTupleTy )
import BasicTypes	( Boxity(..) )
import SrcLoc		( SrcLoc )
import Outputable
import List		( nubBy )
\end{code}


	----------------------------
		General notes
	----------------------------

Generally speaking we now type-check types in three phases

  1.  kcHsType: kind check the HsType
	*includes* performing any TH type splices;
	so it returns a translated, and kind-annotated, type

  2.  dsHsType: convert from HsType to Type:
	perform zonking
	expand type synonyms [mkGenTyApps]
	hoist the foralls [tcHsType]

  3.  checkValidType: check the validity of the resulting type

Often these steps are done one after the other (tcHsSigType).
But in mutually recursive groups of type and class decls we do
	1 kind-check the whole group
	2 build TyCons/Classes in a knot-tied way
	3 check the validity of types in the now-unknotted TyCons/Classes

For example, when we find
	(forall a m. m a -> m a)
we bind a,m to kind varibles and kind-check (m a -> m a).  This makes
a get kind *, and m get kind *->*.  Now we typecheck (m a -> m a) in
an environment that binds a and m suitably.

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

Here we deduce			a::*->*,       b::*
But equally valid would be	a::(*->*)-> *, b::*->*


Validity checking
~~~~~~~~~~~~~~~~~
Some of the validity check could in principle be done by the kind checker, 
but not all:

- During desugaring, we normalise by expanding type synonyms.  Only
  after this step can we check things like type-synonym saturation
  e.g. 	type T k = k Int
	type S a = a
  Then (T S) is ok, because T is saturated; (T S) expands to (S Int);
  and then S is saturated.  This is a GHC extension.

- Similarly, also a GHC extension, we look through synonyms before complaining
  about the form of a class or instance declaration

- Ambiguity checks involve functional dependencies, and it's easier to wait
  until knots have been resolved before poking into them

Also, in a mutually recursive group of types, we can't look at the TyCon until we've
finished building the loop.  So to keep things simple, we postpone most validity
checking until step (3).

Knot tying
~~~~~~~~~~
During step (1) we might fault in a TyCon defined in another module, and it might
(via a loop) refer back to a TyCon defined in this module. So when we tie a big
knot around type declarations with ARecThing, so that the fault-in code can get
the TyCon being defined.


%************************************************************************
%*									*
\subsection{Checking types}
%*									*
%************************************************************************

\begin{code}
tcHsSigType :: UserTypeCtxt -> RenamedHsType -> TcM Type
  -- Do kind checking, and hoist for-alls to the top
tcHsSigType ctxt hs_ty 
  = addErrCtxt (pprHsSigCtxt ctxt hs_ty) $
    do	{ kinded_ty <- kcTypeType hs_ty
	; ty <- tcHsKindedType kinded_ty
	; checkValidType ctxt ty	
	; returnM ty }

-- tcHsPred is happy with a partial application, e.g. (ST s)
-- Used from TcDeriv
tcHsPred pred 
  = do { (kinded_pred,_) <- kc_pred pred	-- kc_pred rather than kcHsPred
						-- to avoid the partial application check
       ; dsHsPred kinded_pred }
\end{code}

	These functions are used during knot-tying in
	type and class declarations, when we have to
 	separate kind-checking, desugaring, and validity checking

\begin{code}
kcHsSigType, kcHsLiftedSigType :: HsType Name -> TcM (HsType Name)
	-- Used for type signatures
kcHsSigType ty 	     = kcTypeType ty
kcHsLiftedSigType ty = kcLiftedType ty

tcHsKindedType :: RenamedHsType -> TcM Type
  -- Don't do kind checking, nor validity checking, 
  -- 	but do hoist for-alls to the top
  -- This is used in type and class decls, where kinding is
  -- done in advance, and validity checking is done later
  -- [Validity checking done later because of knot-tying issues.]
tcHsKindedType hs_ty 
  = do	{ ty <- dsHsType hs_ty
	; return (hoistForAllTys ty) }

tcHsKindedContext :: RenamedContext -> TcM ThetaType
-- Used when we are expecting a ClassContext (i.e. no implicit params)
-- Does not do validity checking, like tcHsKindedType
tcHsKindedContext hs_theta = mappM dsHsPred hs_theta
\end{code}


%************************************************************************
%*									*
		The main kind checker: kcHsType
%*									*
%************************************************************************
	
	First a couple of simple wrappers for kcHsType

\begin{code}
---------------------------
kcLiftedType :: HsType Name -> TcM (HsType Name)
-- The type ty must be a *lifted* *type*
kcLiftedType ty = kcCheckHsType ty liftedTypeKind
    
---------------------------
kcTypeType :: HsType Name -> TcM (HsType Name)
-- The type ty must be a *type*, but it can be lifted or unlifted
-- Be sure to use checkExpectedKind, rather than simply unifying 
-- with (Type bx), because it gives better error messages
kcTypeType ty
  = kcHsType ty			`thenM` \ (ty', kind) ->
    if isTypeKind kind then
	return ty'
    else
    newOpenTypeKind				`thenM` \ type_kind ->
    traceTc (text "kcTypeType" $$ nest 2 (ppr ty $$ ppr ty' $$ ppr kind $$ ppr type_kind)) `thenM_`
    checkExpectedKind (ppr ty) kind type_kind	`thenM_`
    returnM ty'

---------------------------
kcCheckHsType :: HsType Name -> TcKind -> TcM (HsType Name)
-- Check that the type has the specified kind
kcCheckHsType ty exp_kind
  = kcHsType ty						`thenM` \ (ty', act_kind) ->
    checkExpectedKind (ppr ty) act_kind exp_kind	`thenM_`
    returnM ty'
\end{code}

	Here comes the main function

\begin{code}
kcHsType :: HsType Name -> TcM (HsType Name, TcKind)
-- kcHsType *returns* the kind of the type, rather than taking an expected
-- kind as argument as tcExpr does.  
-- Reasons: 
--	(a) the kind of (->) is
--		forall bx1 bx2. Type bx1 -> Type bx2 -> Type Boxed
--  	    so we'd need to generate huge numbers of bx variables.
--	(b) kinds are so simple that the error messages are fine
--
-- The translated type has explicitly-kinded type-variable binders

kcHsType (HsParTy ty)
 = kcHsType ty		`thenM` \ (ty', kind) ->
   returnM (HsParTy ty', kind)

kcHsType (HsTyVar name)
  = kcTyVar name	`thenM` \ kind ->
    returnM (HsTyVar name, kind)

kcHsType (HsListTy ty) 
  = kcLiftedType ty			`thenM` \ ty' ->
    returnM (HsListTy ty', liftedTypeKind)

kcHsType (HsPArrTy ty)
  = kcLiftedType ty			`thenM` \ ty' ->
    returnM (HsPArrTy ty', liftedTypeKind)

kcHsType (HsNumTy n)
   = returnM (HsNumTy n, liftedTypeKind)

kcHsType (HsKindSig ty k) 
  = kcCheckHsType ty k	`thenM` \ ty' ->
    returnM (HsKindSig ty' k, k)

kcHsType (HsTupleTy Boxed tys)
  = mappM kcLiftedType tys	`thenM` \ tys' ->
    returnM (HsTupleTy Boxed tys', liftedTypeKind)

kcHsType (HsTupleTy Unboxed tys)
  = mappM kcTypeType tys	`thenM` \ tys' ->
    returnM (HsTupleTy Unboxed tys', unliftedTypeKind)

kcHsType (HsFunTy ty1 ty2)
  = kcTypeType ty1	`thenM` \ ty1' ->
    kcTypeType ty2	`thenM` \ ty2' ->
    returnM (HsFunTy ty1' ty2', liftedTypeKind)

kcHsType ty@(HsOpTy ty1 op ty2)
  = kcTyVar op				`thenM` \ op_kind ->
    kcApps op_kind (ppr op) [ty1,ty2]	`thenM` \ ([ty1',ty2'], res_kind) ->
    returnM (HsOpTy ty1' op ty2', res_kind)

kcHsType ty@(HsAppTy ty1 ty2)
  = kcHsType fun_ty			  `thenM` \ (fun_ty', fun_kind) ->
    kcApps fun_kind (ppr fun_ty) arg_tys  `thenM` \ (arg_tys', res_kind) ->
    returnM (foldl HsAppTy fun_ty' arg_tys', res_kind)
  where
    (fun_ty, arg_tys) = split ty1 [ty2]
    split (HsAppTy f a) as = split f (a:as)
    split f             as = (f,as)

kcHsType (HsPredTy pred)
  = kcHsPred pred		`thenM` \ pred' ->
    returnM (HsPredTy pred', liftedTypeKind)

kcHsType (HsForAllTy exp tv_names context ty)
  = kcHsTyVars tv_names		$ \ tv_names' ->
    kcHsContext context		`thenM`	\ ctxt' ->
    kcLiftedType ty		`thenM` \ ty' ->
	-- The body of a forall must be a type, but in principle
	-- there's no reason to prohibit *unlifted* types.
	-- In fact, GHC can itself construct a function with an
	-- unboxed tuple inside a for-all (via CPR analyis; see 
	-- typecheck/should_compile/tc170)
	--
	-- Still, that's only for internal interfaces, which aren't
	-- kind-checked, and it's a bit inconvenient to use kcTypeType
	-- here (because it doesn't return the result kind), so I'm 
	-- leaving it as lifted types for now.
    returnM (HsForAllTy exp tv_names' ctxt' ty', liftedTypeKind)

---------------------------
kcApps :: TcKind		-- Function kind
       -> SDoc			-- Function 
       -> [HsType Name]		-- Arg types
       -> TcM ([HsType Name], TcKind)	-- Kind-checked args
kcApps fun_kind ppr_fun args
  = split_fk fun_kind (length args)	`thenM` \ (arg_kinds, res_kind) ->
    mappM kc_arg (args `zip` arg_kinds)	`thenM` \ args' ->
    returnM (args', res_kind)
  where
    split_fk fk 0 = returnM ([], fk)
    split_fk fk n = unifyFunKind fk	`thenM` \ mb_fk ->
		    case mb_fk of 
			Nothing       -> failWithTc too_many_args 
			Just (ak,fk') -> split_fk fk' (n-1)	`thenM` \ (aks, rk) ->
					 returnM (ak:aks, rk)

    kc_arg (arg, arg_kind) = kcCheckHsType arg arg_kind

    too_many_args = ptext SLIT("Kind error:") <+> quotes ppr_fun <+>
		    ptext SLIT("is applied to too many type arguments")

---------------------------
kcHsContext :: HsContext Name -> TcM (HsContext Name)
kcHsContext ctxt = mappM kcHsPred ctxt

kcHsPred pred		-- Checks that the result is of kind liftedType
  = kc_pred pred			`thenM` \ (pred', kind) ->
    checkExpectedKind (ppr pred) kind liftedTypeKind	`thenM_` 
    returnM pred'
    
---------------------------
kc_pred :: HsPred Name -> TcM (HsPred Name, TcKind)	
	-- Does *not* check for a saturated
	-- application (reason: used from TcDeriv)
kc_pred pred@(HsIParam name ty)
  = kcHsType ty		`thenM` \ (ty', kind) ->
    returnM (HsIParam name ty', kind)

kc_pred pred@(HsClassP cls tys)
  = kcClass cls			`thenM` \ kind ->
    kcApps kind (ppr cls) tys	`thenM` \ (tys', res_kind) ->
    returnM (HsClassP cls tys', res_kind)

---------------------------
kcTyVar :: Name -> TcM TcKind
kcTyVar name	-- Could be a tyvar or a tycon
  = tcLookup name	`thenM` \ thing ->
    case thing of 
	ATyVar tv	    	-> returnM (tyVarKind tv)
	ARecTyCon kind		-> returnM kind
	AGlobal (ATyCon tc) 	-> returnM (tyConKind tc) 
	other			-> failWithTc (wrongThingErr "type" thing name)

kcClass :: Name -> TcM TcKind
kcClass cls	-- Must be a class
  = tcLookup cls 				`thenM` \ thing -> 
    case thing of
	ARecClass kind		-> returnM kind
	AGlobal (AClass cls)    -> returnM (tyConKind (classTyCon cls))
	other		        -> failWithTc (wrongThingErr "class" thing cls)
\end{code}

	Helper functions


\begin{code}
---------------------------
-- We would like to get a decent error message from
--   (a) Under-applied type constructors
--		f :: (Maybe, Maybe)
--   (b) Over-applied type constructors
--		f :: Int x -> Int x
--


checkExpectedKind :: SDoc -> TcKind -> TcKind -> TcM TcKind
-- A fancy wrapper for 'unifyKind', which tries to give 
-- decent error messages.
-- Returns the same kind that it is passed, exp_kind
checkExpectedKind pp_ty act_kind exp_kind
  | act_kind `eqKind` exp_kind -- Short cut for a very common case
  = returnM exp_kind	
  | otherwise
  = tryTc (unifyKind exp_kind act_kind)	`thenM` \ (errs, mb_r) ->
    case mb_r of {
	Just _  -> returnM exp_kind ;	-- Unification succeeded
	Nothing ->

	-- So there's definitely an error
	-- Now to find out what sort
    zonkTcType exp_kind		`thenM` \ exp_kind ->
    zonkTcType act_kind		`thenM` \ act_kind ->

    let (exp_as, _) = Type.splitFunTys exp_kind
        (act_as, _) = Type.splitFunTys act_kind
		-- Use the Type versions for kinds	
	n_exp_as = length exp_as
	n_act_as = length act_as

	err | n_exp_as < n_act_as	-- E.g. [Maybe]
	    = quotes pp_ty <+> ptext SLIT("is not applied to enough type arguments")

		-- Now n_exp_as >= n_act_as. In the next two cases, 
		-- n_exp_as == 0, and hence so is n_act_as
	    | exp_kind `eqKind` liftedTypeKind && act_kind `eqKind` unliftedTypeKind
	    = ptext SLIT("Expecting a lifted type, but") <+> quotes pp_ty 
		<+> ptext SLIT("is unlifted")

	    | exp_kind `eqKind` unliftedTypeKind && act_kind `eqKind` liftedTypeKind
	    = ptext SLIT("Expecting an unlifted type, but") <+> quotes pp_ty 
		<+> ptext SLIT("is lifted")

	    | otherwise 		-- E.g. Monad [Int]
	    = sep [ ptext SLIT("Expecting kind") <+> quotes (pprKind exp_kind) <> comma,
		    ptext SLIT("but") <+> quotes pp_ty <+> 
		        ptext SLIT("has kind") <+> quotes (pprKind act_kind)]
   in
   failWithTc (ptext SLIT("Kind error:") <+> err) 
   }
\end{code}

%************************************************************************
%*									*
		Desugaring
%*									*
%************************************************************************

The type desugarer

	* Transforms from HsType to Type
	* Zonks any kinds

It cannot fail, and does no validity checking

\begin{code}
dsHsType :: HsType Name 	-- All HsTyVarBndrs are kind-annotated
	 -> TcM Type

dsHsType ty@(HsTyVar name)
  = ds_app ty []

dsHsType (HsParTy ty)		-- Remove the parentheses markers
  = dsHsType ty

dsHsType (HsKindSig ty k)
  = dsHsType ty	-- Kind checking done already

dsHsType (HsListTy ty)
  = dsHsType ty				`thenM` \ tau_ty ->
    returnM (mkListTy tau_ty)

dsHsType (HsPArrTy ty)
  = dsHsType ty				`thenM` \ tau_ty ->
    returnM (mkPArrTy tau_ty)

dsHsType (HsTupleTy boxity tys)
  = dsHsTypes tys			`thenM` \ tau_tys ->
    returnM (mkTupleTy boxity (length tys) tau_tys)

dsHsType (HsFunTy ty1 ty2)
  = dsHsType ty1			`thenM` \ tau_ty1 ->
    dsHsType ty2			`thenM` \ tau_ty2 ->
    returnM (mkFunTy tau_ty1 tau_ty2)

dsHsType (HsOpTy ty1 op ty2)
  = dsHsType ty1 `thenM` \ tau_ty1 ->
    dsHsType ty2 `thenM` \ tau_ty2 ->
    ds_var_app op [tau_ty1,tau_ty2]

dsHsType (HsNumTy n)
  = ASSERT(n==1)
    tcLookupTyCon genUnitTyConName	`thenM` \ tc ->
    returnM (mkTyConApp tc [])

dsHsType ty@(HsAppTy ty1 ty2) 
  = ds_app ty1 [ty2]

dsHsType (HsPredTy pred)
  = dsHsPred pred	`thenM` \ pred' ->
    returnM (mkPredTy pred')

dsHsType full_ty@(HsForAllTy exp tv_names ctxt ty)
  = tcTyVarBndrs tv_names		$ \ tyvars ->
    mappM dsHsPred ctxt			`thenM` \ theta ->
    dsHsType ty				`thenM` \ tau ->
    returnM (mkSigmaTy tyvars theta tau)

dsHsTypes arg_tys = mappM dsHsType arg_tys
\end{code}

Help functions for type applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
ds_app :: HsType Name -> [HsType Name] -> TcM Type
ds_app (HsAppTy ty1 ty2) tys
  = ds_app ty1 (ty2:tys)

ds_app ty tys
  = dsHsTypes tys			`thenM` \ arg_tys ->
    case ty of
	HsTyVar fun -> ds_var_app fun arg_tys
	other	    -> dsHsType ty		`thenM` \ fun_ty ->
		       returnM (mkAppTys fun_ty arg_tys)

ds_var_app :: Name -> [Type] -> TcM Type
ds_var_app name arg_tys 
 = tcLookup name			`thenM` \ thing ->
    case thing of
	ATyVar tv 	     -> returnM (mkAppTys (mkTyVarTy tv) arg_tys)
	AGlobal (ATyCon tc)  -> returnM (mkGenTyConApp tc arg_tys)
	ARecTyCon _ 	     -> tcLookupTyCon name	`thenM` \ tc ->
			        returnM (mkGenTyConApp tc arg_tys)
	other -> pprPanic "ds_app_type" (ppr name <+> ppr arg_tys)
\end{code}


Contexts
~~~~~~~~
\begin{code}
dsHsPred :: HsPred Name -> TcM PredType
dsHsPred pred@(HsClassP class_name tys)
  = dsHsTypes tys			`thenM` \ arg_tys ->
    tcLookupClass class_name		`thenM` \ clas ->
    returnM (ClassP clas arg_tys)

dsHsPred (HsIParam name ty)
  = dsHsType ty					`thenM` \ arg_ty ->
    returnM (IParam name arg_ty)
\end{code}


%************************************************************************
%*									*
		Type-variable binders
%*									*
%************************************************************************


\begin{code}
kcHsTyVars :: [HsTyVarBndr Name] 
	   -> ([HsTyVarBndr Name] -> TcM r) 	-- These binders are kind-annotated
						-- They scope over the thing inside
	   -> TcM r
kcHsTyVars tvs thing_inside 
  = mappM kcHsTyVar tvs		`thenM` \ bndrs ->
    tcExtendTyVarKindEnv bndrs 	$
    thing_inside bndrs

kcHsTyVar :: HsTyVarBndr Name -> TcM (HsTyVarBndr Name)
	-- Return a *kind-annotated* binder, and a tyvar with a mutable kind in it	
kcHsTyVar (UserTyVar name)        = newKindVar 	`thenM` \ kind ->
				    returnM (KindedTyVar name kind)
kcHsTyVar (KindedTyVar name kind) = returnM (KindedTyVar name kind)

------------------
tcTyVarBndrs :: [HsTyVarBndr Name] 	-- Kind-annotated binders, which need kind-zonking
	     -> ([TyVar] -> TcM r)
	     -> TcM r
-- Used when type-checking types/classes/type-decls
-- Brings into scope immutable TyVars, not mutable ones that require later zonking
tcTyVarBndrs bndrs thing_inside
  = mapM zonk bndrs	`thenM` \ tyvars ->
    tcExtendTyVarEnv tyvars (thing_inside tyvars)
  where
    zonk (KindedTyVar name kind) = zonkTcKindToKind kind	`thenM` \ kind' ->
				   returnM (mkTyVar name kind')
    zonk (UserTyVar name) = pprTrace "BAD: Un-kinded tyvar" (ppr name) $
			    returnM (mkTyVar name liftedTypeKind)
\end{code}


%************************************************************************
%*									*
		Scoped type variables
%*									*
%************************************************************************


tcAddScopedTyVars is used for scoped type variables added by pattern
type signatures
	e.g.  \ ((x::a), (y::a)) -> x+y
They never have explicit kinds (because this is source-code only)
They are mutable (because they can get bound to a more specific type).

Usually we kind-infer and expand type splices, and then
tupecheck/desugar the type.  That doesn't work well for scoped type
variables, because they scope left-right in patterns.  (e.g. in the
example above, the 'a' in (y::a) is bound by the 'a' in (x::a).

The current not-very-good plan is to
  * find all the types in the patterns
  * find their free tyvars
  * do kind inference
  * bring the kinded type vars into scope
  * BUT throw away the kind-checked type
  	(we'll kind-check it again when we type-check the pattern)

This is bad because throwing away the kind checked type throws away
its splices.  But too bad for now.  [July 03]

Historical note:
    We no longer specify that these type variables must be univerally 
    quantified (lots of email on the subject).  If you want to put that 
    back in, you need to
	a) Do a checkSigTyVars after thing_inside
	b) More insidiously, don't pass in expected_ty, else
	   we unify with it too early and checkSigTyVars barfs
	   Instead you have to pass in a fresh ty var, and unify
	   it with expected_ty afterwards

\begin{code}
tcAddScopedTyVars :: [RenamedHsType] -> TcM a -> TcM a
tcAddScopedTyVars [] thing_inside
  = thing_inside	-- Quick get-out for the empty case

tcAddScopedTyVars sig_tys thing_inside
  = getInLocalScope			`thenM` \ in_scope ->
    let
	sig_tvs = [ UserTyVar n | ty <- sig_tys,
				  n <- nameSetToList (extractHsTyVars ty),
				  not (in_scope n) ]
	-- The tyvars we want are the free type variables of 
	-- the type that are not already in scope
    in	      
	-- Behave like kcHsType on a ForAll type
	-- i.e. make kinded tyvars with mutable kinds, 
	--      and kind-check the enclosed types
    kcHsTyVars sig_tvs (\ kinded_tvs -> do
			    { mappM kcTypeType sig_tys
			    ; return kinded_tvs })	`thenM` \ kinded_tvs ->

	-- Zonk the mutable kinds and bring the tyvars into scope
	-- Rather like tcTyVarBndrs, except that it brings *mutable* 
	-- tyvars into scope, not immutable ones
	--
	-- Furthermore, the tyvars are PatSigTvs, which means that we get better
	-- error messages when type variables escape:
	--      Inferred type is less polymorphic than expected
	--   	Quantified type variable `t' escapes
	--   	It is mentioned in the environment:
	--	t is bound by the pattern type signature at tcfail103.hs:6
    mapM zonk kinded_tvs	`thenM` \ tyvars ->
    tcExtendTyVarEnv tyvars thing_inside

  where
    zonk (KindedTyVar name kind) = zonkTcKindToKind kind	`thenM` \ kind' ->
				   newMutTyVar name kind' PatSigTv
    zonk (UserTyVar name) = pprTrace "BAD: Un-kinded tyvar" (ppr name) $
			    returnM (mkTyVar name liftedTypeKind)
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
    ppr (TySigInfo id tyvars theta tau _ inst loc) =
	ppr id <+> ptext SLIT("::") <+> ppr tyvars <+> ppr theta <+> ptext SLIT("=>") <+> ppr tau

tcSigPolyId :: TcSigInfo -> TcId
tcSigPolyId (TySigInfo id _ _ _ _ _ _) = id

tcSigMonoId :: TcSigInfo -> TcId
tcSigMonoId (TySigInfo _ _ _ _ id _ _) = id

maybeSig :: [TcSigInfo] -> Name -> Maybe (TcSigInfo)
	-- Search for a particular signature
maybeSig [] name = Nothing
maybeSig (sig@(TySigInfo sig_id _ _ _ _ _ _) : sigs) name
  | name == idName sig_id = Just sig
  | otherwise	     	  = maybeSig sigs name
\end{code}


\begin{code}
tcTySig :: RenamedSig -> TcM TcSigInfo

tcTySig (Sig v ty src_loc)
 = addSrcLoc src_loc			$ 
   tcHsSigType (FunSigCtxt v) ty	`thenM` \ sigma_tc_ty ->
   mkTcSig (mkLocalId v sigma_tc_ty) 	`thenM` \ sig -> 
   returnM sig

mkTcSig :: TcId -> TcM TcSigInfo
mkTcSig poly_id
  = 	-- Instantiate this type
	-- It's important to do this even though in the error-free case
	-- we could just split the sigma_tc_ty (since the tyvars don't
	-- unified with anything).  But in the case of an error, when
	-- the tyvars *do* get unified with something, we want to carry on
	-- typechecking the rest of the program with the function bound
	-- to a pristine type, namely sigma_tc_ty
   tcInstType SigTv (idType poly_id)		`thenM` \ (tyvars', theta', tau') ->

   getInstLoc SignatureOrigin			`thenM` \ inst_loc ->
   newMethod inst_loc poly_id
	     (mkTyVarTys tyvars')
	     theta' tau'			`thenM` \ inst ->
	-- We make a Method even if it's not overloaded; no harm
	-- But do not extend the LIE!  We're just making an Id.
	
   getSrcLocM					`thenM` \ src_loc ->
   returnM (TySigInfo poly_id tyvars' theta' tau' 
			  (instToId inst) [inst] src_loc)
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************


\begin{code}
hoistForAllTys :: Type -> Type
-- Used for user-written type signatures only
-- Move all the foralls and constraints to the top
-- e.g.  T -> forall a. a        ==>   forall a. T -> a
--	 T -> (?x::Int) -> Int   ==>   (?x::Int) -> T -> Int
--
-- Also: eliminate duplicate constraints.  These can show up
-- when hoisting constraints, notably implicit parameters.
--
-- We want to 'look through' type synonyms when doing this
-- so it's better done on the Type than the HsType

hoistForAllTys ty
  = let
	no_shadow_ty = deShadowTy ty
	-- Running over ty with an empty substitution gives it the
	-- no-shadowing property.  This is important.  For example:
	--	type Foo r = forall a. a -> r
	--	foo :: Foo (Foo ())
	-- Here the hoisting should give
	--	foo :: forall a a1. a -> a1 -> ()
	--
	-- What about type vars that are lexically in scope in the envt?
	-- We simply rely on them having a different unique to any
	-- binder in 'ty'.  Otherwise we'd have to slurp the in-scope-tyvars
	-- out of the envt, which is boring and (I think) not necessary.
    in
    case hoist no_shadow_ty of 
	(tvs, theta, body) -> mkForAllTys tvs (mkFunTys (nubBy tcEqType theta) body)
		-- The 'nubBy' eliminates duplicate constraints,
		-- notably implicit parameters
  where
    hoist ty
	| (tvs1, body_ty) <- tcSplitForAllTys ty,
	  not (null tvs1)
	= case hoist body_ty of
		(tvs2,theta,tau) -> (tvs1 ++ tvs2, theta, tau)

	| Just (arg, res) <- tcSplitFunTy_maybe ty
	= let
	      arg' = hoistForAllTys arg	-- Don't forget to apply hoist recursively
	  in				-- to the argument type
	  if (isPredTy arg') then
	    case hoist res of
		(tvs,theta,tau) -> (tvs, arg':theta, tau)
	  else
	     case hoist res of
		(tvs,theta,tau) -> (tvs, theta, mkFunTy arg' tau)

	| otherwise = ([], [], ty)
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
wrongThingErr expected thing name
  = pp_thing thing <+> quotes (ppr name) <+> ptext SLIT("used as a") <+> text expected
  where
    pp_thing (AGlobal (ATyCon _))   = ptext SLIT("Type constructor")
    pp_thing (AGlobal (AClass _))   = ptext SLIT("Class")
    pp_thing (AGlobal (AnId   _))   = ptext SLIT("Identifier")
    pp_thing (AGlobal (ADataCon _)) = ptext SLIT("Data constructor")
    pp_thing (ATyVar _) 	    = ptext SLIT("Type variable")
    pp_thing (ATcId _ _ _)  	    = ptext SLIT("Local identifier")
    pp_thing (ARecTyCon _) 	    = ptext SLIT("Rec tycon")
    pp_thing (ARecClass _) 	    = ptext SLIT("Rec class")
\end{code}

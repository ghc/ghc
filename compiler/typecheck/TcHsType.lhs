%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
module TcHsType (
	tcHsSigType, tcHsDeriv,
	UserTypeCtxt(..), 

		-- Kind checking
	kcHsTyVars, kcHsSigType, kcHsLiftedSigType, 
	kcCheckHsType, kcHsContext, kcHsType, 
	
		-- Typechecking kinded types
	tcHsKindedContext, tcHsKindedType, tcHsBangType,
	tcTyVarBndrs, dsHsType, tcLHsConResTy,
	tcDataKindSig,

		-- Pattern type signatures
	tcHsPatSigType, tcPatSig
   ) where

#include "HsVersions.h"

import HsSyn
import RnHsSyn
import TcRnMonad
import TcEnv
import TcMType
import TcUnify
import TcIface
import TcType
import {- Kind parts of -} Type
import Var
import TyCon
import Class
import Name
import OccName
import NameSet
import PrelNames
import TysWiredIn
import BasicTypes
import SrcLoc
import UniqSupply
import Outputable
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
tcHsSigType :: UserTypeCtxt -> LHsType Name -> TcM Type
  -- Do kind checking, and hoist for-alls to the top
  -- NB: it's important that the foralls that come from the top-level
  --	 HsForAllTy in hs_ty occur *first* in the returned type.
  --     See Note [Scoped] with TcSigInfo
tcHsSigType ctxt hs_ty 
  = addErrCtxt (pprHsSigCtxt ctxt hs_ty) $
    do	{ kinded_ty <- kcTypeType hs_ty
	; ty <- tcHsKindedType kinded_ty
	; checkValidType ctxt ty	
	; returnM ty }

-- Used for the deriving(...) items
tcHsDeriv :: LHsType Name -> TcM ([TyVar], Class, [Type])
tcHsDeriv = addLocM (tc_hs_deriv [])

tc_hs_deriv tv_names (HsPredTy (HsClassP cls_name hs_tys))
  = kcHsTyVars tv_names 		$ \ tv_names' ->
    do	{ cls_kind <- kcClass cls_name
	; (tys, res_kind) <- kcApps cls_kind (ppr cls_name) hs_tys
	; tcTyVarBndrs tv_names'	$ \ tyvars ->
    do	{ arg_tys <- dsHsTypes tys
	; cls <- tcLookupClass cls_name
	; return (tyvars, cls, arg_tys) }}

tc_hs_deriv tv_names1 (HsForAllTy _ tv_names2 (L _ []) (L _ ty))
  = 	-- Funny newtype deriving form
	-- 	forall a. C [a]
	-- where C has arity 2.  Hence can't use regular functions
    tc_hs_deriv (tv_names1 ++ tv_names2) ty

tc_hs_deriv _ other
  = failWithTc (ptext SLIT("Illegal deriving item") <+> ppr other)
\end{code}

	These functions are used during knot-tying in
	type and class declarations, when we have to
 	separate kind-checking, desugaring, and validity checking

\begin{code}
kcHsSigType, kcHsLiftedSigType :: LHsType Name -> TcM (LHsType Name)
	-- Used for type signatures
kcHsSigType ty 	     = kcTypeType ty
kcHsLiftedSigType ty = kcLiftedType ty

tcHsKindedType :: LHsType Name -> TcM Type
  -- Don't do kind checking, nor validity checking.
  -- This is used in type and class decls, where kinding is
  -- done in advance, and validity checking is done later
  -- [Validity checking done later because of knot-tying issues.]
tcHsKindedType hs_ty = dsHsType hs_ty

tcHsBangType :: LHsType Name -> TcM Type
-- Permit a bang, but discard it
tcHsBangType (L span (HsBangTy b ty)) = tcHsKindedType ty
tcHsBangType ty 		      = tcHsKindedType ty

tcHsKindedContext :: LHsContext Name -> TcM ThetaType
-- Used when we are expecting a ClassContext (i.e. no implicit params)
-- Does not do validity checking, like tcHsKindedType
tcHsKindedContext hs_theta = addLocM (mappM dsHsLPred) hs_theta
\end{code}


%************************************************************************
%*									*
		The main kind checker: kcHsType
%*									*
%************************************************************************
	
	First a couple of simple wrappers for kcHsType

\begin{code}
---------------------------
kcLiftedType :: LHsType Name -> TcM (LHsType Name)
-- The type ty must be a *lifted* *type*
kcLiftedType ty = kcCheckHsType ty liftedTypeKind
    
---------------------------
kcTypeType :: LHsType Name -> TcM (LHsType Name)
-- The type ty must be a *type*, but it can be lifted or 
-- unlifted or an unboxed tuple.
kcTypeType ty = kcCheckHsType ty openTypeKind

---------------------------
kcCheckHsType :: LHsType Name -> TcKind -> TcM (LHsType Name)
-- Check that the type has the specified kind
-- Be sure to use checkExpectedKind, rather than simply unifying 
-- with OpenTypeKind, because it gives better error messages
kcCheckHsType (L span ty) exp_kind 
  = setSrcSpan span				$
    do	{ (ty', act_kind) <- add_ctxt ty (kc_hs_type ty)
		-- Add the context round the inner check only
		-- because checkExpectedKind already mentions
		-- 'ty' by name in any error message

	; checkExpectedKind (strip ty) act_kind exp_kind
	; return (L span ty') }
  where
	-- Wrap a context around only if we want to show that contexts.  
    add_ctxt (HsPredTy p)		 thing = thing
	-- Omit invisble ones and ones user's won't grok (HsPred p).
    add_ctxt (HsForAllTy _ _ (L _ []) _) thing = thing
	-- Omit wrapping if the theta-part is empty
	-- Reason: the recursive call to kcLiftedType, in the ForAllTy
	--	   case of kc_hs_type, will do the wrapping instead
	--	   and we don't want to duplicate
    add_ctxt other_ty thing = addErrCtxt (typeCtxt other_ty) thing

	-- We infer the kind of the type, and then complain if it's
	-- not right.  But we don't want to complain about
	--	(ty) or !(ty) or forall a. ty
	-- when the real difficulty is with the 'ty' part.
    strip (HsParTy (L _ ty))          = strip ty
    strip (HsBangTy _ (L _ ty))       = strip ty
    strip (HsForAllTy _ _ _ (L _ ty)) = strip ty
    strip ty			      = ty
\end{code}

	Here comes the main function

\begin{code}
kcHsType :: LHsType Name -> TcM (LHsType Name, TcKind)
kcHsType ty = wrapLocFstM kc_hs_type ty
-- kcHsType *returns* the kind of the type, rather than taking an expected
-- kind as argument as tcExpr does.  
-- Reasons: 
--	(a) the kind of (->) is
--		forall bx1 bx2. Type bx1 -> Type bx2 -> Type Boxed
--  	    so we'd need to generate huge numbers of bx variables.
--	(b) kinds are so simple that the error messages are fine
--
-- The translated type has explicitly-kinded type-variable binders

kc_hs_type (HsParTy ty)
 = kcHsType ty		`thenM` \ (ty', kind) ->
   returnM (HsParTy ty', kind)

kc_hs_type (HsTyVar name)
  = kcTyVar name	`thenM` \ kind ->
    returnM (HsTyVar name, kind)

kc_hs_type (HsListTy ty) 
  = kcLiftedType ty			`thenM` \ ty' ->
    returnM (HsListTy ty', liftedTypeKind)

kc_hs_type (HsPArrTy ty)
  = kcLiftedType ty			`thenM` \ ty' ->
    returnM (HsPArrTy ty', liftedTypeKind)

kc_hs_type (HsNumTy n)
   = returnM (HsNumTy n, liftedTypeKind)

kc_hs_type (HsKindSig ty k) 
  = kcCheckHsType ty k	`thenM` \ ty' ->
    returnM (HsKindSig ty' k, k)

kc_hs_type (HsTupleTy Boxed tys)
  = mappM kcLiftedType tys	`thenM` \ tys' ->
    returnM (HsTupleTy Boxed tys', liftedTypeKind)

kc_hs_type (HsTupleTy Unboxed tys)
  = mappM kcTypeType tys	`thenM` \ tys' ->
    returnM (HsTupleTy Unboxed tys', ubxTupleKind)

kc_hs_type (HsFunTy ty1 ty2)
  = kcCheckHsType ty1 argTypeKind	`thenM` \ ty1' ->
    kcTypeType ty2			`thenM` \ ty2' ->
    returnM (HsFunTy ty1' ty2', liftedTypeKind)

kc_hs_type ty@(HsOpTy ty1 op ty2)
  = addLocM kcTyVar op			`thenM` \ op_kind ->
    kcApps op_kind (ppr op) [ty1,ty2]	`thenM` \ ([ty1',ty2'], res_kind) ->
    returnM (HsOpTy ty1' op ty2', res_kind)

kc_hs_type ty@(HsAppTy ty1 ty2)
  = kcHsType fun_ty			  `thenM` \ (fun_ty', fun_kind) ->
    kcApps fun_kind (ppr fun_ty) arg_tys  `thenM` \ ((arg_ty':arg_tys'), res_kind) ->
    returnM (foldl mk_app (HsAppTy fun_ty' arg_ty') arg_tys', res_kind)
  where
    (fun_ty, arg_tys) = split ty1 [ty2]
    split (L _ (HsAppTy f a)) as = split f (a:as)
    split f       	      as = (f,as)
    mk_app fun arg = HsAppTy (noLoc fun) arg	-- Add noLocs for inner nodes of
						-- the application; they are never used
    
kc_hs_type (HsPredTy pred)
  = kcHsPred pred		`thenM` \ pred' ->
    returnM (HsPredTy pred', liftedTypeKind)

kc_hs_type (HsForAllTy exp tv_names context ty)
  = kcHsTyVars tv_names		$ \ tv_names' ->
    do	{ ctxt' <- kcHsContext context
	; ty'   <- kcLiftedType ty
	     -- The body of a forall is usually a type, but in principle
	     -- there's no reason to prohibit *unlifted* types.
	     -- In fact, GHC can itself construct a function with an
	     -- unboxed tuple inside a for-all (via CPR analyis; see 
	     -- typecheck/should_compile/tc170)
	     --
	     -- Still, that's only for internal interfaces, which aren't
	     -- kind-checked, so we only allow liftedTypeKind here

  	; return (HsForAllTy exp tv_names' ctxt' ty', liftedTypeKind) }

kc_hs_type (HsBangTy b ty)
  = do { (ty', kind) <- kcHsType ty
       ; return (HsBangTy b ty', kind) }

kc_hs_type ty@(HsSpliceTy _)
  = failWithTc (ptext SLIT("Unexpected type splice:") <+> ppr ty)

-- remove the doc nodes here, no need to worry about the location since
-- its the same for a doc node and it's child type node
kc_hs_type (HsDocTy ty _)
  = kc_hs_type (unLoc ty) 

---------------------------
kcApps :: TcKind			-- Function kind
       -> SDoc				-- Function 
       -> [LHsType Name]		-- Arg types
       -> TcM ([LHsType Name], TcKind)	-- Kind-checked args
kcApps fun_kind ppr_fun args
  = split_fk fun_kind (length args)	`thenM` \ (arg_kinds, res_kind) ->
    zipWithM kc_arg args arg_kinds	`thenM` \ args' ->
    returnM (args', res_kind)
  where
    split_fk fk 0 = returnM ([], fk)
    split_fk fk n = unifyFunKind fk	`thenM` \ mb_fk ->
		    case mb_fk of 
			Nothing       -> failWithTc too_many_args 
			Just (ak,fk') -> split_fk fk' (n-1)	`thenM` \ (aks, rk) ->
					 returnM (ak:aks, rk)

    kc_arg arg arg_kind = kcCheckHsType arg arg_kind

    too_many_args = ptext SLIT("Kind error:") <+> quotes ppr_fun <+>
		    ptext SLIT("is applied to too many type arguments")

---------------------------
kcHsContext :: LHsContext Name -> TcM (LHsContext Name)
kcHsContext ctxt = wrapLocM (mappM kcHsLPred) ctxt

kcHsLPred :: LHsPred Name -> TcM (LHsPred Name)
kcHsLPred = wrapLocM kcHsPred

kcHsPred :: HsPred Name -> TcM (HsPred Name)
kcHsPred pred	-- Checks that the result is of kind liftedType
  = kc_pred pred				`thenM` \ (pred', kind) ->
    checkExpectedKind pred kind liftedTypeKind	`thenM_` 
    returnM pred'
    
---------------------------
kc_pred :: HsPred Name -> TcM (HsPred Name, TcKind)	
	-- Does *not* check for a saturated
	-- application (reason: used from TcDeriv)
kc_pred pred@(HsIParam name ty)
  = do { (ty', kind) <- kcHsType ty
       ; returnM (HsIParam name ty', kind)
       }
kc_pred pred@(HsClassP cls tys)
  = do { kind <- kcClass cls
       ; (tys', res_kind) <- kcApps kind (ppr cls) tys
       ; returnM (HsClassP cls tys', res_kind)
       }
kc_pred pred@(HsEqualP ty1 ty2)
  = do { (ty1', kind1) <- kcHsType ty1
       ; checkExpectedKind ty1 kind1 liftedTypeKind
       ; (ty2', kind2) <- kcHsType ty2
       ; checkExpectedKind ty2 kind2 liftedTypeKind
       ; returnM (HsEqualP ty1 ty2, liftedTypeKind)
       }

---------------------------
kcTyVar :: Name -> TcM TcKind
kcTyVar name	-- Could be a tyvar or a tycon
  = traceTc (text "lk1" <+> ppr name) 	`thenM_`
    tcLookup name	`thenM` \ thing ->
    traceTc (text "lk2" <+> ppr name <+> ppr thing) 	`thenM_`
    case thing of 
	ATyVar _ ty	    	-> returnM (typeKind ty)
	AThing kind		-> returnM kind
	AGlobal (ATyCon tc) 	-> returnM (tyConKind tc) 
	other			-> wrongThingErr "type" thing name

kcClass :: Name -> TcM TcKind
kcClass cls	-- Must be a class
  = tcLookup cls 				`thenM` \ thing -> 
    case thing of
	AThing kind		-> returnM kind
	AGlobal (AClass cls)    -> returnM (tyConKind (classTyCon cls))
	other		        -> wrongThingErr "class" thing cls
\end{code}


%************************************************************************
%*									*
		Desugaring
%*									*
%************************************************************************

The type desugarer

	* Transforms from HsType to Type
	* Zonks any kinds

It cannot fail, and does no validity checking, except for 
structural matters, such as
	(a) spurious ! annotations.
	(b) a class used as a type

\begin{code}
dsHsType :: LHsType Name -> TcM Type
-- All HsTyVarBndrs in the intput type are kind-annotated
dsHsType ty = ds_type (unLoc ty)

ds_type ty@(HsTyVar name)
  = ds_app ty []

ds_type (HsParTy ty)		-- Remove the parentheses markers
  = dsHsType ty

ds_type ty@(HsBangTy _ _)	-- No bangs should be here
  = failWithTc (ptext SLIT("Unexpected strictness annotation:") <+> ppr ty)

ds_type (HsKindSig ty k)
  = dsHsType ty	-- Kind checking done already

ds_type (HsListTy ty)
  = dsHsType ty			`thenM` \ tau_ty ->
    checkWiredInTyCon listTyCon	`thenM_`
    returnM (mkListTy tau_ty)

ds_type (HsPArrTy ty)
  = dsHsType ty			`thenM` \ tau_ty ->
    checkWiredInTyCon parrTyCon	`thenM_`
    returnM (mkPArrTy tau_ty)

ds_type (HsTupleTy boxity tys)
  = dsHsTypes tys		`thenM` \ tau_tys ->
    checkWiredInTyCon tycon	`thenM_`
    returnM (mkTyConApp tycon tau_tys)
  where
    tycon = tupleTyCon boxity (length tys)

ds_type (HsFunTy ty1 ty2)
  = dsHsType ty1			`thenM` \ tau_ty1 ->
    dsHsType ty2			`thenM` \ tau_ty2 ->
    returnM (mkFunTy tau_ty1 tau_ty2)

ds_type (HsOpTy ty1 (L span op) ty2)
  = dsHsType ty1 		`thenM` \ tau_ty1 ->
    dsHsType ty2 		`thenM` \ tau_ty2 ->
    setSrcSpan span (ds_var_app op [tau_ty1,tau_ty2])

ds_type (HsNumTy n)
  = ASSERT(n==1)
    tcLookupTyCon genUnitTyConName	`thenM` \ tc ->
    returnM (mkTyConApp tc [])

ds_type ty@(HsAppTy _ _)
  = ds_app ty []

ds_type (HsPredTy pred)
  = dsHsPred pred	`thenM` \ pred' ->
    returnM (mkPredTy pred')

ds_type full_ty@(HsForAllTy exp tv_names ctxt ty)
  = tcTyVarBndrs tv_names		$ \ tyvars ->
    mappM dsHsLPred (unLoc ctxt)	`thenM` \ theta ->
    dsHsType ty				`thenM` \ tau ->
    returnM (mkSigmaTy tyvars theta tau)

ds_type (HsSpliceTy {}) = panic "ds_type: HsSpliceTy"

dsHsTypes arg_tys = mappM dsHsType arg_tys
\end{code}

Help functions for type applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
ds_app :: HsType Name -> [LHsType Name] -> TcM Type
ds_app (HsAppTy ty1 ty2) tys
  = ds_app (unLoc ty1) (ty2:tys)

ds_app ty tys
  = dsHsTypes tys			`thenM` \ arg_tys ->
    case ty of
	HsTyVar fun -> ds_var_app fun arg_tys
	other	    -> ds_type ty		`thenM` \ fun_ty ->
		       returnM (mkAppTys fun_ty arg_tys)

ds_var_app :: Name -> [Type] -> TcM Type
ds_var_app name arg_tys 
 = tcLookup name			`thenM` \ thing ->
    case thing of
	ATyVar _ ty 	    -> returnM (mkAppTys ty arg_tys)
	AGlobal (ATyCon tc) -> returnM (mkTyConApp tc arg_tys)
	other		    -> wrongThingErr "type" thing name
\end{code}


Contexts
~~~~~~~~

\begin{code}
dsHsLPred :: LHsPred Name -> TcM PredType
dsHsLPred pred = dsHsPred (unLoc pred)

dsHsPred pred@(HsClassP class_name tys)
  = do { arg_tys <- dsHsTypes tys
       ; clas <- tcLookupClass class_name
       ; returnM (ClassP clas arg_tys)
       }
dsHsPred pred@(HsEqualP ty1 ty2)
  = do { arg_ty1 <- dsHsType ty1
       ; arg_ty2 <- dsHsType ty2
       ; returnM (EqPred arg_ty1 arg_ty2)
       }
dsHsPred (HsIParam name ty)
  = do { arg_ty <- dsHsType ty
       ; returnM (IParam name arg_ty)
       }
\end{code}

GADT constructor signatures

\begin{code}
tcLHsConResTy :: LHsType Name -> TcM (TyCon, [TcType])
tcLHsConResTy res_ty
  = addErrCtxt (gadtResCtxt res_ty) $
    case get_largs res_ty [] of
	   (HsTyVar tc_name, args) 
	      -> do { args' <- mapM dsHsType args
		    ; thing <- tcLookup tc_name
		    ; case thing of
		        AGlobal (ATyCon tc) -> return (tc, args')
		        other -> failWithTc (badGadtDecl res_ty) }
	   other -> failWithTc (badGadtDecl res_ty)
  where
	-- We can't call dsHsType on res_ty, and then do tcSplitTyConApp_maybe
	-- because that causes a black hole, and for good reason.  Building
	-- the type means expanding type synonyms, and we can't do that
	-- inside the "knot".  So we have to work by steam.
    get_largs (L _ ty) args = get_args ty args
    get_args (HsAppTy fun arg) 		  args = get_largs fun (arg:args)
    get_args (HsParTy ty)      		  args = get_largs ty  args
    get_args (HsOpTy ty1 (L span tc) ty2) args = (HsTyVar tc, ty1:ty2:args)
    get_args ty  	     		  args = (ty, args)

gadtResCtxt ty
  = hang (ptext SLIT("In the result type of a data constructor:"))
       2 (ppr ty)
badGadtDecl ty
  = hang (ptext SLIT("Malformed constructor result type:"))
       2 (ppr ty)

typeCtxt ty = ptext SLIT("In the type") <+> quotes (ppr ty)
\end{code}

%************************************************************************
%*									*
		Type-variable binders
%*									*
%************************************************************************


\begin{code}
kcHsTyVars :: [LHsTyVarBndr Name] 
	   -> ([LHsTyVarBndr Name] -> TcM r) 	-- These binders are kind-annotated
						-- They scope over the thing inside
	   -> TcM r
kcHsTyVars tvs thing_inside 
  = mappM (wrapLocM kcHsTyVar) tvs	`thenM` \ bndrs ->
    tcExtendKindEnvTvs bndrs (thing_inside bndrs)

kcHsTyVar :: HsTyVarBndr Name -> TcM (HsTyVarBndr Name)
	-- Return a *kind-annotated* binder, and a tyvar with a mutable kind in it	
kcHsTyVar (UserTyVar name)        = newKindVar 	`thenM` \ kind ->
				    returnM (KindedTyVar name kind)
kcHsTyVar (KindedTyVar name kind) = returnM (KindedTyVar name kind)

------------------
tcTyVarBndrs :: [LHsTyVarBndr Name] 	-- Kind-annotated binders, which need kind-zonking
	     -> ([TyVar] -> TcM r)
	     -> TcM r
-- Used when type-checking types/classes/type-decls
-- Brings into scope immutable TyVars, not mutable ones that require later zonking
tcTyVarBndrs bndrs thing_inside
  = mapM (zonk . unLoc) bndrs	`thenM` \ tyvars ->
    tcExtendTyVarEnv tyvars (thing_inside tyvars)
  where
    zonk (KindedTyVar name kind) = do { kind' <- zonkTcKindToKind kind
				      ; return (mkTyVar name kind') }
    zonk (UserTyVar name) = pprTrace "Un-kinded tyvar" (ppr name) $
			    return (mkTyVar name liftedTypeKind)

-----------------------------------
tcDataKindSig :: Maybe Kind -> TcM [TyVar]
-- GADT decls can have a (perhaps partial) kind signature
--	e.g.  data T :: * -> * -> * where ...
-- This function makes up suitable (kinded) type variables for 
-- the argument kinds, and checks that the result kind is indeed *.
-- We use it also to make up argument type variables for for data instances.
tcDataKindSig Nothing = return []
tcDataKindSig (Just kind)
  = do	{ checkTc (isLiftedTypeKind res_kind) (badKindSig kind)
	; span <- getSrcSpanM
	; us   <- newUniqueSupply 
	; let loc   = srcSpanStart span
	      uniqs = uniqsFromSupply us
	; return [ mk_tv loc uniq str kind 
		 | ((kind, str), uniq) <- arg_kinds `zip` names `zip` uniqs ] }
  where
    (arg_kinds, res_kind) = splitKindFunTys kind
    mk_tv loc uniq str kind = mkTyVar name kind
	where
	   name = mkInternalName uniq occ loc
	   occ  = mkOccName tvName str

    names :: [String]	-- a,b,c...aa,ab,ac etc
    names = [ c:cs | cs <- "" : names, c <- ['a'..'z'] ] 

badKindSig :: Kind -> SDoc
badKindSig kind 
 = hang (ptext SLIT("Kind signature on data type declaration has non-* return kind"))
	2 (ppr kind)
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
tcHsPatSigType :: UserTypeCtxt
	       -> LHsType Name 		-- The type signature
	       -> TcM ([TyVar], 	-- Newly in-scope type variables
			Type)		-- The signature
-- Used for type-checking type signatures in
-- (a) patterns 	  e.g  f (x::Int) = e
-- (b) result signatures  e.g. g x :: Int = e
-- (c) RULE forall bndrs  e.g. forall (x::Int). f x = x

tcHsPatSigType ctxt hs_ty 
  = addErrCtxt (pprHsSigCtxt ctxt hs_ty) $
    do	{ 	-- Find the type variables that are mentioned in the type
		-- but not already in scope.  These are the ones that
		-- should be bound by the pattern signature
 	  in_scope <- getInLocalScope
	; let span = getLoc hs_ty
	      sig_tvs = [ L span (UserTyVar n) 
			| n <- nameSetToList (extractHsTyVars hs_ty),
			  not (in_scope n) ]

	-- Behave very like type-checking (HsForAllTy sig_tvs hs_ty),
	-- except that we want to keep the tvs separate
	; (kinded_tvs, kinded_ty) <- kcHsTyVars sig_tvs $ \ kinded_tvs -> do
				    { kinded_ty <- kcTypeType hs_ty
				    ; return (kinded_tvs, kinded_ty) }
	; tcTyVarBndrs kinded_tvs $ \ tyvars -> do
	{ sig_ty <- dsHsType kinded_ty
	; checkValidType ctxt sig_ty 
	; return (tyvars, sig_ty)
      } }

tcPatSig :: UserTypeCtxt
	 -> LHsType Name
	 -> BoxySigmaType
	 -> TcM (TcType,	   -- The type to use for "inside" the signature
		 [(Name,TcType)])  -- The new bit of type environment, binding
				   -- the scoped type variables
tcPatSig ctxt sig res_ty
  = do	{ (sig_tvs, sig_ty) <- tcHsPatSigType ctxt sig

	; if null sig_tvs then do {
		-- The type signature binds no type variables, 
		-- and hence is rigid, so use it to zap the res_ty
		  boxyUnify sig_ty res_ty
		; return (sig_ty, [])

	} else do {
		-- Type signature binds at least one scoped type variable
	
		-- A pattern binding cannot bind scoped type variables
		-- The renamer fails with a name-out-of-scope error 
		-- if a pattern binding tries to bind a type variable,
		-- So we just have an ASSERT here
	; let in_pat_bind = case ctxt of
				BindPatSigCtxt -> True
				other	       -> False
	; ASSERT( not in_pat_bind || null sig_tvs ) return ()

	  	-- Check that pat_ty is rigid
	; checkTc (isRigidTy res_ty) (wobblyPatSig sig_tvs)

		-- Now match the pattern signature against res_ty
		-- For convenience, and uniform-looking error messages
		-- we do the matching by allocating meta type variables, 
		-- unifying, and reading out the results.
		-- This is a strictly local operation.
	; box_tvs <- mapM tcInstBoxyTyVar sig_tvs
	; boxyUnify (substTyWith sig_tvs (mkTyVarTys box_tvs) sig_ty) res_ty
	; sig_tv_tys <- mapM readFilledBox box_tvs

		-- Check that each is bound to a distinct type variable,
		-- and one that is not already in scope
	; let tv_binds = map tyVarName sig_tvs `zip` sig_tv_tys
	; binds_in_scope <- getScopedTyVarBinds
	; check binds_in_scope tv_binds
	
		-- Phew!
	; return (res_ty, tv_binds)
	} }
  where
    check in_scope []		 = return ()
    check in_scope ((n,ty):rest) = do { check_one in_scope n ty
				      ; check ((n,ty):in_scope) rest }

    check_one in_scope n ty
	= do { checkTc (tcIsTyVarTy ty) (scopedNonVar n ty)
		-- Must bind to a type variable

	     ; checkTc (null dups) (dupInScope n (head dups) ty)
		-- Must not bind to the same type variable
		-- as some other in-scope type variable

	     ; return () }
	where
	  dups = [n' | (n',ty') <- in_scope, tcEqType ty' ty]
\end{code}


%************************************************************************
%*									*
		Scoped type variables
%*									*
%************************************************************************

\begin{code}
pprHsSigCtxt :: UserTypeCtxt -> LHsType Name -> SDoc
pprHsSigCtxt ctxt hs_ty = vcat [ ptext SLIT("In") <+> pprUserTypeCtxt ctxt <> colon, 
				 nest 2 (pp_sig ctxt) ]
  where
    pp_sig (FunSigCtxt n)  = pp_n_colon n
    pp_sig (ConArgCtxt n)  = pp_n_colon n
    pp_sig (ForSigCtxt n)  = pp_n_colon n
    pp_sig other	   = ppr (unLoc hs_ty)

    pp_n_colon n = ppr n <+> dcolon <+> ppr (unLoc hs_ty)


wobblyPatSig sig_tvs
  = hang (ptext SLIT("A pattern type signature cannot bind scoped type variables") 
		<+> pprQuotedList sig_tvs)
       2 (ptext SLIT("unless the pattern has a rigid type context"))
		
scopedNonVar n ty
  = vcat [sep [ptext SLIT("The scoped type variable") <+> quotes (ppr n),
	       nest 2 (ptext SLIT("is bound to the type") <+> quotes (ppr ty))],
	  nest 2 (ptext SLIT("You can only bind scoped type variables to type variables"))]

dupInScope n n' ty
  = hang (ptext SLIT("The scoped type variables") <+> quotes (ppr n) <+> ptext SLIT("and") <+> quotes (ppr n'))
       2 (vcat [ptext SLIT("are bound to the same type (variable)"),
		ptext SLIT("Distinct scoped type variables must be distinct")])
\end{code}


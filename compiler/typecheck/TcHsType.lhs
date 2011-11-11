%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcHsType (
	tcHsSigType, tcHsSigTypeNC, tcHsDeriv, tcHsVectInst, 
	tcHsInstHead, tcHsQuantifiedType,
	UserTypeCtxt(..), 

		-- Kind checking
	kcHsTyVars, kcHsSigType, kcHsLiftedSigType, 
	kcLHsType, kcCheckLHsType, kcHsContext, kcApps,
        kindGeneralizeKind, kindGeneralizeKinds,

		-- Sort checking
	scDsLHsKind, scDsLHsMaybeKind,

                -- Typechecking kinded types
	tcHsType, tcCheckHsType,
        tcHsKindedContext, tcHsKindedType, tcHsBangType,
	tcTyVarBndrs, tcTyVarBndrsKindGen, dsHsType,
	tcDataKindSig, tcTyClTyVars,

        ExpKind(..), EkCtxt(..), ekConstraint,
        checkExpectedKind,

		-- Pattern type signatures
	tcHsPatSigType, tcPatSig
   ) where

#include "HsVersions.h"

#ifdef GHCI 	/* Only if bootstrapped */
import {-# SOURCE #-}	TcSplice( kcSpliceType )
#endif

import HsSyn
import RnHsSyn
import TcRnMonad
import RnEnv   ( polyKindsErr )
import TcHsSyn ( mkZonkTcTyVar )
import TcEnv
import TcMType
import TcUnify
import TcIface
import TcType
import {- Kind parts of -} Type
import Kind
import Var
import VarSet
import TyCon
import DataCon ( DataCon, dataConUserType )
import TysPrim ( liftedTypeKindTyConName, constraintKindTyConName )
import Class
import RdrName ( rdrNameSpace, nameRdrName )
import Name
import NameSet
import TysWiredIn
import BasicTypes
import SrcLoc
import DynFlags ( ExtensionFlag( Opt_ConstraintKinds, Opt_PolyKinds ) )
import Util
import UniqSupply
import Outputable
import BuildTyCl ( buildPromotedDataTyCon )
import FastString
import Control.Monad ( unless )
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
tcHsSigType, tcHsSigTypeNC :: UserTypeCtxt -> LHsType Name -> TcM Type
  -- Do kind checking, and hoist for-alls to the top
  -- NB: it's important that the foralls that come from the top-level
  --	 HsForAllTy in hs_ty occur *first* in the returned type.
  --     See Note [Scoped] with TcSigInfo
tcHsSigType ctxt hs_ty 
  = addErrCtxt (pprHsSigCtxt ctxt hs_ty) $
    tcHsSigTypeNC ctxt hs_ty

tcHsSigTypeNC ctxt hs_ty
  = do	{ (kinded_ty, _kind) <- kc_lhs_type hs_ty
    	  -- The kind is checked by checkValidType, and isn't necessarily
	  -- of kind * in a Template Haskell quote eg [t| Maybe |]
	; ty <- tcHsKindedType kinded_ty
	; checkValidType ctxt ty
	; return ty }

-- Like tcHsType, but takes an expected kind
tcCheckHsType :: LHsType Name -> Kind -> TcM Type
tcCheckHsType hs_ty exp_kind
  = do { kinded_ty <- kcCheckLHsType hs_ty (EK exp_kind EkUnk) -- JPM add context
       ; ty <- tcHsKindedType kinded_ty
       ; return ty }

tcHsType :: LHsType Name -> TcM Type
-- kind check and desugar
-- no validity checking because of knot-tying
tcHsType hs_ty
  = do { (kinded_ty, _) <- kc_lhs_type hs_ty
       ; ty <- tcHsKindedType kinded_ty
       ; return ty }

tcHsInstHead :: UserTypeCtxt -> LHsType Name -> TcM ([TyVar], ThetaType, Class, [Type])
-- Typecheck an instance head.  We can't use 
-- tcHsSigType, because it's not a valid user type.
tcHsInstHead ctxt lhs_ty@(L loc hs_ty)
  = setSrcSpan loc   $	-- No need for an "In the type..." context
                        -- because that comes from the caller
    do { kinded_ty <- kc_check_hs_type hs_ty ekConstraint
       ; ty <- ds_type kinded_ty
       ; let (tvs, theta, tau) = tcSplitSigmaTy ty
       ; case getClassPredTys_maybe tau of
           Nothing          -> failWithTc (ptext (sLit "Malformed instance type"))
           Just (clas,tys)  -> do { checkValidInstance ctxt lhs_ty tvs theta clas tys
                                  ; return (tvs, theta, clas, tys) } }

tcHsQuantifiedType :: [LHsTyVarBndr Name] -> LHsType Name -> TcM ([TyVar], Type)
-- Behave very like type-checking (HsForAllTy sig_tvs hs_ty),
-- except that we want to keep the tvs separate
tcHsQuantifiedType tv_names hs_ty
  = kcHsTyVars tv_names $ \ tv_names' ->
    do	{ kc_ty <- kcHsSigType hs_ty
    	; tcTyVarBndrs tv_names' $ \ tvs ->
    do	{ ty <- dsHsType kc_ty
    	; return (tvs, ty) } }

-- Used for the deriving(...) items
tcHsDeriv :: HsType Name -> TcM ([TyVar], Class, [Type])
tcHsDeriv = tc_hs_deriv []

tc_hs_deriv :: [LHsTyVarBndr Name] -> HsType Name
            -> TcM ([TyVar], Class, [Type])
tc_hs_deriv tv_names1 (HsForAllTy _ tv_names2 (L _ []) (L _ ty))
  = 	-- Funny newtype deriving form
	-- 	forall a. C [a]
	-- where C has arity 2.  Hence can't use regular functions
    tc_hs_deriv (tv_names1 ++ tv_names2) ty

tc_hs_deriv tv_names ty
  | Just (cls_name, hs_tys) <- splitHsClassTy_maybe ty
  = kcHsTyVars tv_names                 $ \ tv_names' ->
    do  { cls_kind <- kcClass cls_name
        ; (tys, _res_kind) <- kcApps cls_name cls_kind hs_tys
        ; tcTyVarBndrsKindGen tv_names'        $ \ tyvars ->
    do  { arg_tys <- dsHsTypes tys
        ; cls <- tcLookupClass cls_name
        ; return (tyvars, cls, arg_tys) }}

  | otherwise
  = failWithTc (ptext (sLit "Illegal deriving item") <+> ppr ty)

-- Used for 'VECTORISE [SCALAR] instance' declarations
--
tcHsVectInst :: LHsType Name -> TcM (Class, [Type])
tcHsVectInst ty
  | Just (L _ cls_name, tys) <- splitLHsClassTy_maybe ty
  = do { cls_kind <- kcClass cls_name
       ; (tys, _res_kind) <- kcApps cls_name cls_kind tys
       ; arg_tys <- dsHsTypes tys
       ; cls <- tcLookupClass cls_name
       ; return (cls, arg_tys)
       }
  | otherwise
  = failWithTc $ ptext (sLit "Malformed instance type")
\end{code}

	These functions are used during knot-tying in
	type and class declarations, when we have to
 	separate kind-checking, desugaring, and validity checking

\begin{code}
kcHsSigType, kcHsLiftedSigType :: LHsType Name -> TcM (LHsType Name)
	-- Used for type signatures
kcHsSigType ty 	     = addKcTypeCtxt ty $ kcArgType ty
kcHsLiftedSigType ty = addKcTypeCtxt ty $ kcLiftedType ty

tcHsKindedType :: LHsType Name -> TcM Type
  -- Don't do kind checking, nor validity checking.
  -- This is used in type and class decls, where kinding is
  -- done in advance, and validity checking is done later
  -- [Validity checking done later because of knot-tying issues.]
tcHsKindedType hs_ty = dsHsType hs_ty

tcHsBangType :: LHsType Name -> TcM Type
-- Permit a bang, but discard it
-- Input type has already been kind-checked
tcHsBangType (L _ (HsBangTy _ ty)) = tcHsKindedType ty
tcHsBangType ty                    = tcHsKindedType ty

tcHsKindedContext :: LHsContext Name -> TcM ThetaType
-- Used when we are expecting a ClassContext (i.e. no implicit params)
-- Does not do validity checking, like tcHsKindedType
tcHsKindedContext hs_theta = addLocM (mapM dsHsType) hs_theta
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
kcLiftedType ty = kc_check_lhs_type ty ekLifted
    
---------------------------
kcTypeType :: LHsType Name -> TcM (LHsType Name)
-- The type ty must be a *type*, but it can be lifted or
-- unlifted or an unboxed tuple.
kcTypeType ty = kc_check_lhs_type ty ekOpen

kcArgs :: SDoc -> [LHsType Name] -> Kind -> TcM [LHsType Name]
kcArgs what tys kind 
  = sequence [ kc_check_lhs_type ty (EK kind (EkArg what n)) 
             | (ty,n) <- tys `zip` [1..] ]

---------------------------
kcArgType :: LHsType Name -> TcM (LHsType Name)
-- The type ty must be an *arg* *type* (lifted or unlifted)
kcArgType ty = kc_check_lhs_type ty ekArg

---------------------------
kcCheckLHsType :: LHsType Name -> ExpKind -> TcM (LHsType Name)
kcCheckLHsType ty kind = addKcTypeCtxt ty $ kc_check_lhs_type ty kind


kc_check_lhs_type :: LHsType Name -> ExpKind -> TcM (LHsType Name)
-- Check that the type has the specified kind
-- Be sure to use checkExpectedKind, rather than simply unifying 
-- with OpenTypeKind, because it gives better error messages
kc_check_lhs_type (L span ty) exp_kind 
  = setSrcSpan span $
    do { ty' <- kc_check_hs_type ty exp_kind
       ; return (L span ty') }

kc_check_lhs_types :: [(LHsType Name, ExpKind)] -> TcM [LHsType Name]
kc_check_lhs_types tys_w_kinds
  = mapM kc_arg tys_w_kinds
  where
    kc_arg (arg, arg_kind) = kc_check_lhs_type arg arg_kind


---------------------------
kc_check_hs_type :: HsType Name -> ExpKind -> TcM (HsType Name)

-- First some special cases for better error messages 
-- when we know the expected kind
kc_check_hs_type (HsParTy ty) exp_kind
  = do { ty' <- kc_check_lhs_type ty exp_kind; return (HsParTy ty') }

kc_check_hs_type ty@(HsAppTy ty1 ty2) exp_kind
  = do { let (fun_ty, arg_tys) = splitHsAppTys ty1 [ty2]
       ; (fun_ty', fun_kind) <- kc_lhs_type fun_ty
       ; arg_tys' <- kcCheckApps fun_ty fun_kind arg_tys ty exp_kind
       ; return (mkHsAppTys fun_ty' arg_tys') }

-- This is the general case: infer the kind and compare
kc_check_hs_type ty exp_kind
  = do	{ traceTc "kc_check_hs_type" (ppr ty)
        ; (ty', act_kind) <- kc_hs_type ty
		-- Add the context round the inner check only
		-- because checkExpectedKind already mentions
		-- 'ty' by name in any error message

	; checkExpectedKind (strip ty) act_kind exp_kind
	; return ty' }
  where
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
kcLHsType :: LHsType Name -> TcM (LHsType Name, TcKind)
-- Called from outside: set the context
kcLHsType ty = addKcTypeCtxt ty (kc_lhs_type ty)

kc_lhs_type :: LHsType Name -> TcM (LHsType Name, TcKind)
kc_lhs_type (L span ty)
  = setSrcSpan span $
    do { traceTc "kc_lhs_type" (ppr ty)
       ; (ty', kind) <- kc_hs_type ty
       ; return (L span ty', kind) }

-- kc_hs_type *returns* the kind of the type, rather than taking an expected
-- kind as argument as tcExpr does.  
-- Reasons: 
--	(a) the kind of (->) is
--		forall bx1 bx2. Type bx1 -> Type bx2 -> Type Boxed
--  	    so we'd need to generate huge numbers of bx variables.
--	(b) kinds are so simple that the error messages are fine
--
-- The translated type has explicitly-kinded type-variable binders

kc_hs_type :: HsType Name -> TcM (HsType Name, TcKind)
kc_hs_type (HsParTy ty) = do
   (ty', kind) <- kc_lhs_type ty
   return (HsParTy ty', kind)

kc_hs_type (HsTyVar name)
  -- Special case for the unit tycon so it benefits from kind overloading
  | name == tyConName unitTyCon
  = kc_hs_type (HsTupleTy (HsBoxyTuple placeHolderKind) [])
  | otherwise = kcTyVar name

kc_hs_type (HsListTy ty) = do
    ty' <- kcLiftedType ty
    return (HsListTy ty', liftedTypeKind)

kc_hs_type (HsPArrTy ty) = do
    ty' <- kcLiftedType ty
    return (HsPArrTy ty', liftedTypeKind)

kc_hs_type (HsKindSig ty k) = do
    k' <- scDsLHsKind k
    ty' <- kc_check_lhs_type ty (EK k' EkKindSig)
    return (HsKindSig ty' k, k')

kc_hs_type (HsTupleTy (HsBoxyTuple _) tys)
  = do { fact_tup_ok <- xoptM Opt_ConstraintKinds
       ; k <- if fact_tup_ok
              then newMetaKindVar
              else return liftedTypeKind
       ; tys' <- kcArgs (ptext (sLit "a tuple")) tys k
       ; return (HsTupleTy (HsBoxyTuple k) tys', k) }
             -- In some contexts users really "mean" to write
             -- tuples with Constraint components, rather than * components.
             --
             -- This special case of kind-checking does this rewriting 
             -- when we can detect that we need it.

kc_hs_type (HsTupleTy HsUnboxedTuple tys)
  = do { tys' <- kcArgs (ptext (sLit "an unboxed tuple")) tys argTypeKind
       ; return (HsTupleTy HsUnboxedTuple tys', ubxTupleKind) }

kc_hs_type (HsFunTy ty1 ty2) = do
    ty1' <- kc_check_lhs_type ty1 (EK argTypeKind EkUnk)
    ty2' <- kcTypeType ty2
    return (HsFunTy ty1' ty2', liftedTypeKind)

kc_hs_type (HsOpTy ty1 (_, l_op@(L loc op)) ty2) = do
    (wop, op_kind) <- kcTyVar op
    ([ty1',ty2'], res_kind) <- kcApps l_op op_kind [ty1,ty2]
    let op' = case wop of
                HsTyVar name -> (WpKiApps [], L loc name)
                HsWrapTy wrap (HsTyVar name) -> (wrap, L loc name)
                _ -> panic "kc_hs_type HsOpTy"
    return (HsOpTy ty1' op' ty2', res_kind)

kc_hs_type (HsAppTy ty1 ty2) = do
    let (fun_ty, arg_tys) = splitHsAppTys ty1 [ty2]
    (fun_ty', fun_kind) <- kc_lhs_type fun_ty
    (arg_tys', res_kind) <- kcApps fun_ty fun_kind arg_tys
    return (mkHsAppTys fun_ty' arg_tys', res_kind)

kc_hs_type (HsIParamTy n ty) = do
    ty' <- kc_check_lhs_type ty (EK liftedTypeKind EkIParam)
    return (HsIParamTy n ty', constraintKind)

kc_hs_type (HsEqTy ty1 ty2) = do
    (ty1', kind1) <- kc_lhs_type ty1
    (ty2', kind2) <- kc_lhs_type ty2
    checkExpectedKind ty2 kind2 (EK kind1 EkEqPred)
    return (HsEqTy ty1' ty2', constraintKind)

kc_hs_type (HsCoreTy ty)
  = return (HsCoreTy ty, typeKind ty)

kc_hs_type (HsForAllTy exp tv_names context ty)
  = kcHsTyVars tv_names         $ \ tv_names' ->
    do	{ ctxt' <- kcHsContext context
	; (ty', k)  <- kc_lhs_type ty
	     -- The body of a forall is usually a type, but in principle
	     -- there's no reason to prohibit *unlifted* types.
	     -- In fact, GHC can itself construct a function with an
	     -- unboxed tuple inside a for-all (via CPR analyis; see 
	     -- typecheck/should_compile/tc170).
             --
             -- Moreover in instance heads we get forall-types with
             -- kind Constraint.  
	     --
	     -- Really we should check that it's a type of value kind
             -- {*, Constraint, #}, but I'm not doing that yet
             -- Example that should be rejected:  
             --          f :: (forall (a:*->*). a) Int

  	; return (HsForAllTy exp tv_names' ctxt' ty', k) }

kc_hs_type (HsBangTy b ty)
  = do { (ty', kind) <- kc_lhs_type ty
       ; return (HsBangTy b ty', kind) }

kc_hs_type ty@(HsRecTy _)
  = failWithTc (ptext (sLit "Unexpected record type") <+> ppr ty)
      -- Record types (which only show up temporarily in constructor signatures) 
      -- should have been removed by now

#ifdef GHCI	/* Only if bootstrapped */
kc_hs_type (HsSpliceTy sp fvs _) = kcSpliceType sp fvs
#else
kc_hs_type ty@(HsSpliceTy {}) = failWithTc (ptext (sLit "Unexpected type splice:") <+> ppr ty)
#endif

kc_hs_type (HsQuasiQuoteTy {}) = panic "kc_hs_type"	-- Eliminated by renamer

-- remove the doc nodes here, no need to worry about the location since
-- its the same for a doc node and it's child type node
kc_hs_type (HsDocTy ty _)
  = kc_hs_type (unLoc ty) 

kc_hs_type (HsExplicitListTy _ tys) 
  = do { ty_k_s <- mapM kc_lhs_type tys
       ; kind <- unifyKinds (ptext (sLit "In a promoted list")) ty_k_s
       ; return (HsExplicitListTy kind (map fst ty_k_s), mkListTy kind) }
kc_hs_type (HsExplicitTupleTy _ tys) = do
  ty_k_s <- mapM kc_lhs_type tys
  return ( HsExplicitTupleTy (map snd ty_k_s) (map fst ty_k_s)
         , mkTyConApp (tupleTyCon BoxedTuple (length tys)) (map snd ty_k_s))

kc_hs_type (HsWrapTy {}) = panic "kc_hs_type HsWrapTy"  -- it means we kind checked something twice

---------------------------
kcApps :: Outputable a
       => a 
       -> TcKind			-- Function kind
       -> [LHsType Name]		-- Arg types
       -> TcM ([LHsType Name], TcKind)	-- Kind-checked args
kcApps the_fun fun_kind args
  = do { (args_w_kinds, res_kind) <- splitFunKind (ppr the_fun) 1 fun_kind args
       ; args' <- kc_check_lhs_types args_w_kinds
       ; return (args', res_kind) }

kcCheckApps :: Outputable a => a -> TcKind -> [LHsType Name]
	    -> HsType Name     -- The type being checked (for err messages only)
	    -> ExpKind 	       -- Expected kind
	    -> TcM [LHsType Name]
kcCheckApps the_fun fun_kind args ty exp_kind
  = do { (args_w_kinds, res_kind) <- splitFunKind (ppr the_fun) 1 fun_kind args
       ; checkExpectedKind ty res_kind exp_kind
       	     -- Check the result kind *before* checking argument kinds
	     -- This improves error message; Trac #2994
       ; kc_check_lhs_types args_w_kinds }


---------------------------
splitFunKind :: SDoc -> Int -> TcKind -> [b] -> TcM ([(b,ExpKind)], TcKind)
splitFunKind _       _      fk [] = return ([], fk)
splitFunKind the_fun arg_no fk (arg:args)
  = do { mb_fk <- matchExpectedFunKind fk
       ; case mb_fk of
            Nothing       -> failWithTc too_many_args 
            Just (ak,fk') -> do { (aks, rk) <- splitFunKind the_fun (arg_no+1) fk' args
                                ; return ((arg, EK ak (EkArg (quotes the_fun) arg_no)):aks, rk) } }
  where
    too_many_args = quotes the_fun <+>
		    ptext (sLit "is applied to too many type arguments")

---------------------------
kcHsContext :: LHsContext Name -> TcM (LHsContext Name)
kcHsContext ctxt = wrapLocM (mapM kcHsLPredType) ctxt

kcHsLPredType :: LHsType Name -> TcM (LHsType Name)
kcHsLPredType pred = kc_check_lhs_type pred ekConstraint

---------------------------
kcTyVar :: Name -> TcM (HsType Name, TcKind)
-- See Note [Type checking recursive type and class declarations]
-- in TcTyClsDecls
kcTyVar name         -- Could be a tyvar, a tycon, or a datacon
  = do { traceTc "lk1" (ppr name)
       ; thing <- tcLookup name
       ; traceTc "lk2" (ppr name <+> ppr thing)
       ; case thing of
           ATyVar _ ty           -> wrap_mono (typeKind ty)
           AThing kind           -> wrap_poly kind
           AGlobal (ATyCon tc)   -> wrap_poly (tyConKind tc)
           AGlobal (ADataCon dc) -> kcDataCon dc >>= wrap_poly
           _                     -> wrongThingErr "type" thing name }
  where
    wrap_mono kind = do { traceTc "lk3" (ppr name <+> dcolon <+> ppr kind)
                        ; return (HsTyVar name, kind) }
    wrap_poly kind
      | null kvs = wrap_mono kind
      | otherwise
      = do { traceTc "lk4" (ppr name <+> dcolon <+> ppr kind)
           ; kvs' <- mapM (const newMetaKindVar) kvs
           ; let ki = substKiWith kvs kvs' ki_body
           ; return (HsWrapTy (WpKiApps kvs') (HsTyVar name), ki) }
      where (kvs, ki_body) = splitForAllTys kind

-- IA0_TODO: this function should disapear, and use the dcPromoted field of DataCon
kcDataCon :: DataCon -> TcM TcKind
kcDataCon dc = do
  let ty = dataConUserType dc
  unless (isPromotableType ty) $ promoteErr dc ty
  let ki = promoteType ty
  traceTc "prm" (ppr ty <+> ptext (sLit "~~>") <+> ppr ki)
  return ki
  where
    promoteErr dc ty = failWithTc (quotes (ppr dc) <+> ptext (sLit "of type")
      <+> quotes (ppr ty) <+> ptext (sLit "is not promotable"))

kcClass :: Name -> TcM TcKind
kcClass cls = do	-- Must be a class
    thing <- tcLookup cls
    case thing of
        AThing kind                         -> return kind
        AGlobal (ATyCon tc)
          | Just cls <- tyConClass_maybe tc -> return (tyConKind (classTyCon cls))
        _                                   -> wrongThingErr "class" thing cls
\end{code}


%************************************************************************
%*									*
		Desugaring
%*									*
%************************************************************************

Note [Desugaring types]
~~~~~~~~~~~~~~~~~~~~~~~
The type desugarer is phase 2 of dealing with HsTypes.  Specifically:

  * It transforms from HsType to Type

  * It zonks any kinds.  The returned type should have no mutable kind
    or type variables (hence returning Type not TcType):
      - any unconstrained kind variables are defaulted to AnyK just 
        as in TcHsSyn. 
      - there are no mutable type variables because we are 
        kind-checking a type
    Reason: the returned type may be put in a TyCon or DataCon where
    it will never subsequently be zonked.

You might worry about nested scopes:
        ..a:kappa in scope..
            let f :: forall b. T '[a,b] -> Int
In this case, f's type could have a mutable kind variable kappa in it;
and we might then default it to AnyK when dealing with f's type
signature.  But we don't expect this to happen because we can't get a
lexically scoped type variable with a mutable kind variable in it.  A
delicate point, this.  If it becomes an issue we might need to
distinguish top-level from nested uses.

Moreover
  * it cannot fail, 
  * it does no unifications
  * it does no validity checking, except for structural matters, such as
	(a) spurious ! annotations.
	(b) a class used as a type

\begin{code}

zonkTcKindToKind :: TcKind -> TcM Kind
-- When zonking a TcKind to a kind we instantiate kind variables to AnyK
zonkTcKindToKind = zonkType (mkZonkTcTyVar (\ _ -> return anyKind) mkTyVarTy)

dsHsType :: LHsType Name -> TcM Type
-- All HsTyVarBndrs in the intput type are kind-annotated
-- See Note [Desugaring types]
dsHsType ty = ds_type (unLoc ty)

ds_type :: HsType Name -> TcM Type
-- See Note [Desugaring types]
ds_type ty@(HsTyVar _)
  = ds_app ty []

ds_type (HsParTy ty)		-- Remove the parentheses markers
  = dsHsType ty

ds_type ty@(HsBangTy {})    -- No bangs should be here
  = failWithTc (ptext (sLit "Unexpected strictness annotation:") <+> ppr ty)

ds_type ty@(HsRecTy {})	    -- No bangs should be here
  = failWithTc (ptext (sLit "Unexpected record type:") <+> ppr ty)

ds_type (HsKindSig ty _)
  = dsHsType ty	-- Kind checking done already

ds_type (HsListTy ty) = do
    tau_ty <- dsHsType ty
    checkWiredInTyCon listTyCon
    return (mkListTy tau_ty)

ds_type (HsPArrTy ty) = do
    tau_ty <- dsHsType ty
    checkWiredInTyCon parrTyCon
    return (mkPArrTy tau_ty)

ds_type (HsTupleTy hs_con tys) = do
    con <- case hs_con of
        HsUnboxedTuple -> return UnboxedTuple
        HsBoxyTuple kind -> do
          -- Here we use zonkTcKind instead of zonkTcKindToKind because pairs
          -- are a special case: we use them both for types (eg. (Int, Bool))
          -- and for constraints (eg. (Show a, Eq a))
          kind' <- zonkTcKind kind
          case () of
            _ | kind' `eqKind` constraintKind -> return ConstraintTuple
            _ | kind' `eqKind` liftedTypeKind -> return BoxedTuple
            _ | otherwise
              -> failWithTc (ptext (sLit "Unexpected tuple component kind:") <+> ppr kind')
    let tycon = tupleTyCon con (length tys)
    tau_tys <- dsHsTypes tys
    checkWiredInTyCon tycon
    return (mkTyConApp tycon tau_tys)

ds_type (HsFunTy ty1 ty2) = do
    tau_ty1 <- dsHsType ty1
    tau_ty2 <- dsHsType ty2
    return (mkFunTy tau_ty1 tau_ty2)

ds_type (HsOpTy ty1 (wrap, (L span op)) ty2) =
    setSrcSpan span (ds_app (HsWrapTy wrap (HsTyVar op)) [ty1,ty2])

ds_type ty@(HsAppTy _ _)
  = ds_app ty []

ds_type (HsIParamTy n ty) = do
    tau_ty <- dsHsType ty
    return (mkIPPred n tau_ty)

ds_type (HsEqTy ty1 ty2) = do
    tau_ty1 <- dsHsType ty1
    tau_ty2 <- dsHsType ty2
    return (mkEqPred (tau_ty1, tau_ty2))

ds_type (HsForAllTy _ tv_names ctxt ty)
  = tcTyVarBndrsKindGen tv_names $ \ tyvars -> do
    theta <- mapM dsHsType (unLoc ctxt)
    tau <- dsHsType ty
    return (mkSigmaTy tyvars theta tau)

ds_type (HsDocTy ty _)  -- Remove the doc comment
  = dsHsType ty

ds_type (HsSpliceTy _ _ kind) 
  = do { kind' <- zonkType (mkZonkTcTyVar (\ _ -> return liftedTypeKind) mkTyVarTy) 
                           kind
                     -- See Note [Kind of a type splice]
       ; newFlexiTyVarTy kind' }

ds_type (HsQuasiQuoteTy {}) = panic "ds_type"	-- Eliminated by renamer
ds_type (HsCoreTy ty)       = return ty

ds_type (HsExplicitListTy kind tys) = do
  kind' <- zonkTcKindToKind kind
  ds_tys <- mapM dsHsType tys
  return $
   foldr (\a b -> mkTyConApp (buildPromotedDataTyCon consDataCon) [kind', a, b])
         (mkTyConApp (buildPromotedDataTyCon nilDataCon) [kind']) ds_tys

ds_type (HsExplicitTupleTy kis tys) = do
  MASSERT( length kis == length tys )
  kis' <- mapM zonkTcKindToKind kis
  tys' <- mapM dsHsType tys
  return $ mkTyConApp (buildPromotedDataTyCon (tupleCon BoxedTuple (length kis'))) (kis' ++ tys')

ds_type (HsWrapTy (WpKiApps kappas) ty) = do
  tau <- ds_type ty
  kappas' <- mapM zonkTcKindToKind kappas
  return (mkAppTys tau kappas')

dsHsTypes :: [LHsType Name] -> TcM [Type]
dsHsTypes arg_tys = mapM dsHsType arg_tys
\end{code}

Note [Kind of a type splice]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider these terms, each with TH type splice inside:
     [| e1 :: Maybe $(..blah..) |]
     [| e2 :: $(..blah..) |]
When kind-checking the type signature, we'll kind-check the splice
$(..blah..); we want to give it a kind that can fit in any context,
as if $(..blah..) :: forall k. k.  

In the e1 example, the context of the splice fixes kappa to *.  But
in the e2 example, we'll desugar the type, zonking the kind unification
variables as we go.  When we encournter the unconstrained kappa, we
want to default it to '*', not to AnyK.


Help functions for type applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
ds_app :: HsType Name -> [LHsType Name] -> TcM Type
ds_app (HsAppTy ty1 ty2) tys
  = ds_app (unLoc ty1) (ty2:tys)

ds_app ty tys = do
    arg_tys <- dsHsTypes tys
    case ty of
	HsTyVar fun -> ds_var_app fun arg_tys
	_           -> do fun_ty <- ds_type ty
                          return (mkAppTys fun_ty arg_tys)

ds_var_app :: Name -> [Type] -> TcM Type
-- See Note [Type checking recursive type and class declarations]
-- in TcTyClsDecls
ds_var_app name arg_tys 
  | isTvNameSpace (rdrNameSpace (nameRdrName name))
  = do { thing <- tcLookup name
       ; case thing of
           ATyVar _ ty -> return (mkAppTys ty arg_tys)
	   _           -> wrongThingErr "type" thing name }

  | otherwise
  = do { thing <- tcLookupGlobal name
       ; case thing of
           ATyCon tc   -> return (mkTyConApp tc arg_tys)
           ADataCon dc -> return (mkTyConApp (buildPromotedDataTyCon dc) arg_tys) 
	   _           -> wrongThingErr "type" (AGlobal thing) name }

addKcTypeCtxt :: LHsType Name -> TcM a -> TcM a
	-- Wrap a context around only if we want to show that contexts.  
	-- Omit invisble ones and ones user's won't grok
addKcTypeCtxt (L _ other_ty) thing = addErrCtxt (typeCtxt other_ty) thing

typeCtxt :: HsType Name -> SDoc
typeCtxt ty = ptext (sLit "In the type") <+> quotes (ppr ty)
\end{code}

%************************************************************************
%*									*
		Type-variable binders
%*									*
%************************************************************************

Note [Kind-checking kind-polymorphic types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:
  f :: forall (f::k -> *) a. f a -> Int

Here, the [LHsTyVarBndr Name] of the forall type will be [f,a], where
  a is a  UserTyVar   -> type variable without kind annotation
  f is a  KindedTyVar -> type variable with kind annotation

If were were to allow binding sites for kind variables, thus
  f :: forall @k (f :: k -> *) a. f a -> Int
then we'd also need
  k is a   UserKiVar   -> kind variable (they don't need annotation,
                          since we only have BOX for a super kind)

\begin{code}
kcHsTyVars :: [LHsTyVarBndr Name] 
	   -> ([LHsTyVarBndr Name] -> TcM r) 	-- These binders are kind-annotated
						-- They scope over the thing inside
	   -> TcM r
kcHsTyVars tvs thing_inside
  = do { kinded_tvs <- mapM (wrapLocM kcHsTyVar) tvs
       ; tcExtendKindEnvTvs kinded_tvs thing_inside }

kcHsTyVar :: HsTyVarBndr Name -> TcM (HsTyVarBndr Name)
-- Return a *kind-annotated* binder, whose PostTcKind is
-- initialised with a kind variable.
-- Typically the Kind inside the KindedTyVar will be a tyvar with a mutable kind 
-- in it. We aren't yet sure whether the binder is a *type* variable or a *kind*
-- variable. See Note [Kind-checking kind-polymorphic types]
--
-- If the variable is already in scope return it, instead of introducing a new
-- one. This can occur in 
--   instance C (a,b) where
--     type F (a,b) c = ...
-- Here a,b will be in scope when processing the associated type instance for F.
kcHsTyVar tyvar = do in_scope <- getInLocalScope
                     if in_scope (hsTyVarName tyvar)
                      then do inscope_tyvar <- tcLookupTyVar (hsTyVarName tyvar)
                              return (UserTyVar (tyVarName inscope_tyvar)
                                (tyVarKind inscope_tyvar)) 
                       else kcHsTyVar' tyvar
    where
        kcHsTyVar' (UserTyVar name _)        = UserTyVar name <$> newMetaKindVar
        kcHsTyVar' (KindedTyVar name kind _) = do
          kind' <- scDsLHsKind kind
          return (KindedTyVar name kind kind')

------------------
tcTyVarBndrs :: [LHsTyVarBndr Name] -- Kind-annotated binders, which need kind-zonking
             -> ([TyVar] -> TcM r)
             -> TcM r
-- Used when type-checking types/classes/type-decls
-- Brings into scope immutable TyVars, not mutable ones that require later zonking
-- Fix #5426: avoid abstraction over kinds containing # or (#)
tcTyVarBndrs bndrs thing_inside = do
    tyvars <- mapM (zonk . hsTyVarNameKind . unLoc) bndrs
    tcExtendTyVarEnv tyvars (thing_inside tyvars)
  where
    zonk (name, kind)
      = do { kind' <- zonkTcKind kind
           ; checkTc (noHashInKind kind') (ptext (sLit "Kind signature contains # or (#)"))
           ; return (mkTyVar name kind') }

tcTyVarBndrsKindGen :: [LHsTyVarBndr Name] -> ([TyVar] -> TcM r) -> TcM r
-- tcTyVarBndrsKindGen [(f :: ?k -> *), (a :: ?k)] thing_inside
-- calls thing_inside with [(k :: BOX), (f :: k -> *), (a :: k)]
tcTyVarBndrsKindGen bndrs thing_inside
  = do { let kinds = map (hsTyVarKind . unLoc) bndrs
       ; (kvs, zonked_kinds) <- kindGeneralizeKinds kinds
       ; let tyvars = zipWith mkTyVar (map hsLTyVarName bndrs) zonked_kinds
             ktvs = kvs ++ tyvars     -- See Note [Kinds of quantified type variables]
       ; traceTc "tcTyVarBndrsKindGen" (ppr (bndrs, kvs, tyvars))
       ; tcExtendTyVarEnv ktvs (thing_inside ktvs) }
\end{code}

Note [Kinds of quantified type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcTyVarBndrsKindGen quantifies over a specified list of type variables,
*and* over the kind variables mentioned in the kinds of those tyvars.

Note that we must zonk those kinds (obviously) but less obviously, we
must return type variables whose kinds are zonked too. Example
    (a :: k7)  where  k7 := k9 -> k9
We must return
    [k9, a:k9->k9]
and NOT 
    [k9, a:k7]
Reason: we're going to turn this into a for-all type, 
   forall k9. forall (a:k7). blah
which the type checker will then instantiate, and instantiate does not
look through unification variables!  

Hence using zonked_kinds when forming 'tyvars'.

\begin{code}
tcTyClTyVars :: Name -> [LHsTyVarBndr Name]	-- LHS of the type or class decl
             -> ([TyVar] -> Kind -> TcM a) -> TcM a
-- tcTyClTyVars T [a,b] calls thing_inside with
-- [k1,k2,a,b] (k2 -> *)  where T : forall k1 k2 (a:k1 -> *) (b:k1). k2 -> *
--
-- No need to freshen the k's because they are just skolem 
-- constants here, and we are at top level anyway.
tcTyClTyVars tycon tyvars thing_inside
  = do { thing <- tcLookup tycon
       ; let { kind =
                 case thing of
                   AThing kind -> kind
                   _ -> panic "tcTyClTyVars"
                     -- We only call tcTyClTyVars during typechecking in
                     -- TcTyClDecls, where the local env is extended with
                     -- the generalized_env (mapping Names to AThings).
             ; (kvs, body) = splitForAllTys kind
             ; (kinds, res) = splitKindFunTysN (length names) body
             ; names = hsLTyVarNames tyvars
             ; tvs = zipWith mkTyVar names kinds
             ; all_vs = kvs ++ tvs }
       ; tcExtendTyVarEnv all_vs (thing_inside all_vs res) }

-- Used when generalizing binders and type family patterns
-- It takes a kind from the type checker (like `k0 -> *`), and returns the 
-- final, kind-generalized kind (`forall k::BOX. k -> *`)
kindGeneralizeKinds :: [TcKind] -> TcM ([KindVar], [Kind])
-- INVARIANT: the returned kinds are zonked, and
--            mention the returned kind variables
kindGeneralizeKinds kinds 
  = do { -- Quantify over kind variables free in
         -- the kinds, and *not* in the environment
       ; zonked_kinds <- mapM zonkTcKind kinds
       ; gbl_tvs <- tcGetGlobalTyVars -- Already zonked
       ; let kvs_to_quantify = tyVarsOfTypes zonked_kinds 
                               `minusVarSet` gbl_tvs

       ; kvs <- ASSERT2 (all isKiVar (varSetElems kvs_to_quantify), ppr kvs_to_quantify)
                zonkQuantifiedTyVars kvs_to_quantify

         -- Zonk the kinds again, to pick up either the kind 
         -- variables we quantify over, or *, depending on whether
         -- zonkQuantifiedTyVars decided to generalise (which in
         -- turn depends on PolyKinds)
       ; final_kinds <- mapM zonkTcKind zonked_kinds

       ; traceTc "generalizeKind" (    ppr kinds <+> ppr kvs_to_quantify
                                   <+> ppr kvs   <+> ppr final_kinds)
       ; return (kvs, final_kinds) }

kindGeneralizeKind :: TcKind -> TcM ( [KindVar]  -- these were flexi kind vars
                                    , Kind )     -- this is the old kind where flexis got zonked
kindGeneralizeKind kind = do
  (kvs, [kind']) <- kindGeneralizeKinds [kind]
  return (kvs, kind')

-----------------------------------
tcDataKindSig :: Kind -> TcM [TyVar]
-- GADT decls can have a (perhaps partial) kind signature
--	e.g.  data T :: * -> * -> * where ...
-- This function makes up suitable (kinded) type variables for 
-- the argument kinds, and checks that the result kind is indeed *.
-- We use it also to make up argument type variables for for data instances.
tcDataKindSig kind
  = do	{ checkTc (isLiftedTypeKind res_kind) (badKindSig kind)
	; span <- getSrcSpanM
	; us   <- newUniqueSupply 
	; let uniqs = uniqsFromSupply us
	; return [ mk_tv span uniq str kind 
		 | ((kind, str), uniq) <- arg_kinds `zip` dnames `zip` uniqs ] }
  where
    (arg_kinds, res_kind) = splitKindFunTys kind
    mk_tv loc uniq str kind = mkTyVar name kind
	where
	   name = mkInternalName uniq occ loc
	   occ  = mkOccName tvName str
	  
    dnames = map ('$' :) names	-- Note [Avoid name clashes for associated data types]

    names :: [String]
    names = [ c:cs | cs <- "" : names, c <- ['a'..'z'] ] 

badKindSig :: Kind -> SDoc
badKindSig kind 
 = hang (ptext (sLit "Kind signature on data type declaration has non-* return kind"))
	2 (ppr kind)
\end{code}

Note [Avoid name clashes for associated data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider    class C a b where
               data D b :: * -> *
When typechecking the decl for D, we'll invent an extra type variable for D,
to fill out its kind.  We *don't* want this type variable to be 'a', because
in an .hi file we'd get
            class C a b where
               data D b a 
which makes it look as if there are *two* type indices.  But there aren't!
So we use $a instead, which cannot clash with a user-written type variable.
Remember that type variable binders in interface files are just FastStrings,
not proper Names.

(The tidying phase can't help here because we don't tidy TyCons.  Another
alternative would be to record the number of indexing parameters in the 
interface file.)


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
	      sig_tvs = userHsTyVarBndrs $ map (L span) $ 
			filterOut in_scope $
                        nameSetToList (extractHsTyVars hs_ty)

	; (tyvars, sig_ty) <- tcHsQuantifiedType sig_tvs hs_ty
	; checkValidType ctxt sig_ty 
	; return (tyvars, sig_ty)
      }

tcPatSig :: UserTypeCtxt
	 -> LHsType Name
	 -> TcSigmaType
	 -> TcM (TcType,	   -- The type to use for "inside" the signature
		 [(Name, TcType)], -- The new bit of type environment, binding
				   -- the scoped type variables
                 HsWrapper)        -- Coercion due to unification with actual ty
                                   -- Of shape:  res_ty ~ sig_ty
tcPatSig ctxt sig res_ty
  = do	{ (sig_tvs, sig_ty) <- tcHsPatSigType ctxt sig
    	-- sig_tvs are the type variables free in 'sig', 
	-- and not already in scope. These are the ones
	-- that should be brought into scope

	; if null sig_tvs then do {
		-- The type signature binds no type variables, 
		-- and hence is rigid, so use it to zap the res_ty
                  wrap <- tcSubType PatSigOrigin ctxt res_ty sig_ty
		; return (sig_ty, [], wrap)
        } else do {
		-- Type signature binds at least one scoped type variable
	
		-- A pattern binding cannot bind scoped type variables
		-- The renamer fails with a name-out-of-scope error 
		-- if a pattern binding tries to bind a type variable,
		-- So we just have an ASSERT here
	; let in_pat_bind = case ctxt of
				BindPatSigCtxt -> True
				_              -> False
	; ASSERT( not in_pat_bind || null sig_tvs ) return ()

		-- Check that all newly-in-scope tyvars are in fact
		-- constrained by the pattern.  This catches tiresome
		-- cases like	
		--	type T a = Int
		--	f :: Int -> Int
		-- 	f (x :: T a) = ...
		-- Here 'a' doesn't get a binding.  Sigh
	; let bad_tvs = filterOut (`elemVarSet` exactTyVarsOfType sig_ty) sig_tvs
	; checkTc (null bad_tvs) (badPatSigTvs sig_ty bad_tvs)

	-- Now do a subsumption check of the pattern signature against res_ty
        ; sig_tvs' <- tcInstSigTyVars sig_tvs
        ; let sig_ty' = substTyWith sig_tvs sig_tv_tys' sig_ty
              sig_tv_tys' = mkTyVarTys sig_tvs'
	; wrap <- tcSubType PatSigOrigin ctxt res_ty sig_ty'

	-- Check that each is bound to a distinct type variable,
	-- and one that is not already in scope
        ; binds_in_scope <- getScopedTyVarBinds
	; let tv_binds = map tyVarName sig_tvs `zip` sig_tv_tys'
	; check binds_in_scope tv_binds
	
	-- Phew!
        ; return (sig_ty', tv_binds, wrap)
        } }
  where
    check _ [] = return ()
    check in_scope ((n,ty):rest) = do { check_one in_scope n ty
				      ; check ((n,ty):in_scope) rest }

    check_one in_scope n ty
	= checkTc (null dups) (dupInScope n (head dups) ty)
		-- Must not bind to the same type variable
		-- as some other in-scope type variable
	where
	  dups = [n' | (n',ty') <- in_scope, eqType ty' ty]
\end{code}


%************************************************************************
%*                                                                      *
        Checking kinds
%*                                                                      *
%************************************************************************

We would like to get a decent error message from
  (a) Under-applied type constructors
             f :: (Maybe, Maybe)
  (b) Over-applied type constructors
             f :: Int x -> Int x

\begin{code}
-- The ExpKind datatype means "expected kind" and contains 
-- some info about just why that kind is expected, to improve
-- the error message on a mis-match
data ExpKind = EK TcKind EkCtxt
data EkCtxt  = EkUnk		-- Unknown context
      	     | EkEqPred		-- Second argument of an equality predicate
      	     | EkKindSig	-- Kind signature
     	     | EkArg SDoc Int   -- Function, arg posn, expected kind
             | EkIParam         -- Implicit parameter type
             | EkFamInst        -- Family instance

instance Outputable ExpKind where
  ppr (EK k _) = ptext (sLit "Expected kind:") <+> ppr k

ekLifted, ekOpen, ekArg, ekConstraint :: ExpKind
ekLifted     = EK liftedTypeKind EkUnk
ekOpen       = EK openTypeKind   EkUnk
ekArg        = EK argTypeKind    EkUnk
ekConstraint = EK constraintKind EkUnk

unifyKinds :: SDoc -> [(LHsType Name, TcKind)] -> TcM TcKind
unifyKinds fun act_kinds = do
  kind <- newMetaKindVar
  let exp_kind arg_no = EK kind (EkArg fun arg_no)
  mapM_ (\(arg_no, (ty, act_kind)) -> checkExpectedKind ty act_kind (exp_kind arg_no)) (zip [1..] act_kinds)
  return kind

checkExpectedKind :: Outputable a => a -> TcKind -> ExpKind -> TcM ()
-- A fancy wrapper for 'unifyKind', which tries
-- to give decent error messages.
--      (checkExpectedKind ty act_kind exp_kind)
-- checks that the actual kind act_kind is compatible
--      with the expected kind exp_kind
-- The first argument, ty, is used only in the error message generation
checkExpectedKind ty act_kind ek@(EK exp_kind ek_ctxt) = do
    traceTc "checkExpectedKind" (ppr ty $$ ppr act_kind $$ ppr ek)
    (_errs, mb_r) <- tryTc (unifyKind act_kind exp_kind)
    case mb_r of
        Just _  -> return ()  -- Unification succeeded
        Nothing -> do

        -- So there's definitely an error
        -- Now to find out what sort
           exp_kind <- zonkTcKind exp_kind
           act_kind <- zonkTcKind act_kind

           env0 <- tcInitTidyEnv
           let (exp_as, _) = splitKindFunTys exp_kind
               (act_as, _) = splitKindFunTys act_kind
               n_exp_as = length exp_as
               n_act_as = length act_as

               (env1, tidy_exp_kind) = tidyOpenKind env0 exp_kind
               (env2, tidy_act_kind) = tidyOpenKind env1 act_kind

               err | n_exp_as < n_act_as     -- E.g. [Maybe]
                   = quotes (ppr ty) <+> ptext (sLit "is not applied to enough type arguments")

                     -- Now n_exp_as >= n_act_as. In the next two cases,
                     -- n_exp_as == 0, and hence so is n_act_as
                   | isConstraintKind tidy_act_kind
                   = text "Predicate" <+> quotes (ppr ty) <+> text "used as a type"
                   
                   | isConstraintKind tidy_exp_kind
                   = text "Type of kind " <+> ppr tidy_act_kind <+> text "used as a constraint"
                   
                   | isLiftedTypeKind exp_kind && isUnliftedTypeKind act_kind
                   = ptext (sLit "Expecting a lifted type, but") <+> quotes (ppr ty)
                       <+> ptext (sLit "is unlifted")

                   | isUnliftedTypeKind exp_kind && isLiftedTypeKind act_kind
                   = ptext (sLit "Expecting an unlifted type, but") <+> quotes (ppr ty)
                       <+> ptext (sLit "is lifted")

                   | otherwise               -- E.g. Monad [Int]
                   = ptext (sLit "Kind mis-match")

               more_info = sep [ expected_herald ek_ctxt <+> ptext (sLit "kind") 
                                    <+> quotes (pprKind tidy_exp_kind) <> comma,
                                 ptext (sLit "but") <+> quotes (ppr ty) <+>
                                     ptext (sLit "has kind") <+> quotes (pprKind tidy_act_kind)]

               expected_herald EkUnk     = ptext (sLit "Expected")
               expected_herald EkKindSig = ptext (sLit "An enclosing kind signature specified")
               expected_herald EkEqPred  = ptext (sLit "The left argument of the equality predicate had")
               expected_herald EkIParam  = ptext (sLit "The type argument of the implicit parameter had")
               expected_herald EkFamInst = ptext (sLit "The family instance required")
               expected_herald (EkArg fun arg_no)
	         = ptext (sLit "The") <+> speakNth arg_no <+> ptext (sLit "argument of")
		   <+> fun <+> ptext (sLit ("should have"))

           failWithTcM (env2, err $$ more_info)
\end{code}

%************************************************************************
%*                                                                      *
        Sort checking kinds
%*                                                                      *
%************************************************************************

scDsLHsKind converts a user-written kind to an internal, sort-checked kind.
It does sort checking and desugaring at the same time, in one single pass.
It fails when the kinds are not well-formed (eg. data A :: * Int), or if there
are non-promotable or non-fully applied kinds.

\begin{code}
scDsLHsKind :: LHsKind Name -> TcM Kind
scDsLHsKind k = addErrCtxt (ptext (sLit "In the kind") <+> quotes (ppr k)) $
                  sc_ds_lhs_kind k

scDsLHsMaybeKind :: Maybe (LHsKind Name) -> TcM (Maybe Kind)
scDsLHsMaybeKind Nothing  = return Nothing
scDsLHsMaybeKind (Just k) = do k' <- scDsLHsKind k
                               return (Just k')

sc_ds_lhs_kind :: LHsKind Name -> TcM Kind
sc_ds_lhs_kind (L span ki) = setSrcSpan span (sc_ds_hs_kind ki)

-- The main worker
sc_ds_hs_kind :: HsKind Name -> TcM Kind
sc_ds_hs_kind k@(HsTyVar _)   = sc_ds_app k []
sc_ds_hs_kind k@(HsAppTy _ _) = sc_ds_app k []

sc_ds_hs_kind (HsParTy ki) = sc_ds_lhs_kind ki

sc_ds_hs_kind (HsFunTy ki1 ki2) =
  do kappa_ki1 <- sc_ds_lhs_kind ki1
     kappa_ki2 <- sc_ds_lhs_kind ki2
     return (mkArrowKind kappa_ki1 kappa_ki2)

sc_ds_hs_kind (HsListTy ki) =
  do kappa <- sc_ds_lhs_kind ki
     checkWiredInTyCon listTyCon
     return $ mkListTy kappa

sc_ds_hs_kind (HsTupleTy _ kis) =
  do kappas <- mapM sc_ds_lhs_kind kis
     checkWiredInTyCon tycon
     return $ mkTyConApp tycon kappas
  where tycon = tupleTyCon BoxedTuple (length kis)

-- Argument not kind-shaped
sc_ds_hs_kind k = panic ("sc_ds_hs_kind: " ++ showPpr k)

-- Special case for kind application
sc_ds_app :: HsKind Name -> [LHsKind Name] -> TcM Kind
sc_ds_app (HsAppTy ki1 ki2) kis = sc_ds_app (unLoc ki1) (ki2:kis)
sc_ds_app (HsTyVar tc)      kis =
  do arg_kis <- mapM sc_ds_lhs_kind kis
     sc_ds_var_app tc arg_kis
sc_ds_app ki                _   = failWithTc (quotes (ppr ki) <+> 
                                    ptext (sLit "is not a kind constructor"))

-- IA0_TODO: With explicit kind polymorphism I might need to add ATyVar
sc_ds_var_app :: Name -> [Kind] -> TcM Kind
-- Special case for * and Constraint kinds
sc_ds_var_app name arg_kis
  |    name == liftedTypeKindTyConName
    || name == constraintKindTyConName = do
    unless (null arg_kis)
      (failWithTc (text "Kind" <+> ppr name <+> text "cannot be applied"))
    thing <- tcLookup name
    case thing of
      AGlobal (ATyCon tc) -> return (mkTyConApp tc [])
      _                   -> panic "sc_ds_var_app 1"

-- General case
sc_ds_var_app name arg_kis = do
  thing <- tcLookup name
  case thing of
    AGlobal (ATyCon tc)
      | isAlgTyCon tc || isTupleTyCon tc -> do
      poly_kinds <- xoptM Opt_PolyKinds
      unless poly_kinds $ addErr (polyKindsErr name)
      let tc_kind = tyConKind tc
      case isPromotableKind tc_kind of
        Just n | n == length arg_kis ->
          return (mkTyConApp (mkPromotedTypeTyCon tc) arg_kis)
        Just _  -> err tc_kind "is not fully applied"
        Nothing -> err tc_kind "is not promotable"

    _ -> wrongThingErr "promoted type" thing name

  where err k m = failWithTc (    quotes (ppr name) <+> ptext (sLit "of kind")
                              <+> quotes (ppr k)    <+> ptext (sLit m))

\end{code}

%************************************************************************
%*									*
		Scoped type variables
%*									*
%************************************************************************

\begin{code}
pprHsSigCtxt :: UserTypeCtxt -> LHsType Name -> SDoc
pprHsSigCtxt ctxt hs_ty = sep [ ptext (sLit "In") <+> pprUserTypeCtxt ctxt <> colon, 
				 nest 2 (pp_sig ctxt) ]
  where
    pp_sig (FunSigCtxt n)  = pp_n_colon n
    pp_sig (ConArgCtxt n)  = pp_n_colon n
    pp_sig (ForSigCtxt n)  = pp_n_colon n
    pp_sig _               = ppr (unLoc hs_ty)

    pp_n_colon n = ppr n <+> dcolon <+> ppr (unLoc hs_ty)

badPatSigTvs :: TcType -> [TyVar] -> SDoc
badPatSigTvs sig_ty bad_tvs
  = vcat [ fsep [ptext (sLit "The type variable") <> plural bad_tvs, 
                 quotes (pprWithCommas ppr bad_tvs), 
          	 ptext (sLit "should be bound by the pattern signature") <+> quotes (ppr sig_ty),
	  	 ptext (sLit "but are actually discarded by a type synonym") ]
         , ptext (sLit "To fix this, expand the type synonym") 
         , ptext (sLit "[Note: I hope to lift this restriction in due course]") ]

dupInScope :: Name -> Name -> Type -> SDoc
dupInScope n n' _
  = hang (ptext (sLit "The scoped type variables") <+> quotes (ppr n) <+> ptext (sLit "and") <+> quotes (ppr n'))
       2 (vcat [ptext (sLit "are bound to the same type (variable)"),
		ptext (sLit "Distinct scoped type variables must be distinct")])
\end{code}


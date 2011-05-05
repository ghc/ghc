%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
module TcHsType (
	tcHsSigType, tcHsSigTypeNC, tcHsDeriv, 
	tcHsInstHead, tcHsQuantifiedType,
	UserTypeCtxt(..), 

		-- Kind checking
	kcHsTyVars, kcHsSigType, kcHsLiftedSigType, 
	kcLHsType, kcCheckLHsType, kcHsContext, 
	
		-- Typechecking kinded types
	tcHsKindedContext, tcHsKindedType, tcHsBangType,
	tcTyVarBndrs, dsHsType, kcHsLPred, dsHsLPred,
	tcDataKindSig, ExpKind(..), EkCtxt(..),

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
import TcEnv
import TcMType
import TcUnify
import TcIface
import TcType
import {- Kind parts of -} Type
import Var
import VarSet
import TyCon
import Class
import Name
import NameSet
import TysWiredIn
import BasicTypes
import SrcLoc
import Util
import UniqSupply
import Outputable
import FastString
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

tcHsInstHead :: LHsType Name -> TcM ([TyVar], ThetaType, Class, [Type])
-- Typecheck an instance head.  We can't use 
-- tcHsSigType, because it's not a valid user type.
tcHsInstHead (L loc hs_ty)
  = setSrcSpan loc   $	-- No need for an "In the type..." context
                        -- because that comes from the caller
    do { kinded_ty <- kc_inst_head hs_ty
       ; ds_inst_head kinded_ty }
  where
    kc_inst_head ty@(HsPredTy pred@(HsClassP {}))
      = do { (pred', kind) <- kc_pred pred
           ; checkExpectedKind ty kind ekLifted
           ; return (HsPredTy pred') }
    kc_inst_head (HsForAllTy exp tv_names context (L loc ty))
      = kcHsTyVars tv_names         $ \ tv_names' ->
        do { ctxt' <- kcHsContext context
           ; ty'   <- kc_inst_head ty
           ; return (HsForAllTy exp tv_names' ctxt' (L loc ty')) }
    kc_inst_head _ = failWithTc (ptext (sLit "Malformed instance type"))

    ds_inst_head (HsPredTy (HsClassP cls_name tys))
      = do { clas <- tcLookupClass cls_name
           ; arg_tys <- dsHsTypes tys
           ; return ([], [], clas, arg_tys) }
    ds_inst_head (HsForAllTy _ tvs ctxt (L _ tau))
      = tcTyVarBndrs tvs  $ \ tvs' ->
        do { ctxt' <- mapM dsHsLPred (unLoc ctxt)
           ; (tvs_r, ctxt_r, cls, tys) <- ds_inst_head tau
           ; return (tvs' ++ tvs_r, ctxt' ++ ctxt_r , cls, tys) }
    ds_inst_head _ = panic "ds_inst_head"

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
tc_hs_deriv tv_names (HsPredTy (HsClassP cls_name hs_tys))
  = kcHsTyVars tv_names 		$ \ tv_names' ->
    do	{ cls_kind <- kcClass cls_name
	; (tys, _res_kind) <- kcApps cls_name cls_kind hs_tys
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
  = failWithTc (ptext (sLit "Illegal deriving item") <+> ppr other)
\end{code}

	These functions are used during knot-tying in
	type and class declarations, when we have to
 	separate kind-checking, desugaring, and validity checking

\begin{code}
kcHsSigType, kcHsLiftedSigType :: LHsType Name -> TcM (LHsType Name)
	-- Used for type signatures
kcHsSigType ty 	     = addKcTypeCtxt ty $ kcTypeType ty
kcHsLiftedSigType ty = addKcTypeCtxt ty $ kcLiftedType ty

tcHsKindedType :: LHsType Name -> TcM Type
  -- Don't do kind checking, nor validity checking.
  -- This is used in type and class decls, where kinding is
  -- done in advance, and validity checking is done later
  -- [Validity checking done later because of knot-tying issues.]
tcHsKindedType hs_ty = dsHsType hs_ty

tcHsBangType :: LHsType Name -> TcM Type
-- Permit a bang, but discard it
tcHsBangType (L _ (HsBangTy _ ty)) = tcHsKindedType ty
tcHsBangType ty                    = tcHsKindedType ty

tcHsKindedContext :: LHsContext Name -> TcM ThetaType
-- Used when we are expecting a ClassContext (i.e. no implicit params)
-- Does not do validity checking, like tcHsKindedType
tcHsKindedContext hs_theta = addLocM (mapM dsHsLPred) hs_theta
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
  = do { let (fun_ty, arg_tys) = splitHsAppTys ty1 ty2
       ; (fun_ty', fun_kind) <- kc_lhs_type fun_ty
       ; arg_tys' <- kcCheckApps fun_ty fun_kind arg_tys ty exp_kind
       ; return (mkHsAppTys fun_ty' arg_tys') }

-- This is the general case: infer the kind and compare
kc_check_hs_type ty exp_kind
  = do	{ (ty', act_kind) <- kc_hs_type ty
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
    do { (ty', kind) <- kc_hs_type ty
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

kc_hs_type (HsTyVar name) = do
    kind <- kcTyVar name
    return (HsTyVar name, kind)

kc_hs_type (HsListTy ty) = do
    ty' <- kcLiftedType ty
    return (HsListTy ty', liftedTypeKind)

kc_hs_type (HsPArrTy ty) = do
    ty' <- kcLiftedType ty
    return (HsPArrTy ty', liftedTypeKind)

kc_hs_type (HsKindSig ty k) = do
    ty' <- kc_check_lhs_type ty (EK k EkKindSig)
    return (HsKindSig ty' k, k)

kc_hs_type (HsTupleTy Boxed tys) = do
    tys' <- mapM kcLiftedType tys
    return (HsTupleTy Boxed tys', liftedTypeKind)

kc_hs_type (HsTupleTy Unboxed tys) = do
    tys' <- mapM kcTypeType tys
    return (HsTupleTy Unboxed tys', ubxTupleKind)

kc_hs_type (HsFunTy ty1 ty2) = do
    ty1' <- kc_check_lhs_type ty1 (EK argTypeKind EkUnk)
    ty2' <- kcTypeType ty2
    return (HsFunTy ty1' ty2', liftedTypeKind)

kc_hs_type (HsOpTy ty1 op ty2) = do
    op_kind <- addLocM kcTyVar op
    ([ty1',ty2'], res_kind) <- kcApps op op_kind [ty1,ty2]
    return (HsOpTy ty1' op ty2', res_kind)

kc_hs_type (HsAppTy ty1 ty2) = do
    (fun_ty', fun_kind) <- kc_lhs_type fun_ty
    (arg_tys', res_kind) <- kcApps fun_ty fun_kind arg_tys
    return (mkHsAppTys fun_ty' arg_tys', res_kind)
  where
    (fun_ty, arg_tys) = splitHsAppTys ty1 ty2

kc_hs_type (HsPredTy pred)
  = wrongPredErr pred

kc_hs_type (HsCoreTy ty)
  = return (HsCoreTy ty, typeKind ty)

kc_hs_type (HsForAllTy exp tv_names context ty)
  = kcHsTyVars tv_names         $ \ tv_names' ->
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

splitHsAppTys :: LHsType Name -> LHsType Name -> (LHsType Name, [LHsType Name])
splitHsAppTys fun_ty arg_ty = split fun_ty [arg_ty]
  where
    split (L _ (HsAppTy f a)) as = split f (a:as)
    split f       	      as = (f,as)

mkHsAppTys :: LHsType Name -> [LHsType Name] -> HsType Name
mkHsAppTys fun_ty [] = pprPanic "mkHsAppTys" (ppr fun_ty)
mkHsAppTys fun_ty (arg_ty:arg_tys)
  = foldl mk_app (HsAppTy fun_ty arg_ty) arg_tys
  where
    mk_app fun arg = HsAppTy (noLoc fun) arg	-- Add noLocs for inner nodes of
					 	-- the application; they are
						-- never used 

---------------------------
splitFunKind :: SDoc -> Int -> TcKind -> [b] -> TcM ([(b,ExpKind)], TcKind)
splitFunKind _       _      fk [] = return ([], fk)
splitFunKind the_fun arg_no fk (arg:args)
  = do { mb_fk <- matchExpectedFunKind fk
       ; case mb_fk of
            Nothing       -> failWithTc too_many_args 
            Just (ak,fk') -> do { (aks, rk) <- splitFunKind the_fun (arg_no+1) fk' args
                                ; return ((arg, EK ak (EkArg the_fun arg_no)):aks, rk) } }
  where
    too_many_args = quotes the_fun <+>
		    ptext (sLit "is applied to too many type arguments")

---------------------------
kcHsContext :: LHsContext Name -> TcM (LHsContext Name)
kcHsContext ctxt = wrapLocM (mapM kcHsLPred) ctxt

kcHsLPred :: LHsPred Name -> TcM (LHsPred Name)
kcHsLPred = wrapLocM kcHsPred

kcHsPred :: HsPred Name -> TcM (HsPred Name)
kcHsPred pred = do      -- Checks that the result is a type kind
    (pred', kind) <- kc_pred pred
    checkExpectedKind pred kind ekOpen
    return pred'
    
---------------------------
kc_pred :: HsPred Name -> TcM (HsPred Name, TcKind)	
	-- Does *not* check for a saturated
	-- application (reason: used from TcDeriv)
kc_pred (HsIParam name ty)
  = do { (ty', kind) <- kc_lhs_type ty
       ; return (HsIParam name ty', kind) }
kc_pred (HsClassP cls tys)
  = do { kind <- kcClass cls
       ; (tys', res_kind) <- kcApps cls kind tys
       ; return (HsClassP cls tys', res_kind) }
kc_pred (HsEqualP ty1 ty2)
  = do { (ty1', kind1) <- kc_lhs_type ty1
       ; (ty2', kind2) <- kc_lhs_type ty2
       ; checkExpectedKind ty2 kind2 (EK kind1 EkEqPred)
       ; return (HsEqualP ty1' ty2', unliftedTypeKind) }

---------------------------
kcTyVar :: Name -> TcM TcKind
kcTyVar name = do	-- Could be a tyvar or a tycon
    traceTc "lk1" (ppr name)
    thing <- tcLookup name
    traceTc "lk2" (ppr name <+> ppr thing)
    case thing of 
        ATyVar _ ty             -> return (typeKind ty)
        AThing kind             -> return kind
        AGlobal (ATyCon tc)     -> return (tyConKind tc)
        _                       -> wrongThingErr "type" thing name

kcClass :: Name -> TcM TcKind
kcClass cls = do	-- Must be a class
    thing <- tcLookup cls
    case thing of
        AThing kind             -> return kind
        AGlobal (AClass cls)    -> return (tyConKind (classTyCon cls))
        _                       -> wrongThingErr "class" thing cls
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

ds_type :: HsType Name -> TcM Type
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

ds_type (HsTupleTy boxity tys) = do
    tau_tys <- dsHsTypes tys
    checkWiredInTyCon tycon
    return (mkTyConApp tycon tau_tys)
  where
    tycon = tupleTyCon boxity (length tys)

ds_type (HsFunTy ty1 ty2) = do
    tau_ty1 <- dsHsType ty1
    tau_ty2 <- dsHsType ty2
    return (mkFunTy tau_ty1 tau_ty2)

ds_type (HsOpTy ty1 (L span op) ty2) = do
    tau_ty1 <- dsHsType ty1
    tau_ty2 <- dsHsType ty2
    setSrcSpan span (ds_var_app op [tau_ty1,tau_ty2])

ds_type ty@(HsAppTy _ _)
  = ds_app ty []

ds_type (HsPredTy pred) = do
    pred' <- dsHsPred pred
    return (mkPredTy pred')

ds_type (HsForAllTy _ tv_names ctxt ty)
  = tcTyVarBndrs tv_names               $ \ tyvars -> do
    theta <- mapM dsHsLPred (unLoc ctxt)
    tau <- dsHsType ty
    return (mkSigmaTy tyvars theta tau)

ds_type (HsDocTy ty _)  -- Remove the doc comment
  = dsHsType ty

ds_type (HsSpliceTy _ _ kind) 
  = do { kind' <- zonkTcKindToKind kind
       ; newFlexiTyVarTy kind' }

ds_type (HsQuasiQuoteTy {}) = panic "ds_type"	-- Eliminated by renamer
ds_type (HsCoreTy ty)       = return ty

dsHsTypes :: [LHsType Name] -> TcM [Type]
dsHsTypes arg_tys = mapM dsHsType arg_tys
\end{code}

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
ds_var_app name arg_tys = do
    thing <- tcLookup name
    case thing of
	ATyVar _ ty         -> return (mkAppTys ty arg_tys)
	AGlobal (ATyCon tc) -> return (mkTyConApp tc arg_tys)
	_                   -> wrongThingErr "type" thing name
\end{code}


Contexts
~~~~~~~~

\begin{code}
dsHsLPred :: LHsPred Name -> TcM PredType
dsHsLPred pred = dsHsPred (unLoc pred)

dsHsPred :: HsPred Name -> TcM PredType
dsHsPred (HsClassP class_name tys)
  = do { arg_tys <- dsHsTypes tys
       ; clas <- tcLookupClass class_name
       ; return (ClassP clas arg_tys)
       }
dsHsPred (HsEqualP ty1 ty2)
  = do { arg_ty1 <- dsHsType ty1
       ; arg_ty2 <- dsHsType ty2
       ; return (EqPred arg_ty1 arg_ty2)
       }
dsHsPred (HsIParam name ty)
  = do { arg_ty <- dsHsType ty
       ; return (IParam name arg_ty)
       }
\end{code}

\begin{code}
addKcTypeCtxt :: LHsType Name -> TcM a -> TcM a
	-- Wrap a context around only if we want to show that contexts.  
addKcTypeCtxt (L _ (HsPredTy _)) thing = thing
	-- Omit invisble ones and ones user's won't grok (HsPred p).
addKcTypeCtxt (L _ other_ty) thing = addErrCtxt (typeCtxt other_ty) thing

typeCtxt :: HsType Name -> SDoc
typeCtxt ty = ptext (sLit "In the type") <+> quotes (ppr ty)
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
  = do { kinded_tvs <- mapM (wrapLocM kcHsTyVar) tvs
       ; tcExtendKindEnvTvs kinded_tvs thing_inside }

kcHsTyVar :: HsTyVarBndr Name -> TcM (HsTyVarBndr Name)
	-- Return a *kind-annotated* binder, and a tyvar with a mutable kind in it	
kcHsTyVar (UserTyVar name _)  = UserTyVar name <$> newKindVar
kcHsTyVar tv@(KindedTyVar {}) = return tv

------------------
tcTyVarBndrs :: [LHsTyVarBndr Name] 	-- Kind-annotated binders, which need kind-zonking
	     -> ([TyVar] -> TcM r)
	     -> TcM r
-- Used when type-checking types/classes/type-decls
-- Brings into scope immutable TyVars, not mutable ones that require later zonking
tcTyVarBndrs bndrs thing_inside = do
    tyvars <- mapM (zonk . unLoc) bndrs
    tcExtendTyVarEnv tyvars (thing_inside tyvars)
  where
    zonk (UserTyVar name kind) = do { kind' <- zonkTcKindToKind kind
				    ; return (mkTyVar name kind') }
    zonk (KindedTyVar name kind) = return (mkTyVar name kind)

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
	  dups = [n' | (n',ty') <- in_scope, tcEqType ty' ty]
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


ekLifted, ekOpen :: ExpKind
ekLifted = EK liftedTypeKind EkUnk
ekOpen   = EK openTypeKind   EkUnk

checkExpectedKind :: Outputable a => a -> TcKind -> ExpKind -> TcM ()
-- A fancy wrapper for 'unifyKind', which tries
-- to give decent error messages.
--      (checkExpectedKind ty act_kind exp_kind)
-- checks that the actual kind act_kind is compatible
--      with the expected kind exp_kind
-- The first argument, ty, is used only in the error message generation
checkExpectedKind ty act_kind (EK exp_kind ek_ctxt)
  | act_kind `isSubKind` exp_kind -- Short cut for a very common case
  = return ()
  | otherwise = do
    (_errs, mb_r) <- tryTc (unifyKind exp_kind act_kind)
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

               (env1, tidy_exp_kind) = tidyKind env0 exp_kind
               (env2, tidy_act_kind) = tidyKind env1 act_kind

               err | n_exp_as < n_act_as     -- E.g. [Maybe]
                   = quotes (ppr ty) <+> ptext (sLit "is not applied to enough type arguments")

                     -- Now n_exp_as >= n_act_as. In the next two cases,
                     -- n_exp_as == 0, and hence so is n_act_as
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
               expected_herald (EkArg fun arg_no)
	         = ptext (sLit "The") <+> speakNth arg_no <+> ptext (sLit "argument of")
		   <+> quotes fun <+> ptext (sLit ("should have"))

           failWithTcM (env2, err $$ more_info)
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

wrongPredErr :: HsPred Name -> TcM (HsType Name, TcKind)
wrongPredErr pred = failWithTc (text "Predicate used as a type:" <+> ppr pred)
\end{code}


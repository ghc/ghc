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
	tcHsInstHead, 
	UserTypeCtxt(..), 

                -- Type checking type and class decls
	kcTyClTyVars, tcTyClTyVars,
        tcHsConArgType, tcDataKindSig, 
        tcClassSigType, 

		-- Kind-checking types
                -- No kind generalisation, no checkValidType
	kcHsTyVarBndrs, tcHsTyVarBndrs, 
        tcHsLiftedType, tcHsOpenType,
	tcLHsType, tcCheckLHsType, 
        tcHsContext, tcInferApps, tcHsArgTys,

        kindGeneralize, checkKind,

		-- Sort-checking kinds
	tcLHsKind, 

		-- Pattern type signatures
	tcHsPatSigType, tcPatSig
   ) where

#include "HsVersions.h"

#ifdef GHCI 	/* Only if bootstrapped */
import {-# SOURCE #-}	TcSplice( tcSpliceType )
#endif

import HsSyn
import TcHsSyn ( zonkTcTypeToType, emptyZonkEnv )
import TcRnMonad
import TcEvidence( HsWrapper )
import TcEnv
import TcMType
import TcValidity
import TcUnify
import TcIface
import TcType
import Type
import Kind
import TypeRep( mkNakedTyConApp )
import Var
import VarSet
import TyCon
import DataCon
import TysPrim ( liftedTypeKindTyConName, constraintKindTyConName )
import Class
import Name
import NameEnv
import TysWiredIn
import BasicTypes
import SrcLoc
import ErrUtils ( isEmptyMessages )
import DynFlags ( ExtensionFlag( Opt_DataKinds ), getDynFlags )
import Unique
import UniqSupply
import Outputable
import FastString
import Util

import Control.Monad ( unless, when, zipWithM )
import PrelNames( ipClassName, funTyConKey )
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
              Check types AND do validity checking
%*									*
%************************************************************************

\begin{code}
tcHsSigType, tcHsSigTypeNC :: UserTypeCtxt -> LHsType Name -> TcM Type
  -- NB: it's important that the foralls that come from the top-level
  --	 HsForAllTy in hs_ty occur *first* in the returned type.
  --     See Note [Scoped] with TcSigInfo
tcHsSigType ctxt hs_ty
  = addErrCtxt (pprHsSigCtxt ctxt hs_ty) $
    tcHsSigTypeNC ctxt hs_ty

tcHsSigTypeNC ctxt (L loc hs_ty)
  = setSrcSpan loc $    -- The "In the type..." context
                        -- comes from the caller; hence "NC"
    do  { kind <- case expectedKindInCtxt ctxt of
                    Nothing -> newMetaKindVar
                    Just k  -> return k
          -- The kind is checked by checkValidType, and isn't necessarily
          -- of kind * in a Template Haskell quote eg [t| Maybe |]

          -- Generalise here: see Note [ generalisation]
        ; ty <- tcCheckHsTypeAndGen hs_ty kind

          -- Zonk to expose kind information to checkValidType
        ; ty <- zonkTcType ty
        ; checkValidType ctxt ty
        ; return ty }

-----------------
tcHsInstHead :: UserTypeCtxt -> LHsType Name -> TcM ([TyVar], ThetaType, Class, [Type])
-- Like tcHsSigTypeNC, but for an instance head.
tcHsInstHead ctxt lhs_ty@(L loc hs_ty)
  = setSrcSpan loc $    -- The "In the type..." context comes from the caller
    do { ty <- tcCheckHsTypeAndGen hs_ty constraintKind
       ; ty <- zonkTcType ty
       ; checkValidInstance ctxt lhs_ty ty }

-----------------
tcHsDeriv :: HsType Name -> TcM ([TyVar], Class, [Type])
-- Like tcHsSigTypeNC, but for the ...deriving( ty ) clause
tcHsDeriv hs_ty 
  = do { kind <- newMetaKindVar
       ; ty   <- tcCheckHsTypeAndGen hs_ty kind
                 -- Funny newtype deriving form
                 -- 	forall a. C [a]
                 -- where C has arity 2. Hence any-kinded result
       ; ty   <- zonkTcType ty
       ; let (tvs, pred) = splitForAllTys ty
       ; case getClassPredTys_maybe pred of
           Just (cls, tys) -> return (tvs, cls, tys)
           Nothing -> failWithTc (ptext (sLit "Illegal deriving item") <+> quotes (ppr hs_ty)) }

-- Used for 'VECTORISE [SCALAR] instance' declarations
--
tcHsVectInst :: LHsType Name -> TcM (Class, [Type])
tcHsVectInst ty
  | Just (L _ cls_name, tys) <- splitLHsClassTy_maybe ty
  = do { (cls, cls_kind) <- tcClass cls_name
       ; (arg_tys, _res_kind) <- tcInferApps cls_name cls_kind tys
       ; return (cls, arg_tys) }
  | otherwise
  = failWithTc $ ptext (sLit "Malformed instance type")
\end{code}

	These functions are used during knot-tying in
	type and class declarations, when we have to
 	separate kind-checking, desugaring, and validity checking


%************************************************************************
%*									*
            The main kind checker: no validity checks here
%*									*
%************************************************************************
	
	First a couple of simple wrappers for kcHsType

\begin{code}
tcClassSigType :: LHsType Name -> TcM Type
tcClassSigType lhs_ty@(L _ hs_ty)
  = addTypeCtxt lhs_ty $
    do { ty <- tcCheckHsTypeAndGen hs_ty liftedTypeKind
       ; zonkTcTypeToType emptyZonkEnv ty }

tcHsConArgType :: NewOrData ->  LHsType Name -> TcM Type
-- Permit a bang, but discard it
tcHsConArgType NewType  bty = tcHsLiftedType (getBangType bty)
  -- Newtypes can't have bangs, but we don't check that
  -- until checkValidDataCon, so do not want to crash here

tcHsConArgType DataType bty = tcHsOpenType (getBangType bty)
  -- Can't allow an unlifted type for newtypes, because we're effectively
  -- going to remove the constructor while coercing it to a lifted type.
  -- And newtypes can't be bang'd

---------------------------
tcHsArgTys :: SDoc -> [LHsType Name] -> [Kind] -> TcM [TcType]
tcHsArgTys what tys kinds
  = sequence [ addTypeCtxt ty $
               tc_lhs_type ty (expArgKind what kind n)
             | (ty,kind,n) <- zip3 tys kinds [1..] ]

tc_hs_arg_tys :: SDoc -> [LHsType Name] -> [Kind] -> TcM [TcType]
-- Just like tcHsArgTys but without the addTypeCtxt
tc_hs_arg_tys what tys kinds
  = sequence [ tc_lhs_type ty (expArgKind what kind n)
             | (ty,kind,n) <- zip3 tys kinds [1..] ]

---------------------------
tcHsOpenType, tcHsLiftedType :: LHsType Name -> TcM TcType
-- Used for type signatures
-- Do not do validity checking
tcHsOpenType ty   = addTypeCtxt ty $ tc_lhs_type ty ekOpen
tcHsLiftedType ty = addTypeCtxt ty $ tc_lhs_type ty ekLifted

-- Like tcHsType, but takes an expected kind
tcCheckLHsType :: LHsType Name -> Kind -> TcM Type
tcCheckLHsType hs_ty exp_kind
  = addTypeCtxt hs_ty $ 
    tc_lhs_type hs_ty (EK exp_kind expectedKindMsg)

tcLHsType :: LHsType Name -> TcM (TcType, TcKind)
-- Called from outside: set the context
tcLHsType ty = addTypeCtxt ty (tc_infer_lhs_type ty)

---------------------------
tcCheckHsTypeAndGen :: HsType Name -> Kind -> TcM Type
-- Input type is HsType, not LhsType; the caller adds the context
-- Typecheck a type signature, and kind-generalise it
-- The result is not necessarily zonked, and has not been checked for validity
tcCheckHsTypeAndGen hs_ty kind
  = do { ty  <- tc_hs_type hs_ty (EK kind expectedKindMsg)
       ; kvs <- kindGeneralize (tyVarsOfType ty) []
       ; return (mkForAllTys kvs ty) }
\end{code}

Like tcExpr, tc_hs_type takes an expected kind which it unifies with
the kind it figures out. When we don't know what kind to expect, we use
tc_lhs_type_fresh, to first create a new meta kind variable and use that as
the expected kind.

\begin{code}
tc_infer_lhs_type :: LHsType Name -> TcM (TcType, TcKind)
tc_infer_lhs_type ty =
  do { kv <- newMetaKindVar
     ; r <- tc_lhs_type ty (EK kv expectedKindMsg)
     ; return (r, kv) }

tc_lhs_type :: LHsType Name -> ExpKind -> TcM TcType
tc_lhs_type (L span ty) exp_kind
  = setSrcSpan span $
    do { traceTc "tc_lhs_type:" (ppr ty $$ ppr exp_kind)
       ; tc_hs_type ty exp_kind }

tc_lhs_types :: [(LHsType Name, ExpKind)] -> TcM [TcType]
tc_lhs_types tys_w_kinds = mapM (uncurry tc_lhs_type) tys_w_kinds

------------------------------------------
tc_fun_type :: HsType Name -> LHsType Name -> LHsType Name -> ExpKind -> TcM TcType
-- We need to recognise (->) so that we can construct a FunTy, 
-- *and* we need to do by looking at the Name, not the TyCon
-- (see Note [Zonking inside the knot]).  For example,
-- consider  f :: (->) Int Int  (Trac #7312)
tc_fun_type ty ty1 ty2 exp_kind@(EK _ ctxt)
  = do { ty1' <- tc_lhs_type ty1 (EK openTypeKind ctxt)
       ; ty2' <- tc_lhs_type ty2 (EK openTypeKind ctxt)
       ; checkExpectedKind ty liftedTypeKind exp_kind
       ; return (mkFunTy ty1' ty2') }

------------------------------------------
tc_hs_type :: HsType Name -> ExpKind -> TcM TcType
tc_hs_type (HsParTy ty)        exp_kind = tc_lhs_type ty exp_kind
tc_hs_type (HsDocTy ty _)      exp_kind = tc_lhs_type ty exp_kind
tc_hs_type (HsQuasiQuoteTy {}) _ = panic "tc_hs_type: qq"	-- Eliminated by renamer
tc_hs_type ty@(HsBangTy {})    _
    -- While top-level bangs at this point are eliminated (eg !(Maybe Int)),
    -- other kinds of bangs are not (eg ((!Maybe) Int)). These kinds of
    -- bangs are invalid, so fail. (#7210)
    = failWithTc (ptext (sLit "Unexpected strictness annotation:") <+> ppr ty)
tc_hs_type (HsRecTy _)         _ = panic "tc_hs_type: record" -- Unwrapped by con decls
      -- Record types (which only show up temporarily in constructor 
      -- signatures) should have been removed by now

---------- Functions and applications
tc_hs_type hs_ty@(HsTyVar name) exp_kind
  = do { (ty, k) <- tcTyVar name
       ; checkExpectedKind hs_ty k exp_kind
       ; return ty }

tc_hs_type ty@(HsFunTy ty1 ty2) exp_kind
  = tc_fun_type ty ty1 ty2 exp_kind

tc_hs_type hs_ty@(HsOpTy ty1 (_, l_op@(L _ op)) ty2) exp_kind
  | op `hasKey` funTyConKey
  = tc_fun_type hs_ty ty1 ty2 exp_kind
  | otherwise
  = do { (op', op_kind) <- tcTyVar op
       ; tys' <- tcCheckApps hs_ty l_op op_kind [ty1,ty2] exp_kind
       ; return (mkNakedAppTys op' tys') }
         -- mkNakedAppTys: see Note [Zonking inside the knot]

tc_hs_type hs_ty@(HsAppTy ty1 ty2) exp_kind
  | L _ (HsTyVar fun) <- fun_ty
  , fun `hasKey` funTyConKey
  , [fty1,fty2] <- arg_tys
  = tc_fun_type hs_ty fty1 fty2 exp_kind
  | otherwise
  = do { (fun_ty', fun_kind) <- tc_infer_lhs_type fun_ty
       ; arg_tys' <- tcCheckApps hs_ty fun_ty fun_kind arg_tys exp_kind
       ; return (mkNakedAppTys fun_ty' arg_tys') }
         -- mkNakedAppTys: see Note [Zonking inside the knot]
  where
    (fun_ty, arg_tys) = splitHsAppTys ty1 [ty2]

--------- Foralls
tc_hs_type (HsForAllTy _ hs_tvs context ty) exp_kind
  = tcHsTyVarBndrs hs_tvs $ \ tvs' -> 
    -- Do not kind-generalise here!  See Note [Kind generalisation]
    do { ctxt' <- tcHsContext context
       ; ty'   <- tc_lhs_type ty exp_kind
           -- Why exp_kind?  See Note [Body kind of forall]
       ; return (mkSigmaTy tvs' ctxt' ty') }

--------- Lists, arrays, and tuples
tc_hs_type hs_ty@(HsListTy elt_ty) exp_kind 
  = do { tau_ty <- tc_lhs_type elt_ty ekLifted
       ; checkExpectedKind hs_ty liftedTypeKind exp_kind
       ; checkWiredInTyCon listTyCon
       ; return (mkListTy tau_ty) }

tc_hs_type hs_ty@(HsPArrTy elt_ty) exp_kind
  = do { tau_ty <- tc_lhs_type elt_ty ekLifted
       ; checkExpectedKind hs_ty liftedTypeKind exp_kind
       ; checkWiredInTyCon parrTyCon
       ; return (mkPArrTy tau_ty) }

-- See Note [Distinguishing tuple kinds] in HsTypes
-- See Note [Inferring tuple kinds]
tc_hs_type hs_ty@(HsTupleTy HsBoxedOrConstraintTuple tys) exp_kind@(EK exp_k _ctxt)
     -- (NB: not zonking before looking at exp_k, to avoid left-right bias)
  | isConstraintKind exp_k = tc_tuple hs_ty HsConstraintTuple tys exp_kind
  | isLiftedTypeKind exp_k = tc_tuple hs_ty HsBoxedTuple      tys exp_kind
  | otherwise
  = do { k <- newMetaKindVar
       ; (msgs, mb_tau_tys) <- tryTc (tc_hs_arg_tys (ptext (sLit "a tuple")) tys (repeat k))
       ; k <- zonkTcKind k
           -- Do the experiment inside a 'tryTc' because errors can be
           -- confusing.  Eg Trac #7410 (Either Int, Int), we do not want to get
           -- an error saying "the second argument of a tuple should have kind *->*"

       ; case mb_tau_tys of
           Just tau_tys 
             | not (isEmptyMessages msgs) -> try_again k
             | isConstraintKind k         -> go_for HsConstraintTuple tau_tys
             | isLiftedTypeKind k         -> go_for HsBoxedTuple      tau_tys
             | otherwise                  -> try_again k
           Nothing                        -> try_again k }
   where
     go_for sort tau_tys = finish_tuple hs_ty sort tau_tys exp_kind

     try_again k
       | isConstraintKind k = tc_tuple hs_ty HsConstraintTuple tys exp_kind
       | otherwise          = tc_tuple hs_ty HsBoxedTuple      tys exp_kind
         -- It's not clear what the kind is, so make best guess and
         -- check the arguments again to give good error messages
         -- in eg. `(Maybe, Maybe)`

tc_hs_type hs_ty@(HsTupleTy tup_sort tys) exp_kind
  = tc_tuple hs_ty tup_sort tys exp_kind

--------- Promoted lists and tuples
tc_hs_type hs_ty@(HsExplicitListTy _k tys) exp_kind
  = do { tks <- mapM tc_infer_lhs_type tys
       ; let taus = map fst tks
       ; kind <- unifyKinds (ptext (sLit "In a promoted list")) tks
       ; checkExpectedKind hs_ty (mkPromotedListTy kind) exp_kind
       ; return (foldr (mk_cons kind) (mk_nil kind) taus) }
  where
    mk_cons k a b = mkTyConApp (promoteDataCon consDataCon) [k, a, b]
    mk_nil  k     = mkTyConApp (promoteDataCon nilDataCon) [k]

tc_hs_type hs_ty@(HsExplicitTupleTy _ tys) exp_kind
  = do { tks <- mapM tc_infer_lhs_type tys
       ; let n          = length tys
             kind_con   = promotedTupleTyCon   BoxedTuple n
             ty_con     = promotedTupleDataCon BoxedTuple n
             (taus, ks) = unzip tks
             tup_k      = mkTyConApp kind_con ks
       ; checkExpectedKind hs_ty tup_k exp_kind
       ; return (mkTyConApp ty_con (ks ++ taus)) }

--------- Constraint types
tc_hs_type ipTy@(HsIParamTy n ty) exp_kind
  = do { ty' <- tc_lhs_type ty ekLifted
       ; checkExpectedKind ipTy constraintKind exp_kind
       ; ipClass <- tcLookupClass ipClassName
       ; let n' = mkStrLitTy $ hsIPNameFS n
       ; return (mkClassPred ipClass [n',ty'])
       }

tc_hs_type ty@(HsEqTy ty1 ty2) exp_kind 
  = do { (ty1', kind1) <- tc_infer_lhs_type ty1
       ; (ty2', kind2) <- tc_infer_lhs_type ty2
       ; checkExpectedKind ty2 kind2
              (EK kind1 msg_fn)
       ; checkExpectedKind ty constraintKind exp_kind
       ; return (mkNakedTyConApp eqTyCon [kind1, ty1', ty2']) }
  where
    msg_fn pkind = ptext (sLit "The left argument of the equality had kind")
                   <+> quotes (pprKind pkind)

--------- Misc
tc_hs_type (HsKindSig ty sig_k) exp_kind 
  = do { sig_k' <- tcLHsKind sig_k
       ; checkExpectedKind ty sig_k' exp_kind
       ; tc_lhs_type ty (EK sig_k' msg_fn) }
  where
    msg_fn pkind = ptext (sLit "The signature specified kind") 
                   <+> quotes (pprKind pkind)

tc_hs_type (HsCoreTy ty) exp_kind
  = do { checkExpectedKind ty (typeKind ty) exp_kind
       ; return ty }


#ifdef GHCI	/* Only if bootstrapped */
-- This looks highly suspect to me
-- It will really only be fixed properly when we do the TH
-- reorganisation so that type splices happen in the renamer
tc_hs_type hs_ty@(HsSpliceTy sp fvs _) exp_kind 
  = do { s <- getStage
       ; traceTc "tc_hs_type: splice" (ppr sp $$ ppr s) 
       ; (ty, kind) <- tcSpliceType sp fvs
       ; checkExpectedKind hs_ty kind exp_kind
--                     -- See Note [Kind of a type splice]
       ; return ty }
#else
tc_hs_type ty@(HsSpliceTy {}) _exp_kind 
  = failWithTc (ptext (sLit "Unexpected type splice:") <+> ppr ty)
#endif

tc_hs_type (HsWrapTy {}) _exp_kind 
  = panic "tc_hs_type HsWrapTy"  -- We kind checked something twice

tc_hs_type hs_ty@(HsTyLit (HsNumTy n)) exp_kind 
  = do { checkExpectedKind hs_ty typeNatKind exp_kind
       ; checkWiredInTyCon typeNatKindCon
       ; return (mkNumLitTy n) }

tc_hs_type hs_ty@(HsTyLit (HsStrTy s)) exp_kind 
  = do { checkExpectedKind hs_ty typeSymbolKind exp_kind
       ; checkWiredInTyCon typeSymbolKindCon
       ; return (mkStrLitTy s) }

---------------------------
tc_tuple :: HsType Name -> HsTupleSort -> [LHsType Name] -> ExpKind -> TcM TcType
-- Invariant: tup_sort is not HsBoxedOrConstraintTuple
tc_tuple hs_ty tup_sort tys exp_kind
  = do { tau_tys <- tc_hs_arg_tys cxt_doc tys (repeat arg_kind)
       ; finish_tuple hs_ty tup_sort tau_tys exp_kind }
  where
    arg_kind = case tup_sort of
                 HsBoxedTuple      -> liftedTypeKind
                 HsUnboxedTuple    -> openTypeKind
                 HsConstraintTuple -> constraintKind
                 _                 -> panic "tc_hs_type arg_kind"
    cxt_doc = case tup_sort of
                 HsBoxedTuple      -> ptext (sLit "a tuple")
                 HsUnboxedTuple    -> ptext (sLit "an unboxed tuple")
                 HsConstraintTuple -> ptext (sLit "a constraint tuple")
                 _                 -> panic "tc_hs_type tup_sort"

finish_tuple :: HsType Name -> HsTupleSort -> [TcType] -> ExpKind -> TcM TcType
finish_tuple hs_ty tup_sort tau_tys exp_kind
  = do { checkExpectedKind hs_ty res_kind exp_kind
       ; checkWiredInTyCon tycon
       ; return (mkTyConApp tycon tau_tys) }
  where
    tycon = tupleTyCon con (length tau_tys)
    con = case tup_sort of
            HsUnboxedTuple    -> UnboxedTuple
            HsBoxedTuple      -> BoxedTuple
            HsConstraintTuple -> ConstraintTuple
            _                 -> panic "tc_hs_type HsTupleTy"

    res_kind = case tup_sort of
                 HsUnboxedTuple    -> unliftedTypeKind
                 HsBoxedTuple      -> liftedTypeKind
                 HsConstraintTuple -> constraintKind
                 _                 -> panic "tc_hs_type arg_kind"

---------------------------
tcInferApps :: Outputable a
       => a 
       -> TcKind			-- Function kind
       -> [LHsType Name]		-- Arg types
       -> TcM ([TcType], TcKind)	-- Kind-checked args
tcInferApps the_fun fun_kind args
  = do { (args_w_kinds, res_kind) <- splitFunKind (ppr the_fun) fun_kind args
       ; args' <- tc_lhs_types args_w_kinds
       ; return (args', res_kind) }

tcCheckApps :: Outputable a 
            => HsType Name     -- The type being checked (for err messages only)
            -> a               -- The function
            -> TcKind -> [LHsType Name]   -- Fun kind and arg types
	    -> ExpKind 	                  -- Expected kind
	    -> TcM [TcType]
tcCheckApps hs_ty the_fun fun_kind args exp_kind
  = do { (arg_tys, res_kind) <- tcInferApps the_fun fun_kind args
       ; checkExpectedKind hs_ty res_kind exp_kind
       ; return arg_tys }

---------------------------
splitFunKind :: SDoc -> TcKind -> [b] -> TcM ([(b,ExpKind)], TcKind)
splitFunKind the_fun fun_kind args
  = go 1 fun_kind args
  where
    go _      fk [] = return ([], fk)
    go arg_no fk (arg:args)
       = do { mb_fk <- matchExpectedFunKind fk
            ; case mb_fk of
                 Nothing       -> failWithTc too_many_args 
                 Just (ak,fk') -> do { (aks, rk) <- go (arg_no+1) fk' args
                                     ; let exp_kind = expArgKind (quotes the_fun) ak arg_no
                                     ; return ((arg, exp_kind) : aks, rk) } }
 
    too_many_args = quotes the_fun <+>
		    ptext (sLit "is applied to too many type arguments")


---------------------------
tcHsContext :: LHsContext Name -> TcM [PredType]
tcHsContext ctxt = mapM tcHsLPredType (unLoc ctxt)

tcHsLPredType :: LHsType Name -> TcM PredType
tcHsLPredType pred = tc_lhs_type pred ekConstraint

---------------------------
tcTyVar :: Name -> TcM (TcType, TcKind)
-- See Note [Type checking recursive type and class declarations]
-- in TcTyClsDecls
tcTyVar name         -- Could be a tyvar, a tycon, or a datacon
  = do { traceTc "lk1" (ppr name)
       ; thing <- tcLookup name
       ; traceTc "lk2" (ppr name <+> ppr thing)
       ; case thing of
           ATyVar _ tv 
              | isKindVar tv
              -> failWithTc (ptext (sLit "Kind variable") <+> quotes (ppr tv)
                             <+> ptext (sLit "used as a type"))
              | otherwise
              -> return (mkTyVarTy tv, tyVarKind tv)

           AThing kind -> do { tc <- get_loopy_tc name
                             ; inst_tycon (mkNakedTyConApp tc) kind }
                             -- mkNakedTyConApp: see Note [Zonking inside the knot]

           AGlobal (ATyCon tc) -> inst_tycon (mkTyConApp tc) (tyConKind tc)

           AGlobal (ADataCon dc)
             | Just tc <- promoteDataCon_maybe dc
             -> do { data_kinds <- xoptM Opt_DataKinds
                   ; unless data_kinds $ promotionErr name NoDataKinds
                   ; inst_tycon (mkTyConApp tc) (tyConKind tc) }
             | otherwise -> failWithTc (ptext (sLit "Data constructor") <+> quotes (ppr dc)
                                        <+> ptext (sLit "comes from an un-promotable type") 
                                        <+> quotes (ppr (dataConTyCon dc)))

           APromotionErr err -> promotionErr name err

           _  -> wrongThingErr "type" thing name }
  where
    get_loopy_tc name
      = do { env <- getGblEnv
           ; case lookupNameEnv (tcg_type_env env) name of
                Just (ATyCon tc) -> return tc
                _                -> return (aThingErr "tcTyVar" name) }

    inst_tycon :: ([Type] -> Type) -> Kind -> TcM (Type, Kind)
    -- Instantiate the polymorphic kind
    -- Lazy in the TyCon
    inst_tycon mk_tc_app kind
      | null kvs 
      = return (mk_tc_app [], ki_body)
      | otherwise
      = do { traceTc "lk4" (ppr name <+> dcolon <+> ppr kind)
           ; ks <- mapM (const newMetaKindVar) kvs
           ; return (mk_tc_app ks, substKiWith kvs ks ki_body) }
      where 
        (kvs, ki_body) = splitForAllTys kind

tcClass :: Name -> TcM (Class, TcKind)
tcClass cls 	-- Must be a class
  = do { thing <- tcLookup cls
       ; case thing of
           AThing kind -> return (aThingErr "tcClass" cls, kind)
           AGlobal (ATyCon tc)
             | Just cls <- tyConClass_maybe tc 
             -> return (cls, tyConKind tc)
           _ -> wrongThingErr "class" thing cls }


aThingErr :: String -> Name -> b
-- The type checker for types is sometimes called simply to
-- do *kind* checking; and in that case it ignores the type
-- returned. Which is a good thing since it may not be available yet!
aThingErr str x = pprPanic "AThing evaluated unexpectedly" (text str <+> ppr x)
\end{code}

Note [Zonking inside the knot]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are checking the argument types of a data constructor.  We
must zonk the types before making the DataCon, because once built we
can't change it.  So we must traverse the type.

BUT the parent TyCon is knot-tied, so we can't look at it yet. 

So we must be careful not to use "smart constructors" for types that
look at the TyCon or Class involved.  Hence the use of mkNakedXXX
functions.

This is sadly delicate.

Note [Body kind of a forall]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The body of a forall is usually a type, but in principle
there's no reason to prohibit *unlifted* types.
In fact, GHC can itself construct a function with an
unboxed tuple inside a for-all (via CPR analyis; see 
typecheck/should_compile/tc170).

Moreover in instance heads we get forall-types with
kind Constraint.  

Moreover if we have a signature
   f :: Int#
then we represent it as (HsForAll Implicit [] [] Int#).  And this must
be legal!  We can't drop the empty forall until *after* typechecking
the body because of kind polymorphism:
   Typeable :: forall k. k -> Constraint
   data Apply f t = Apply (f t)
   -- Apply :: forall k. (k -> *) -> k -> *
   instance Typeable Apply where ...
Then the dfun has type
   df :: forall k. Typeable ((k->*) -> k -> *) (Apply k)

   f :: Typeable Apply

   f :: forall (t:k->*) (a:k).  t a -> t a

   class C a b where
      op :: a b -> Typeable Apply

   data T a = MkT (Typeable Apply)
            | T2 a
      T :: * -> *
      MkT :: forall k. (Typeable ((k->*) -> k -> *) (Apply k)) -> T a

   f :: (forall (k:BOX). forall (t:: k->*) (a:k). t a -> t a) -> Int
   f :: (forall a. a -> Typeable Apply) -> Int

So we *must* keep the HsForAll on the instance type
   HsForAll Implicit [] [] (Typeable Apply)
so that we do kind generalisation on it.

Really we should check that it's a type of value kind
{*, Constraint, #}, but I'm not doing that yet
Example that should be rejected:  
         f :: (forall (a:*->*). a) Int

Note [Inferring tuple kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Give a tuple type (a,b,c), which the parser labels as HsBoxedOrConstraintTuple,
we try to figure out whether it's a tuple of kind * or Constraint.
  Step 1: look at the expected kind
  Step 2: infer argument kinds

If after Step 2 it's not clear from the arguments that it's
Constraint, then it must be *.  Once having decided that we re-check
the Check the arguments again to give good error messages
in eg. `(Maybe, Maybe)`

Note that we will still fail to infer the correct kind in this case:

  type T a = ((a,a), D a)
  type family D :: Constraint -> Constraint

While kind checking T, we do not yet know the kind of D, so we will default the
kind of T to * -> *. It works if we annotate `a` with kind `Constraint`.

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
addTypeCtxt :: LHsType Name -> TcM a -> TcM a
	-- Wrap a context around only if we want to show that contexts.  
	-- Omit invisble ones and ones user's won't grok
addTypeCtxt (L _ ty) thing 
  = addErrCtxt doc thing
  where
    doc = ptext (sLit "In the type") <+> quotes (ppr ty)
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
kcScopedKindVars :: [Name] -> TcM a -> TcM a
-- Given some tyvar binders like [a (b :: k -> *) (c :: k)]
-- bind each scoped kind variable (k in this case) to a fresh
-- kind skolem variable
kcScopedKindVars kv_ns thing_inside 
  = do { kvs <- mapM (\n -> newSigTyVar n superKind) kv_ns
                     -- NB: use mutable signature variables
       ; tcExtendTyVarEnv2 (kv_ns `zip` kvs) thing_inside } 

kcHsTyVarBndrs :: Bool    -- True <=> full kind signature provided
                          -- Default UserTyVar to *
                          -- and use KindVars not meta kind vars
               -> LHsTyVarBndrs Name 
	       -> ([TcKind] -> TcM r)
	       -> TcM r
-- Used in getInitialKind
kcHsTyVarBndrs full_kind_sig (HsQTvs { hsq_kvs = kv_ns, hsq_tvs = hs_tvs }) thing_inside
  = do { kvs <- if full_kind_sig 
                then return (map mkKindSigVar kv_ns)
                else mapM (\n -> newSigTyVar n superKind) kv_ns
       ; tcExtendTyVarEnv2 (kv_ns `zip` kvs) $
    do { nks <- mapM (kc_hs_tv . unLoc) hs_tvs
       ; tcExtendKindEnv nks (thing_inside (map snd nks)) } }
  where
    kc_hs_tv :: HsTyVarBndr Name -> TcM (Name, TcKind)
    kc_hs_tv (UserTyVar n)     
      = do { mb_thing <- tcLookupLcl_maybe n
           ; kind <- case mb_thing of
               	       Just (AThing k)   -> return k
               	       _ | full_kind_sig -> return liftedTypeKind
               	         | otherwise     -> newMetaKindVar
           ; return (n, kind) }
    kc_hs_tv (KindedTyVar n k) 
      = do { kind <- tcLHsKind k
               -- In an associated type decl, the type variable may already 
               -- be in scope; in that case we want to make sure its kind
               -- matches the one declared here
           ; mb_thing <- tcLookupLcl_maybe n
           ; case mb_thing of
               Nothing          -> return ()
               Just (AThing ks) -> checkKind kind ks
               Just thing       -> pprPanic "check_in_scope" (ppr thing)
           ; return (n, kind) }

tcScopedKindVars :: [Name] -> TcM a -> TcM a
-- Given some tyvar binders like [a (b :: k -> *) (c :: k)]
-- bind each scoped kind variable (k in this case) to a fresh
-- kind skolem variable
tcScopedKindVars kv_ns thing_inside 
  = tcExtendTyVarEnv (map mkKindSigVar kv_ns) thing_inside

tcHsTyVarBndrs :: LHsTyVarBndrs Name 
	       -> ([TcTyVar] -> TcM r)
	       -> TcM r
-- Bind the type variables to skolems, each with a meta-kind variable kind
tcHsTyVarBndrs (HsQTvs { hsq_kvs = kvs, hsq_tvs = hs_tvs }) thing_inside
  = tcScopedKindVars kvs $
    do { tvs <- mapM tcHsTyVarBndr hs_tvs
       ; traceTc "tcHsTyVarBndrs" (ppr hs_tvs $$ ppr tvs)
       ; tcExtendTyVarEnv tvs (thing_inside tvs) }

tcHsTyVarBndr :: LHsTyVarBndr Name -> TcM TcTyVar
-- Return a type variable 
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
-- See Note [Associated type tyvar names] in Class
tcHsTyVarBndr (L _ hs_tv)
  = do { let name = hsTyVarName hs_tv
       ; mb_tv <- tcLookupLcl_maybe name
       ; case mb_tv of {
           Just (ATyVar _ tv) -> return tv ;
           _ -> do
       { kind <- case hs_tv of
                   UserTyVar {}       -> newMetaKindVar
                   KindedTyVar _ kind -> tcLHsKind kind
       ; return (mkTcTyVar name kind (SkolemTv False)) } } }

------------------
kindGeneralize :: TyVarSet -> [Name] -> TcM [KindVar]
kindGeneralize tkvs _names_to_avoid
  = do { gbl_tvs <- tcGetGlobalTyVars -- Already zonked
       ; tkvs    <- zonkTyVarsAndFV tkvs
       ; let kvs_to_quantify = filter isKindVar (varSetElems (tkvs `minusVarSet` gbl_tvs))
                -- ToDo: remove the (filter isKindVar)
                -- Any type variables in tkvs will be in scope,
                -- and hence in gbl_tvs, so after removing gbl_tvs
                -- we should only have kind variables left
		--
 		-- BUT there is a smelly case (to be fixed when TH is reorganised)
		--     f t = [| e :: $t |]
                -- When typechecking the body of the bracket, we typecheck $t to a
                -- unification variable 'alpha', with no biding forall.  We don't
                -- want to kind-quantify it!

       ; traceTc "kindGeneralise" (vcat [ppr kvs_to_quantify])
       ; ASSERT2 (all isKindVar kvs_to_quantify, ppr kvs_to_quantify $$ ppr tkvs)
             -- This assertion is obviosly true because of the filter isKindVar
             -- but we'll remove that when reorganising TH, and then the assertion
             -- will mean something

             -- If we tidied the kind variables, which should all be mutable,
             -- this 'zonkQuantifiedTyVars' update the original TyVar to point to
             -- the tided and skolemised one
         zonkQuantifiedTyVars kvs_to_quantify }
\end{code}

Note [Kind generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We do kind generalisation only at the outer level of a type signature.
For example, consider
  T :: forall k. k -> *
  f :: (forall a. T a -> Int) -> Int
When kind-checking f's type signature we generalise the kind at
the outermost level, thus:
  f1 :: forall k. (forall (a:k). T k a -> Int) -> Int  -- YES!
and *not* at the inner forall:
  f2 :: (forall k. forall (a:k). T k a -> Int) -> Int  -- NO!
Reason: same as for HM inference on value level declarations,
we want to infer the most general type.  The f2 type signature
would be *less applicable* than f1, becuase it requires a more
polymorphic argument.

Note [Kinds of quantified type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcTyVarBndrsGen quantifies over a specified list of type variables,
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

Hence using zonked_kinds when forming tvs'.

\begin{code}
--------------------
-- getInitialKind has made a suitably-shaped kind for the type or class
-- Unpack it, and attribute those kinds to the type variables
-- Extend the env with bindings for the tyvars, taken from
-- the kind of the tycon/class.  Give it to the thing inside, and 
-- check the result kind matches
kcLookupKind :: Name -> TcM Kind
kcLookupKind nm 
  = do { tc_ty_thing <- tcLookup nm
       ; case tc_ty_thing of
           AThing k            -> return k
           AGlobal (ATyCon tc) -> return (tyConKind tc)
           _                   -> pprPanic "kcLookupKind" (ppr tc_ty_thing) }

kcTyClTyVars :: Name -> LHsTyVarBndrs Name -> TcM a -> TcM a
-- Used for the type variables of a type or class decl,
-- when doing the initial kind-check.  
kcTyClTyVars name (HsQTvs { hsq_kvs = kvs, hsq_tvs = hs_tvs }) thing_inside
  = kcScopedKindVars kvs $
    do 	{ tc_kind <- kcLookupKind name
	; let (arg_ks, _res_k) = splitKindFunTysN (length hs_tvs) tc_kind
                     -- There should be enough arrows, because
                     -- getInitialKinds used the tcdTyVars
        ; name_ks <- zipWithM kc_tv hs_tvs arg_ks
        ; tcExtendKindEnv name_ks thing_inside }
  where
    -- getInitialKind has already gotten the kinds of these type
    -- variables, but tiresomely we need to check them *again* 
    -- to match the kind variables they mention against the ones 
    -- we've freshly brought into scope
    kc_tv :: LHsTyVarBndr Name -> Kind -> TcM (Name, Kind)
    kc_tv (L _ (UserTyVar n)) exp_k 
      = return (n, exp_k)
    kc_tv (L _ (KindedTyVar n hs_k)) exp_k
      = do { k <- tcLHsKind hs_k
           ; checkKind k exp_k
           ; return (n, exp_k) }

-----------------------
tcTyClTyVars :: Name -> LHsTyVarBndrs Name	-- LHS of the type or class decl
             -> ([TyVar] -> Kind -> TcM a) -> TcM a
-- Used for the type variables of a type or class decl,
-- on the second pass when constructing the final result
-- (tcTyClTyVars T [a,b] thing_inside) 
--   where T : forall k1 k2 (a:k1 -> *) (b:k1). k2 -> *
--   calls thing_inside with arguments
--      [k1,k2,a,b] (k2 -> *)
--   having also extended the type environment with bindings 
--   for k1,k2,a,b
--
-- No need to freshen the k's because they are just skolem 
-- constants here, and we are at top level anyway.
tcTyClTyVars tycon (HsQTvs { hsq_kvs = hs_kvs, hsq_tvs = hs_tvs }) thing_inside
  = kcScopedKindVars hs_kvs $ -- Bind scoped kind vars to fresh kind univ vars
                              -- There may be fewer of these than the kvs of
                              -- the type constructor, of course
    do { thing <- tcLookup tycon
       ; let { kind = case thing of
                        AThing kind -> kind
                        _ -> panic "tcTyClTyVars"
                     -- We only call tcTyClTyVars during typechecking in
                     -- TcTyClDecls, where the local env is extended with
                     -- the generalized_env (mapping Names to AThings).
             ; (kvs, body)  = splitForAllTys kind
             ; (kinds, res) = splitKindFunTysN (length hs_tvs) body }
       ; tvs <- zipWithM tc_hs_tv hs_tvs kinds
       ; tcExtendTyVarEnv tvs (thing_inside (kvs ++ tvs) res) }
  where
    tc_hs_tv (L _ (UserTyVar n))        kind = return (mkTyVar n kind)
    tc_hs_tv (L _ (KindedTyVar n hs_k)) kind = do { tc_kind <- tcLHsKind hs_k
                                                  ; checkKind kind tc_kind
                                                  ; return (mkTyVar n kind) }

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
	       -> HsWithBndrs (LHsType Name)  -- The type signature
	      -> TcM ( Type                   -- The signature
                      , [(Name, TcTyVar)] )   -- The new bit of type environment, binding
				              -- the scoped type variables
-- Used for type-checking type signatures in
-- (a) patterns 	  e.g  f (x::Int) = e
-- (b) result signatures  e.g. g x :: Int = e
-- (c) RULE forall bndrs  e.g. forall (x::Int). f x = x

tcHsPatSigType ctxt (HsWB { hswb_cts = hs_ty, hswb_kvs = sig_kvs, hswb_tvs = sig_tvs })
  = addErrCtxt (pprHsSigCtxt ctxt hs_ty) $
    do	{ kvs <- mapM new_kv sig_kvs
        ; tvs <- mapM new_tv sig_tvs
        ; let ktv_binds = (sig_kvs `zip` kvs) ++ (sig_tvs `zip` tvs)
	; sig_ty <- tcExtendTyVarEnv2 ktv_binds $
                    tcHsLiftedType hs_ty
        ; sig_ty <- zonkTcType sig_ty
	; checkValidType ctxt sig_ty 
	; return (sig_ty, ktv_binds) }
  where
    new_kv name = new_tkv name superKind
    new_tv name = do { kind <- newMetaKindVar
                     ; new_tkv name kind }

    new_tkv name kind   -- See Note [Pattern signature binders]
      = case ctxt of
          RuleSigCtxt {} -> return (mkTcTyVar name kind (SkolemTv False))
          _              -> newSigTyVar name kind  -- See Note [Unifying SigTvs]

tcPatSig :: UserTypeCtxt
	 -> HsWithBndrs (LHsType Name)
	 -> TcSigmaType
	 -> TcM (TcType,	    -- The type to use for "inside" the signature
		 [(Name, TcTyVar)], -- The new bit of type environment, binding
				    -- the scoped type variables
                 HsWrapper)         -- Coercion due to unification with actual ty
                                    -- Of shape:  res_ty ~ sig_ty
tcPatSig ctxt sig res_ty
  = do	{ (sig_ty, sig_tvs) <- tcHsPatSigType ctxt sig
    	-- sig_tvs are the type variables free in 'sig', 
	-- and not already in scope. These are the ones
	-- that should be brought into scope

	; if null sig_tvs then do {
		-- Just do the subsumption check and return
                  wrap <- tcSubType PatSigOrigin ctxt res_ty sig_ty
		; return (sig_ty, [], wrap)
        } else do
		-- Type signature binds at least one scoped type variable
	
		-- A pattern binding cannot bind scoped type variables
                -- It is more convenient to make the test here
                -- than in the renamer
	{ let in_pat_bind = case ctxt of
				BindPatSigCtxt -> True
				_              -> False
	; when in_pat_bind (addErr (patBindSigErr sig_tvs))

		-- Check that all newly-in-scope tyvars are in fact
		-- constrained by the pattern.  This catches tiresome
		-- cases like	
		--	type T a = Int
		--	f :: Int -> Int
		-- 	f (x :: T a) = ...
		-- Here 'a' doesn't get a binding.  Sigh
	; let bad_tvs = [ tv | (_, tv) <- sig_tvs
                             , not (tv `elemVarSet` exactTyVarsOfType sig_ty) ]
	; checkTc (null bad_tvs) (badPatSigTvs sig_ty bad_tvs)

	-- Now do a subsumption check of the pattern signature against res_ty
	; wrap <- tcSubType PatSigOrigin ctxt res_ty sig_ty

	-- Phew!
        ; return (sig_ty, sig_tvs, wrap)
        } }

patBindSigErr :: [(Name, TcTyVar)] -> SDoc
patBindSigErr sig_tvs 
  = hang (ptext (sLit "You cannot bind scoped type variable") <> plural sig_tvs
          <+> pprQuotedList (map fst sig_tvs))
       2 (ptext (sLit "in a pattern binding signature"))
\end{code}

Note [Pattern signature binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T = forall a. T a (a->Int)
   f (T x (f :: a->Int) = blah)

Here 
 * The pattern (T p1 p2) creates a *skolem* type variable 'a_sk', 
   It must be a skolem so that that it retains its identity, and    
   TcErrors.getSkolemInfo can thereby find the binding site for the skolem.

 * The type signature pattern (f :: a->Int) binds "a" -> a_sig in the envt

 * Then unificaiton makes a_sig := a_sk

That's why we must make a_sig a MetaTv (albeit a SigTv), 
not a SkolemTv, so that it can unify to a_sk.

For RULE binders, though, things are a bit different (yuk).  
  RULE "foo" forall (x::a) (y::[a]).  f x y = ...
Here this really is the binding site of the type variable so we'd like
to use a skolem, so that we get a complaint if we unify two of them
together.

Note [Unifying SigTvs]
~~~~~~~~~~~~~~~~~~~~~~
ALAS we have no decent way of avoiding two SigTvs getting unified.  
Consider
  f (x::(a,b)) (y::c)) = [fst x, y]
Here we'd really like to complain that 'a' and 'c' are unified. But
for the reasons above we can't make a,b,c into skolems, so they
are just SigTvs that can unify.  And indeed, this would be ok,
  f x (y::c) = case x of
                 (x1 :: a1, True) -> [x,y]
                 (x1 :: a2, False) -> [x,y,y]
Here the type of x's first component is called 'a1' in one branch and
'a2' in the other.  We could try insisting on the same OccName, but
they definitely won't have the sane lexical Name. 

I think we could solve this by recording in a SigTv a list of all the 
in-scope varaibles that it should not unify with, but it's fiddly.


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
data ExpKind = EK TcKind (TcKind -> SDoc)
   -- The second arg is function that takes a *tidied* version 
   -- of the first arg, and produces something like
   --    "Expected kind k"
   --    "Expected a constraint"
   --    "The argument of Maybe should have kind k"

instance Outputable ExpKind where
  ppr (EK k f) = f k

ekLifted, ekOpen, ekConstraint :: ExpKind
ekLifted     = EK liftedTypeKind expectedKindMsg
ekOpen       = EK openTypeKind   expectedKindMsg
ekConstraint = EK constraintKind expectedKindMsg

expectedKindMsg :: TcKind -> SDoc
expectedKindMsg pkind
  | isConstraintKind pkind = ptext (sLit "Expected a constraint")
  | isOpenTypeKind   pkind = ptext (sLit "Expected a type")
  | otherwise = ptext (sLit "Expected kind") <+> quotes (pprKind pkind)

-- Build an ExpKind for arguments
expArgKind :: SDoc -> TcKind -> Int -> ExpKind
expArgKind exp kind arg_no = EK kind msg_fn
  where
    msg_fn pkind 
      = sep [ ptext (sLit "The") <+> speakNth arg_no 
              <+> ptext (sLit "argument of") <+> exp
            , nest 2 $ ptext (sLit "should have kind") 
              <+> quotes (pprKind pkind) ]

unifyKinds :: SDoc -> [(TcType, TcKind)] -> TcM TcKind
unifyKinds fun act_kinds
  = do { kind <- newMetaKindVar
       ; let check (arg_no, (ty, act_kind)) 
               = checkExpectedKind ty act_kind (expArgKind (quotes fun) kind arg_no)
       ; mapM_ check (zip [1..] act_kinds)
       ; return kind }

checkKind :: TcKind -> TcKind -> TcM ()
checkKind act_kind exp_kind
  = do { mb_subk <- unifyKindX act_kind exp_kind
       ; case mb_subk of
           Just EQ -> return ()
           _       -> unifyKindMisMatch act_kind exp_kind }

checkExpectedKind :: Outputable a => a -> TcKind -> ExpKind -> TcM ()
-- A fancy wrapper for 'unifyKindX', which tries
-- to give decent error messages.
--      (checkExpectedKind ty act_kind exp_kind)
-- checks that the actual kind act_kind is compatible
--      with the expected kind exp_kind
-- The first argument, ty, is used only in the error message generation
checkExpectedKind ty act_kind (EK exp_kind ek_ctxt)
 = do { mb_subk <- unifyKindX act_kind exp_kind

         -- Kind unification only generates definite errors
      ; case mb_subk of {
          Just LT -> return () ;    -- act_kind is a sub-kind of exp_kind
          Just EQ -> return () ;    -- The two are equal
          _other  -> do

      {  -- So there's an error
         -- Now to find out what sort
        exp_kind <- zonkTcKind exp_kind
      ; act_kind <- zonkTcKind act_kind
      ; traceTc "checkExpectedKind" (ppr ty $$ ppr act_kind $$ ppr exp_kind)
      ; env0 <- tcInitTidyEnv
      ; dflags <- getDynFlags
      ; let (exp_as, _) = splitKindFunTys exp_kind
            (act_as, _) = splitKindFunTys act_kind
            n_exp_as  = length exp_as
            n_act_as  = length act_as
            n_diff_as = n_act_as - n_exp_as

            (env1, tidy_exp_kind) = tidyOpenKind env0 exp_kind
            (env2, tidy_act_kind) = tidyOpenKind env1 act_kind

            occurs_check 
               | Just act_tv <- tcGetTyVar_maybe act_kind
               = check_occ act_tv exp_kind
               | Just exp_tv <- tcGetTyVar_maybe exp_kind
               = check_occ exp_tv act_kind
               | otherwise 
               = False

            check_occ tv k = case occurCheckExpand dflags tv k of
                                OC_Occurs -> True
                                _bad      -> False

            err | isLiftedTypeKind exp_kind && isUnliftedTypeKind act_kind
                = ptext (sLit "Expecting a lifted type, but") <+> quotes (ppr ty)
                    <+> ptext (sLit "is unlifted")

                | isUnliftedTypeKind exp_kind && isLiftedTypeKind act_kind
                = ptext (sLit "Expecting an unlifted type, but") <+> quotes (ppr ty)
                    <+> ptext (sLit "is lifted")

                | occurs_check   -- Must precede the "more args expected" check
                = ptext (sLit "Kind occurs check") $$ more_info

                | n_exp_as < n_act_as     -- E.g. [Maybe]
                = vcat [ ptext (sLit "Expecting") <+>
                         speakN n_diff_as <+> ptext (sLit "more argument")
                         <> (if n_diff_as > 1 then char 's' else empty)
                         <+> ptext (sLit "to") <+> quotes (ppr ty)
                       , more_info ]

                  -- Now n_exp_as >= n_act_as. In the next two cases,
                  -- n_exp_as == 0, and hence so is n_act_as
                | otherwise               -- E.g. Monad [Int]
                = more_info

            more_info = sep [ ek_ctxt tidy_exp_kind <> comma
                            , nest 2 $ ptext (sLit "but") <+> quotes (ppr ty)
                              <+> ptext (sLit "has kind") <+> quotes (pprKind tidy_act_kind)]

      ; traceTc "checkExpectedKind 1" (ppr ty $$ ppr tidy_act_kind $$ ppr tidy_exp_kind $$ ppr env1 $$ ppr env2)
      ; failWithTcM (env2, err) } } }
\end{code}

%************************************************************************
%*                                                                      *
        Sort checking kinds
%*                                                                      *
%************************************************************************

tcLHsKind converts a user-written kind to an internal, sort-checked kind.
It does sort checking and desugaring at the same time, in one single pass.
It fails when the kinds are not well-formed (eg. data A :: * Int), or if there
are non-promotable or non-fully applied kinds.

\begin{code}
tcLHsKind :: LHsKind Name -> TcM Kind
tcLHsKind k = addErrCtxt (ptext (sLit "In the kind") <+> quotes (ppr k)) $
              tc_lhs_kind k

tc_lhs_kind :: LHsKind Name -> TcM Kind
tc_lhs_kind (L span ki) = setSrcSpan span (tc_hs_kind ki)

-- The main worker
tc_hs_kind :: HsKind Name -> TcM Kind
tc_hs_kind (HsTyVar tc)    = tc_kind_var_app tc []
tc_hs_kind k@(HsAppTy _ _) = tc_kind_app k []

tc_hs_kind (HsParTy ki) = tc_lhs_kind ki

tc_hs_kind (HsFunTy ki1 ki2) =
  do kappa_ki1 <- tc_lhs_kind ki1
     kappa_ki2 <- tc_lhs_kind ki2
     return (mkArrowKind kappa_ki1 kappa_ki2)

tc_hs_kind (HsListTy ki) =
  do kappa <- tc_lhs_kind ki
     checkWiredInTyCon listTyCon
     return $ mkPromotedListTy kappa

tc_hs_kind (HsTupleTy _ kis) =
  do kappas <- mapM tc_lhs_kind kis
     checkWiredInTyCon tycon
     return $ mkTyConApp tycon kappas
  where 
     tycon = promotedTupleTyCon BoxedTuple (length kis)

-- Argument not kind-shaped
tc_hs_kind k = pprPanic "tc_hs_kind" (ppr k)

-- Special case for kind application
tc_kind_app :: HsKind Name -> [LHsKind Name] -> TcM Kind
tc_kind_app (HsAppTy ki1 ki2) kis = tc_kind_app (unLoc ki1) (ki2:kis)
tc_kind_app (HsTyVar tc)      kis = do { arg_kis <- mapM tc_lhs_kind kis
                                       ; tc_kind_var_app tc arg_kis }
tc_kind_app ki                _   = failWithTc (quotes (ppr ki) <+> 
                                    ptext (sLit "is not a kind constructor"))

tc_kind_var_app :: Name -> [Kind] -> TcM Kind
-- Special case for * and Constraint kinds
-- They are kinds already, so we don't need to promote them
tc_kind_var_app name arg_kis
  |  name == liftedTypeKindTyConName
  || name == constraintKindTyConName
  = do { unless (null arg_kis)
           (failWithTc (text "Kind" <+> ppr name <+> text "cannot be applied"))
       ; thing <- tcLookup name
       ; case thing of
           AGlobal (ATyCon tc) -> return (mkTyConApp tc [])
           _                   -> panic "tc_kind_var_app 1" }

-- General case
tc_kind_var_app name arg_kis
  = do { thing <- tcLookup name
       ; case thing of
  	   AGlobal (ATyCon tc)
  	     -> do { data_kinds <- xoptM Opt_DataKinds
  	           ; unless data_kinds $ addErr (dataKindsErr name)
  	     	   ; case promotableTyCon_maybe tc of
  	     	       Just prom_tc | arg_kis `lengthIs` tyConArity prom_tc
  	     	               -> return (mkTyConApp prom_tc arg_kis)
  	     	       Just _  -> tycon_err tc "is not fully applied"
  	     	       Nothing -> tycon_err tc "is not promotable" }

  	   -- A lexically scoped kind variable
  	   ATyVar _ kind_var 
             | not (isKindVar kind_var) 
             -> failWithTc (ptext (sLit "Type variable") <+> quotes (ppr kind_var)
                            <+> ptext (sLit "used as a kind"))
             | not (null arg_kis) -- Kind variables always have kind BOX, 
                                  -- so cannot be applied to anything
             -> failWithTc (ptext (sLit "Kind variable") <+> quotes (ppr name)
                            <+> ptext (sLit "cannot appear in a function position"))
             | otherwise 
             -> return (mkAppTys (mkTyVarTy kind_var) arg_kis)

  	   -- It is in scope, but not what we expected
  	   AThing _ 
             | isTyVarName name 
             -> failWithTc (ptext (sLit "Type variable") <+> quotes (ppr name)
                            <+> ptext (sLit "used in a kind"))
             | otherwise 
             -> failWithTc (hang (ptext (sLit "Type constructor") <+> quotes (ppr name)
                                  <+> ptext (sLit "used in a kind"))
  	                       2 (ptext (sLit "inside its own recursive group"))) 

           APromotionErr err -> promotionErr name err

  	   _ -> wrongThingErr "promoted type" thing name
                -- This really should not happen
     }
  where 
   tycon_err tc msg = failWithTc (quotes (ppr tc) <+> ptext (sLit "of kind")
                                  <+> quotes (ppr (tyConKind tc)) <+> ptext (sLit msg))

dataKindsErr :: Name -> SDoc
dataKindsErr name
  = hang (ptext (sLit "Illegal kind:") <+> quotes (ppr name))
       2 (ptext (sLit "Perhaps you intended to use -XDataKinds"))

promotionErr :: Name -> PromotionErr -> TcM a
promotionErr name err
  = failWithTc (hang (pprPECategory err <+> quotes (ppr name) <+> ptext (sLit "cannot be used here"))
                   2 (parens reason))
  where
    reason = case err of
               FamDataConPE -> ptext (sLit "it comes from a data family instance")
               NoDataKinds  -> ptext (sLit "Perhaps you intended to use -XDataKinds")
               _ -> ptext (sLit "it is defined and used in the same recursive group")
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

unifyKindMisMatch :: TcKind -> TcKind -> TcM a
unifyKindMisMatch ki1 ki2 = do
    ki1' <- zonkTcKind ki1
    ki2' <- zonkTcKind ki2
    let msg = hang (ptext (sLit "Couldn't match kind"))
              2 (sep [quotes (ppr ki1'),
                      ptext (sLit "against"),
                      quotes (ppr ki2')])
    failWithTc msg
\end{code}


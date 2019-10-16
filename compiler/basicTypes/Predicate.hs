{-

Describes predicates as they are considered by the solver.

-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Predicate (
  Pred(..), UserPred(..),
  classifyPredType, classifyUserPredType,
  isPredTy, isEvVarType,
  predType, fromEqPred, fromEqPreds,
  toUserPred, toUserPreds, fromUserPred, fromUserPreds,
  userPredType, mkPhiTy,

  -- Equality predicates
  EqPred(..), mkEqPred, mkEqualityPred, mkEqPredRole,
  EqRel(..), eqRelRole, eqPredEqRel,
  isLiftedEqPred, liftEqPred,
  predEqRel,
  mkPrimEqPred, mkReprPrimEqPred, mkPrimEqPredRole,
  mkHeteroPrimEqPred, mkHeteroReprPrimEqPred,
  eqPredType, isEqPred,

  -- Class predicates
  mkClassPred, mkClassPredTy, isDictTy,
  isClassUserPred, isEqPredClass, isCTupleClass,
  getClassPredTys, getClassPredTys_maybe,

  -- Forall predicates
  mkDFunPred,

  -- Implicit parameters
  isIPPred, isIPUserPred, isIPTyCon, isIPClass, hasIPPred,

  -- Evidence variables
  DictId, isEvVar, isDictId,

  -- Equality
  eqUserPred, eqUserPreds, eqPred, nonDetCmpUserPred,
  eqUserPredX,

  -- Tidying
  tidyPred, tidyUserPred, tidyOpenUserPred,

  -- Substitution
  substPred, substPreds, substPredUnchecked,
  substTheta, substThetaUnchecked,
  substUserPred, substEqPred,

  -- Free variables
  tyCoVarsOfUserPred, tyCoVarsOfUserPreds,
  tyCoVarsOfUserPredList, tyCoVarsOfUserPredsList,
  tyCoVarsOfPreds, tyCoVarsOfPred, tyCoVarsOfPredList, tyCoVarsOfPredsList,
  tyCoFVsOfPred,
  tyCoVarsOfEqPred, tyCoVarsOfEqPreds,
  tyCoVarsOfEqPredList,
  coVarsOfUserPred, coVarsOfUserPreds,
  exactTyCoVarsOfUserPred, exactTyCoVarsOfPred, exactTyCoVarsOfPreds,

  -- Printing
  pprTheta, pprParendTheta, pprThetaArrowTy, pprPredsContext,
  pprParendPredsContext, pprClassPred, pprUserPred, pprPred,
  pprParendPred, pprEqPred
  ) where

#include "HsVersions.h"

import GhcPrelude

import Type
import TyCoSubst
import TyCoPpr
import TyCoFVs
import GHC.Iface.Type
import {-# SOURCE #-} GHC.CoreToIface ( toIfacePredX )
import Class
import TyCon
import Var
import VarSet
import VarEnv  ( TidyEnv, emptyTidyEnv, RnEnv2 )
import OccName ( initTidyOccEnv )
import Name    ( getOccName )
import FV
import Coercion
import CoAxiom ( EqRel(..), eqRelRole )

import PrelNames
import {-# SOURCE #-} TysWiredIn ( coercibleClass, heqClass )

import Outputable
import Util

import Data.List     ( mapAccumL )
import Data.Data     ( Data )

-- | A predicate in the solver. The solver tries to prove Wanted predicates
-- from Given ones.
data Pred
  = EqualityPred {-# UNPACK #-} !EqPred
  | UserPred !UserPred
  deriving Data

-- | Predicates the user might write. These all have user-facing types.
data UserPred
  = ClassPred Class [Type]
  | IrredPred PredType
  | ForAllPred [TyVar] [UserPred] UserPred
    deriving Data
  -- ForAllPred: see Note [Quantified constraints] in TcCanonical
  -- NB: There is no TuplePred case
  --     Tuple predicates like (Eq a, Ord b) are just treated
  --     as ClassPred, as if we had a tuple class with two superclasses
  --        class (c1, c2) => (%,%) c1 c2

-- | An equality predicate works at an equality relation and relates
-- two types.
data EqPred = EqPred EqRel Type Type
  deriving Data

classifyPredType :: PredType -> Pred
classifyPredType ev_ty = case splitTyConApp_maybe ev_ty of
    Just (tc, [_, _, ty1, ty2])
      | tc `hasKey` eqReprPrimTyConKey -> EqualityPred $ EqPred ReprEq ty1 ty2
      | tc `hasKey` eqPrimTyConKey     -> EqualityPred $ EqPred NomEq ty1 ty2

    _ -> fromUserPred $ classifyUserPredType ev_ty

classifyUserPredType :: PredType -> UserPred
classifyUserPredType ev_ty
  = ASSERT( tcIsConstraintKind (tcTypeKind ev_ty) )  -- rules out ~#
    case splitTyConApp_maybe ev_ty of
    Just (tc, tys)
      | Just clas <- tyConClass_maybe tc
      -> mkClassPred clas tys

    _ | (tvs, rho) <- splitForAllTys ev_ty
      , (theta, pred) <- splitFunTys rho
      , not (null tvs && null theta)
      -> ForAllPred tvs (map classifyUserPredType theta)
                        (classifyUserPredType pred)

      | otherwise
      -> IrredPred ev_ty

-- | Given a Pred, convert it into a Type.
-- Inverse of 'classifyPredType'.
predType :: Pred -> PredType
predType (EqualityPred eq_pred) = eqPredType eq_pred
predType (UserPred u_pred)      = userPredType u_pred

eqPredType :: EqPred -> Type
eqPredType (EqPred eq_rel ty1 ty2) = mkPrimEqPredEqRel eq_rel ty1 ty2

userPredType :: UserPred -> Type
userPredType (ClassPred cls tys) = mkTyConApp (classTyCon cls) tys
userPredType (IrredPred ty) = ty
userPredType (ForAllPred tvbs premises conclusion)
  = mkSpecForAllTys tvbs $
    mkInvisFunTys (map userPredType premises) $
    userPredType conclusion

fromEqPred :: EqPred -> Pred
fromEqPred = EqualityPred

fromEqPreds :: [EqPred] -> [Pred]
fromEqPreds = map fromEqPred

fromUserPred :: UserPred -> Pred
fromUserPred = UserPred

fromUserPreds :: [UserPred] -> [Pred]
fromUserPreds = map fromUserPred

-- | Unconditionally convert a 'Pred' into a 'UserPred'. Panics
-- if this is not possible.
toUserPred :: String -> Pred -> UserPred
toUserPred _err_str (UserPred u) = u
toUserPred err_str  _other       = pprPanic err_str (ppr _other)

-- | Unconditionally convert a list of 'Pred' into a list of 'UserPred'. Panics
-- if this is not possible.
toUserPreds :: String -> [Pred] -> [UserPred]
toUserPreds err_str = map (toUserPred err_str)

-- | Build a qualified type.
mkPhiTy :: [UserPred] -> Type -> Type
mkPhiTy preds = mkInvisFunTys (map userPredType preds)

-- --------------------- Dictionary types ---------------------------------

mkClassPred :: Class -> [Type] -> UserPred
mkClassPred = ClassPred

mkClassPredTy :: Class -> [Type] -> PredType
mkClassPredTy clas tys = mkTyConApp (classTyCon clas) tys

isDictTy :: Type -> Bool
isDictTy ty
  | Just (tc, _) <- tcSplitTyConApp_maybe ty
  = isClassTyCon tc

  | otherwise
  = False

getClassPredTys :: HasDebugCallStack => Pred -> (Class, [Type])
getClassPredTys pred = case getClassPredTys_maybe pred of
        Just (clas, tys) -> (clas, tys)
        Nothing          -> pprPanic "getClassPredTys" (ppr pred)

getClassPredTys_maybe :: Pred -> Maybe (Class, [Type])
getClassPredTys_maybe (UserPred (ClassPred clas tys)) = Just (clas, tys)
getClassPredTys_maybe _                               = Nothing

-- iff the variables or givens are non-empty, make a Forall; otherwise,
-- just return the conclusion.
-- Precondition: if either the tvs or theta is non-empty, the conclusion
-- is a UserPred
mkDFunPred :: [TyVar] -> [UserPred] -> Pred -> Pred
mkDFunPred tvs theta pred
  | null tvs && null theta
  = pred
  | otherwise
  = fromUserPred $ ForAllPred tvs theta (toUserPred "mkDFunPred" pred)

-- --------------------- Equality predicates ---------------------------------

mkEqPred :: EqRel -> Type -> Type -> EqPred
mkEqPred = EqPred

mkEqualityPred :: EqRel -> Type -> Type -> Pred
mkEqualityPred eq_rel ty1 ty2 = EqualityPred (EqPred eq_rel ty1 ty2)

mkEqPredRole :: Role -> Type -> Type -> EqPred
mkEqPredRole Phantom ty1 ty2
  = pprPanic "mkEqPredRole sees a phantom" (ppr ty1 $$ ppr ty2)
mkEqPredRole Representational ty1 ty2 = mkEqPred ReprEq ty1 ty2
mkEqPredRole Nominal ty1 ty2          = mkEqPred NomEq ty1 ty2

-- | Get the equality relation from an EqPred
eqPredEqRel :: EqPred -> EqRel
eqPredEqRel (EqPred rel _ _) = rel

mkPrimEqPredEqRel :: EqRel -> Type -> Type -> PredType
mkPrimEqPredEqRel eq_rel ty1 ty2
  = mkPrimEqPredRole (eqRelRole eq_rel) ty1 ty2

liftEqPred :: EqPred -> UserPred
liftEqPred (EqPred NomEq ty1 ty2) = ClassPred heqClass [k1, k2, ty1, ty2]
  where k1 = typeKind ty1
        k2 = typeKind ty2
liftEqPred (EqPred ReprEq ty1 ty2) = ASSERT( typeKind ty2 `eqType` k )
                                     ClassPred coercibleClass [k, ty1, ty2]
  where k = typeKind ty1

-- | Get the relevant 'EqRel' for a 'Pred'. Used e.g. in flattening to know
-- how to flatten components of a predicate.
predEqRel :: Pred -> EqRel
predEqRel (EqualityPred (EqPred rel _ _)) = rel
predEqRel _other                          = NomEq

{-------------------------------------------
Predicates on Predicates and PredType
--------------------------------------------}

{-
Note [Evidence for quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclass mechanism in TcCanonical.makeSuperClasses risks
taking a quantified constraint like
   (forall a. C a => a ~ b)
and generate superclass evidence
   (forall a. C a => a ~# b)

This is a funny thing: neither isPredTy nor isCoVarType are true
of it.  So we are careful not to generate it in the first place:
see Note [Equality superclasses in quantified constraints]
in TcCanonical.
-}

isEvVarType :: Type -> Bool
-- True of (a) predicates, of kind Constraint, such as (Eq a), and (a ~ b)
--         (b) coercion types, such as (t1 ~# t2) or (t1 ~R# t2)
-- See Note [Types for coercions, predicates, and evidence] in TyCoRep
-- See Note [Evidence for quantified constraints]
isEvVarType ty = isCoVarType ty || isPredTy ty

isEqPredClass :: Class -> Bool
-- True of (~) and (~~)
isEqPredClass cls =  cls `hasKey` eqTyConKey
                  || cls `hasKey` heqTyConKey

isClassUserPred :: UserPred -> Bool
isClassUserPred (ClassPred {}) = True
isClassUserPred _              = False

isIPPred :: Pred -> Bool
isIPPred (UserPred u) = isIPUserPred u
isIPPred _            = False

isIPUserPred :: UserPred -> Bool
isIPUserPred (ClassPred cls _) = isIPClass cls
isIPUserPred _                 = False

-- | True of @(a ~ b)@ and @(a ~~ b)@
isLiftedEqPred :: UserPred -> Bool
isLiftedEqPred (ClassPred cls _)
  = isEqPredClass cls
isLiftedEqPred _ = False

isIPTyCon :: TyCon -> Bool
isIPTyCon tc = tc `hasKey` ipClassKey
  -- Class and its corresponding TyCon have the same Unique

isIPClass :: Class -> Bool
isIPClass cls = cls `hasKey` ipClassKey

isCTupleClass :: Class -> Bool
isCTupleClass cls = isTupleTyCon (classTyCon cls)

hasIPPred :: PredType -> Bool
hasIPPred pred = case splitTyConApp_maybe pred of
  Just (tc, tys)
    | isIPTyCon tc -> True
    | isTupleTyCon tc -> any hasIPPred tys
  _other -> False

isEqPred :: Pred -> Bool
isEqPred (EqualityPred {}) = True
isEqPred _                 = False

{-
************************************************************************
*                                                                      *
              Evidence variables
*                                                                      *
************************************************************************
-}

isEvVar :: Var -> Bool
isEvVar var = isEvVarType (varType var)

isDictId :: Id -> Bool
isDictId id = isDictTy (varType id)

{-
************************************************************************
*                                                                      *
            Free variables
*                                                                      *
************************************************************************
-}

tyCoVarsOfUserPred :: UserPred -> TyCoVarSet
tyCoVarsOfUserPred = tyCoVarsOfType . userPredType

tyCoVarsOfUserPreds :: [UserPred] -> TyCoVarSet
tyCoVarsOfUserPreds = mapUnionVarSet tyCoVarsOfUserPred

tyCoVarsOfUserPredList :: UserPred -> [TyCoVar]
tyCoVarsOfUserPredList = tyCoVarsOfTypeList . userPredType

tyCoVarsOfUserPredsList :: [UserPred] -> [TyCoVar]
tyCoVarsOfUserPredsList = tyCoVarsOfTypesList . map userPredType

tyCoVarsOfEqPred :: EqPred -> TyCoVarSet
tyCoVarsOfEqPred (EqPred _ ty1 ty2) = tyCoVarsOfTypes [ty1, ty2]

tyCoVarsOfEqPreds :: [EqPred] -> TyCoVarSet
tyCoVarsOfEqPreds = mapUnionVarSet tyCoVarsOfEqPred

tyCoVarsOfEqPredList :: EqPred -> [TyCoVar]
tyCoVarsOfEqPredList (EqPred _ ty1 ty2) = tyCoVarsOfTypesList [ty1, ty2]

tyCoVarsOfPreds :: [Pred] -> TyCoVarSet
tyCoVarsOfPreds = mapUnionVarSet tyCoVarsOfPred

tyCoVarsOfPred :: Pred -> TyCoVarSet
tyCoVarsOfPred (EqualityPred eq_pred) = tyCoVarsOfEqPred eq_pred
tyCoVarsOfPred (UserPred u_pred)      = tyCoVarsOfUserPred u_pred

tyCoVarsOfPredList :: Pred -> [TyCoVar]
tyCoVarsOfPredList (EqualityPred eq_pred) = tyCoVarsOfEqPredList eq_pred
tyCoVarsOfPredList (UserPred u_pred)      = tyCoVarsOfUserPredList u_pred

tyCoVarsOfPredsList :: [Pred] -> [TyCoVar]
tyCoVarsOfPredsList = tyCoVarsOfTypesList . concatMap all_types
  where
    all_types (EqualityPred (EqPred _ ty1 ty2)) = [ty1, ty2]
    all_types (UserPred u_pred)                 = [userPredType u_pred]

tyCoFVsOfPred :: Pred -> FV
tyCoFVsOfPred pred a b c = tyCoFVsOfType (predType pred) a b c

tyCoVarsOfPredWellScoped :: Pred -> [TyVar]
tyCoVarsOfPredWellScoped = tyCoVarsOfTypeWellScoped . predType

tyCoVarsOfUserPredWellScoped :: UserPred -> [TyVar]
tyCoVarsOfUserPredWellScoped = tyCoVarsOfTypeWellScoped . userPredType

coVarsOfUserPred :: UserPred -> CoVarSet
coVarsOfUserPred = coVarsOfType . userPredType

coVarsOfUserPreds :: [UserPred] -> CoVarSet
coVarsOfUserPreds = coVarsOfTypes . map userPredType

exactTyCoVarsOfUserPred :: UserPred -> TyCoVarSet
exactTyCoVarsOfUserPred = exactTyCoVarsOfType . userPredType

exactTyCoVarsOfPred :: Pred -> TyCoVarSet
exactTyCoVarsOfPred = exactTyCoVarsOfType . predType

exactTyCoVarsOfPreds :: [Pred] -> TyCoVarSet
exactTyCoVarsOfPreds = exactTyCoVarsOfTypes . map predType

{-
************************************************************************
*                                                                      *
            Equality
*                                                                      *
************************************************************************
-}

eqUserPred :: UserPred -> UserPred -> Bool
eqUserPred u1 u2 = userPredType u1 `eqType` userPredType u2

eqUserPreds :: [UserPred] -> [UserPred] -> Bool
eqUserPreds (u1:us1) (u2:us2) = u1 `eqUserPred` u2 && us1 `eqUserPreds` us2
eqUserPreds []       []       = True
eqUserPreds _        _        = False

eqEqPred :: EqPred -> EqPred -> Bool
eqEqPred (EqPred r1 ty11 ty21) (EqPred r2 ty12 ty22)
  = r1 == r2 && ty11 `eqType` ty12 && ty21 `eqType` ty22

eqPred :: Pred -> Pred -> Bool
eqPred (EqualityPred eq1) (EqualityPred eq2) = eqEqPred eq1 eq2
eqPred (UserPred u1)      (UserPred u2)      = eqUserPred u1 u2
eqPred _                  _                  = False

nonDetCmpUserPred :: UserPred -> UserPred -> Ordering
nonDetCmpUserPred pred1 pred2 = nonDetCmpType (userPredType pred1) (userPredType pred2)

-- | Compare UserPreds with respect to a (presumably) non-empty 'RnEnv2'.
eqUserPredX :: RnEnv2 -> UserPred -> UserPred -> Bool
eqUserPredX env p1 p2 = eqTypeX env (userPredType p1) (userPredType p2)
  -- It's OK to use nonDetCmpType here and eqTypeX is deterministic,
  -- nonDetCmpTypeX does equality deterministically

{-
************************************************************************
*                                                                      *
            Substitution
*                                                                      *
************************************************************************
-}

substPred :: TCvSubst -> Pred -> Pred
substPred subst (EqualityPred eq_pred) = EqualityPred (substEqPred subst eq_pred)
substPred subst (UserPred u_pred)      = UserPred (substUserPred subst u_pred)

substPredUnchecked :: TCvSubst -> Pred -> Pred
substPredUnchecked subst (EqualityPred eq_pred)
  = EqualityPred (substEqPredUnchecked subst eq_pred)
substPredUnchecked subst (UserPred u_pred)
  = UserPred (substUserPredUnchecked subst u_pred)

substPreds :: TCvSubst -> [Pred] -> [Pred]
substPreds subst = map (substPred subst)

substEqPred :: TCvSubst -> EqPred -> EqPred
substEqPred subst (EqPred r ty1 ty2) = EqPred r (substTy subst ty1)
                                                (substTy subst ty2)

substEqPredUnchecked :: TCvSubst -> EqPred -> EqPred
substEqPredUnchecked subst (EqPred r ty1 ty2) = EqPred r (substTyUnchecked subst ty1)
                                                         (substTyUnchecked subst ty2)

-- | Substitute within a theta -- that is, a list of 'UserPred's
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant] in TyCoRep.
substTheta :: HasCallStack => TCvSubst -> [UserPred] -> [UserPred]
substTheta subst
  | isEmptyTCvSubst subst = id
  | otherwise             = map (substUserPred subst)

-- | Substitute within a theta, disabling the sanity checks.
-- The problems that the sanity checks in substTys catch are described in
-- Note [The substitution invariant] in TyCoRep.
-- The goal of #11371 is to migrate all the calls of substThetaUnchecked to
-- substTheta and remove this function. Please don't use in new code.
substThetaUnchecked :: TCvSubst -> [UserPred] -> [UserPred]
substThetaUnchecked subst
  | isEmptyTCvSubst subst = id
  | otherwise             = map (substUserPredUnchecked subst)

substUserPred :: TCvSubst -> UserPred -> UserPred
substUserPred subst (ClassPred cls tys) = ClassPred cls (substTys subst tys)
substUserPred subst (IrredPred ty) = IrredPred (substTy subst ty)
substUserPred subst (ForAllPred tvbs premises conclusion) =
  let (subst', tvbs') = substTyVarBndrs subst tvbs
      premises'       = substTheta subst' premises
      conclusion'     = substUserPred  subst' conclusion in
  ForAllPred tvbs' premises' conclusion'

substUserPredUnchecked :: TCvSubst -> UserPred -> UserPred
substUserPredUnchecked subst (ClassPred cls tys) = ClassPred cls (substTysUnchecked subst tys)
substUserPredUnchecked subst (IrredPred ty) = IrredPred (substTyUnchecked subst ty)
substUserPredUnchecked subst (ForAllPred tvbs premises conclusion) =
  let (subst', tvbs') = mapAccumL substVarBndrUnchecked subst tvbs
      premises'       = substThetaUnchecked subst' premises
      conclusion'     = substUserPredUnchecked  subst' conclusion in
  ForAllPred tvbs' premises' conclusion'

{-
************************************************************************
*                                                                      *
            Tidying
*                                                                      *
************************************************************************
-}

tidyPred :: TidyEnv -> Pred -> Pred
tidyPred env (EqualityPred (EqPred role ty1 ty2)) = EqualityPred (EqPred role ty1' ty2')
  where ty1' = tidyType env ty1
        ty2' = tidyType env ty2
tidyPred env (UserPred u) = UserPred $ tidyUserPred env u

tidyUserPred :: TidyEnv -> UserPred -> UserPred
tidyUserPred env (ClassPred cls tys)         = ClassPred cls (tidyTypes env tys)
tidyUserPred env (IrredPred pred_ty)         = IrredPred (tidyType env pred_ty)
tidyUserPred env (ForAllPred tvs theta head) = ForAllPred tvs' theta' head'
  where (env', tvs') = tidyVarBndrs env tvs
        theta'       = tidyUserPreds env' theta
        head'        = tidyUserPred env' head

tidyUserPreds :: TidyEnv -> [UserPred] -> [UserPred]
tidyUserPreds env = map (tidyUserPred env)

-- cf. tidyOpenTypes
tidyOpenUserPred :: TidyEnv -> UserPred -> (TidyEnv, UserPred)
tidyOpenUserPred env pred
  = (env', tidyUserPred (trimmed_occ_env, var_env) pred)
  where
    (env'@(_, var_env), tvs') = tidyOpenTyCoVars env $
                                tyCoVarsOfUserPredWellScoped pred
    trimmed_occ_env = initTidyOccEnv (map getOccName tvs')
      -- The idea here was that we restrict the new TidyEnv to the
      -- _free_ vars of the types, so that we don't gratuitously rename
      -- the _bound_ variables of the types.

tidyToIfacePred :: Pred -> IfacePred
tidyToIfacePred = tidyToIfacePredX emptyTidyEnv

tidyToIfaceUserPred :: UserPred -> IfaceType -- converts to IfaceType because
                                             -- this is just used for pretty-printing
tidyToIfaceUserPred = tidyToIfaceTypeX emptyTidyEnv . userPredType

tidyToIfacePredX :: TidyEnv -> Pred -> IfacePred
tidyToIfacePredX env ty = toIfacePredX (mkVarSet free_tcvs) (tidyPred env' ty)
  where
    env'      = tidyFreeTyCoVars env free_tcvs
    free_tcvs = tyCoVarsOfPredWellScoped ty

{-
************************************************************************
*                                                                      *
            Printing
*                                                                      *
************************************************************************
-}

-- | Works over 'Pred', not just 'UserPred', so it can be used
-- to print messages containing info about constraints in the solver
pprPredsContext :: [Pred] -> SDoc
pprPredsContext = pprIfaceContextPreds topPrec . map tidyToIfacePred

-- | Like 'pprPredsContext', but puts parens over anything that isn't
-- atomic.
pprParendPredsContext :: [Pred] -> SDoc
pprParendPredsContext = pprIfaceContextPreds appPrec . map tidyToIfacePred

-- | Prints e.g. @(Show a, Eq a)@.
pprTheta :: [UserPred] -> SDoc
pprTheta = pprIfaceContext topPrec . map tidyToIfaceUserPred

pprParendTheta :: [UserPred] -> SDoc
pprParendTheta = pprIfaceContext appPrec . map tidyToIfaceUserPred

-- | Print e.g. @(Show a, Eq a) =>@. 'UserPred's only.
pprThetaArrowTy :: [UserPred] -> SDoc
pprThetaArrowTy = pprIfaceContextArr . map tidyToIfaceUserPred

pprClassPred :: Class -> [Type] -> SDoc
pprClassPred clas tys = pprTypeApp (classTyCon clas) tys

-- | Renders a 'UserPred' into an 'SDoc'
pprUserPred :: UserPred -> SDoc
pprUserPred = ppr . userPredType

-- | Renders a 'Pred' into an 'SDoc'
pprPred :: Pred -> SDoc
pprPred = pprIfacePred . tidyToIfacePred

-- | Renders a 'Pred' into an 'SDoc', using parentheses if not atomic
pprParendPred :: Pred -> SDoc
pprParendPred = pprParendIfacePred . tidyToIfacePred

-- | Renders an 'EqPred' into an 'SDoc'
pprEqPred :: EqPred -> SDoc
pprEqPred = pprPred . fromEqPred   -- this is a bit inefficient, but so much easier

instance Outputable EqPred where
  ppr = pprEqPred

instance Outputable UserPred where
  ppr = pprUserPred

instance Outputable Pred where
  ppr = pprPred

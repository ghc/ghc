{-

Describes predicates as they are considered by the solver.

-}

module GHC.Core.Predicate (
  Pred(..), classifyPredType,
  isPredTy, isEvVarType,

  -- Equality predicates
  EqRel(..), eqRelRole,
  isEqPrimPred, isEqPred,
  getEqPredTys, getEqPredTys_maybe, getEqPredRole,
  predTypeEqRel,
  mkPrimEqPred, mkReprPrimEqPred, mkPrimEqPredRole,
  mkHeteroPrimEqPred, mkHeteroReprPrimEqPred,

  -- Class predicates
  mkClassPred, isDictTy,
  isClassPred, isEqPredClass, isCTupleClass,
  getClassPredTys, getClassPredTys_maybe,

  -- Implicit parameters
  isIPPred, isIPPred_maybe, isIPTyCon, isIPClass, hasIPPred,

  -- Evidence variables
  DictId, isEvVar, isDictId
  ) where

import GHC.Prelude

import GHC.Core.Type
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Types.Var
import GHC.Core.Coercion

import GHC.Builtin.Names

import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Misc

import Control.Monad ( guard )

-- | A predicate in the solver. The solver tries to prove Wanted predicates
-- from Given ones.
data Pred
  = ClassPred Class [Type]
  | EqPred EqRel Type Type
  | IrredPred PredType
  | ForAllPred [TyVar] [PredType] PredType
     -- ForAllPred: see Note [Quantified constraints] in GHC.Tc.Solver.Canonical
  -- NB: There is no TuplePred case
  --     Tuple predicates like (Eq a, Ord b) are just treated
  --     as ClassPred, as if we had a tuple class with two superclasses
  --        class (c1, c2) => (%,%) c1 c2

classifyPredType :: PredType -> Pred
classifyPredType ev_ty = case splitTyConApp_maybe ev_ty of
    Just (tc, [_, _, ty1, ty2])
      | tc `hasKey` eqReprPrimTyConKey -> EqPred ReprEq ty1 ty2
      | tc `hasKey` eqPrimTyConKey     -> EqPred NomEq ty1 ty2

    Just (tc, tys)
      | Just clas <- tyConClass_maybe tc
      -> ClassPred clas tys

    _ | (tvs, rho) <- splitForAllTys ev_ty
      , (theta, pred) <- splitFunTys rho
      , not (null tvs && null theta)
      -> ForAllPred tvs theta pred

      | otherwise
      -> IrredPred ev_ty

-- --------------------- Dictionary types ---------------------------------

mkClassPred :: Class -> [Type] -> PredType
mkClassPred clas tys = mkTyConApp (classTyCon clas) tys

isDictTy :: Type -> Bool
isDictTy = isClassPred

getClassPredTys :: HasDebugCallStack => PredType -> (Class, [Type])
getClassPredTys ty = case getClassPredTys_maybe ty of
        Just (clas, tys) -> (clas, tys)
        Nothing          -> pprPanic "getClassPredTys" (ppr ty)

getClassPredTys_maybe :: PredType -> Maybe (Class, [Type])
getClassPredTys_maybe ty = case splitTyConApp_maybe ty of
        Just (tc, tys) | Just clas <- tyConClass_maybe tc -> Just (clas, tys)
        _ -> Nothing

-- --------------------- Equality predicates ---------------------------------

-- | A choice of equality relation. This is separate from the type 'Role'
-- because 'Phantom' does not define a (non-trivial) equality relation.
data EqRel = NomEq | ReprEq
  deriving (Eq, Ord)

instance Outputable EqRel where
  ppr NomEq  = text "nominal equality"
  ppr ReprEq = text "representational equality"

eqRelRole :: EqRel -> Role
eqRelRole NomEq  = Nominal
eqRelRole ReprEq = Representational

getEqPredTys :: PredType -> (Type, Type)
getEqPredTys ty
  = case splitTyConApp_maybe ty of
      Just (tc, [_, _, ty1, ty2])
        |  tc `hasKey` eqPrimTyConKey
        || tc `hasKey` eqReprPrimTyConKey
        -> (ty1, ty2)
      _ -> pprPanic "getEqPredTys" (ppr ty)

getEqPredTys_maybe :: PredType -> Maybe (Role, Type, Type)
getEqPredTys_maybe ty
  = case splitTyConApp_maybe ty of
      Just (tc, [_, _, ty1, ty2])
        | tc `hasKey` eqPrimTyConKey     -> Just (Nominal, ty1, ty2)
        | tc `hasKey` eqReprPrimTyConKey -> Just (Representational, ty1, ty2)
      _ -> Nothing

getEqPredRole :: PredType -> Role
getEqPredRole ty = eqRelRole (predTypeEqRel ty)

-- | Get the equality relation relevant for a pred type.
predTypeEqRel :: PredType -> EqRel
predTypeEqRel ty
  | Just (tc, _) <- splitTyConApp_maybe ty
  , tc `hasKey` eqReprPrimTyConKey
  = ReprEq
  | otherwise
  = NomEq

{-------------------------------------------
Predicates on PredType
--------------------------------------------}

{-
Note [Evidence for quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclass mechanism in GHC.Tc.Solver.Canonical.makeSuperClasses risks
taking a quantified constraint like
   (forall a. C a => a ~ b)
and generate superclass evidence
   (forall a. C a => a ~# b)

This is a funny thing: neither isPredTy nor isCoVarType are true
of it.  So we are careful not to generate it in the first place:
see Note [Equality superclasses in quantified constraints]
in GHC.Tc.Solver.Canonical.
-}

isEvVarType :: Type -> Bool
-- True of (a) predicates, of kind Constraint, such as (Eq a), and (a ~ b)
--         (b) coercion types, such as (t1 ~# t2) or (t1 ~R# t2)
-- See Note [Types for coercions, predicates, and evidence] in GHC.Core.TyCo.Rep
-- See Note [Evidence for quantified constraints]
isEvVarType ty = isCoVarType ty || isPredTy ty

isEqPredClass :: Class -> Bool
-- True of (~) and (~~)
isEqPredClass cls =  cls `hasKey` eqTyConKey
                  || cls `hasKey` heqTyConKey

isClassPred, isEqPred, isEqPrimPred, isIPPred :: PredType -> Bool
isClassPred ty = case tyConAppTyCon_maybe ty of
    Just tyCon | isClassTyCon tyCon -> True
    _                               -> False

isEqPred ty  -- True of (a ~ b) and (a ~~ b)
             -- ToDo: should we check saturation?
  | Just tc <- tyConAppTyCon_maybe ty
  , Just cls <- tyConClass_maybe tc
  = isEqPredClass cls
  | otherwise
  = False

isEqPrimPred ty = isCoVarType ty
  -- True of (a ~# b) (a ~R# b)

isIPPred ty = case tyConAppTyCon_maybe ty of
    Just tc -> isIPTyCon tc
    _       -> False

isIPTyCon :: TyCon -> Bool
isIPTyCon tc = tc `hasKey` ipClassKey
  -- Class and its corresponding TyCon have the same Unique

isIPClass :: Class -> Bool
isIPClass cls = cls `hasKey` ipClassKey

isCTupleClass :: Class -> Bool
isCTupleClass cls = isTupleTyCon (classTyCon cls)

isIPPred_maybe :: Type -> Maybe (FastString, Type)
isIPPred_maybe ty =
  do (tc,[t1,t2]) <- splitTyConApp_maybe ty
     guard (isIPTyCon tc)
     x <- isStrLitTy t1
     return (x,t2)

hasIPPred :: PredType -> Bool
hasIPPred pred
  = case classifyPredType pred of
      ClassPred cls tys
        | isIPClass     cls -> True
        | isCTupleClass cls -> any hasIPPred tys
      _other -> False

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

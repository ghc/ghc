{-# LANGUAGE DerivingStrategies #-}

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

  -- Special predicates
  SpecialPred(..), specialPredTyCon,

  -- Class predicates
  mkClassPred, isDictTy,
  isClassPred, isEqPredClass, isCTupleClass,
  getClassPredTys, getClassPredTys_maybe,
  classMethodTy, classMethodInstTy,

  -- Implicit parameters
  isIPLikePred, hasIPSuperClasses, isIPTyCon, isIPClass,
  isCallStackTy, isCallStackPred, isCallStackPredTy,
  isIPPred_maybe,

  -- Evidence variables
  DictId, isEvVar, isDictId
  ) where

import GHC.Prelude

import GHC.Core.Type
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.TyCon.RecWalk
import GHC.Types.Var
import GHC.Core.Coercion
import GHC.Core.Multiplicity ( scaledThing )

import GHC.Builtin.Names
import GHC.Builtin.Types.Prim ( concretePrimTyCon )

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Data.FastString

import Control.Monad ( guard )


-- | A predicate in the solver. The solver tries to prove Wanted predicates
-- from Given ones.
data Pred

  -- | A typeclass predicate.
  = ClassPred Class [Type]

  -- | A type equality predicate.
  | EqPred EqRel Type Type

  -- | An irreducible predicate.
  | IrredPred PredType

  -- | A quantified predicate.
  --
  -- See Note [Quantified constraints] in GHC.Tc.Solver.Canonical
  | ForAllPred [TyVar] [PredType] PredType

  -- | A special predicate, used internally in GHC.
  --
  -- The meaning of the type argument is dictated by the 'SpecialPred'
  -- specified in the first agument; see the documentation of 'SpecialPred' for more info.
  --
  -- Example: @Concrete# rep@, used for representation-polymorphism checks
  -- within GHC. See Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete.
  -- (This is the only example currently. More to come: see GHC ticket #20000.)
  | SpecialPred SpecialPred Type

  -- NB: There is no TuplePred case
  --     Tuple predicates like (Eq a, Ord b) are just treated
  --     as ClassPred, as if we had a tuple class with two superclasses
  --        class (c1, c2) => (%,%) c1 c2

classifyPredType :: PredType -> Pred
classifyPredType ev_ty = case splitTyConApp_maybe ev_ty of
    Just (tc, [_, _, ty1, ty2])
      | tc `hasKey` eqReprPrimTyConKey -> EqPred ReprEq ty1 ty2
      | tc `hasKey` eqPrimTyConKey     -> EqPred NomEq ty1 ty2

    Just (tc, [_ki, ty])
      | tc `hasKey` concretePrimTyConKey
      -> SpecialPred ConcretePrimPred ty

    Just (tc, tys)
      | Just clas <- tyConClass_maybe tc
      -> ClassPred clas tys

    _ | (tvs, rho) <- splitForAllTyCoVars ev_ty
      , (theta, pred) <- splitFunTys rho
      , not (null tvs && null theta)
      -> ForAllPred tvs (map scaledThing theta) pred

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

classMethodTy :: Id -> Type
-- Takes a class selector op :: forall a. C a => meth_ty
-- and returns the type of its method, meth_ty
-- The selector can be a superclass selector, in which case
-- you get back a superclass
classMethodTy sel_id
  = funResultTy $        -- meth_ty
    dropForAlls $        -- C a => meth_ty
    varType sel_id        -- forall a. C n => meth_ty

classMethodInstTy :: Id -> [Type] -> Type
-- Takes a class selector op :: forall a b. C a b => meth_ty
-- and the types [ty1, ty2] at which it is instantiated,
-- returns the instantiated type of its method, meth_ty[t1/a,t2/b]
-- The selector can be a superclass selector, in which case
-- you get back a superclass
classMethodInstTy sel_id arg_tys
  = funResultTy $
    piResultTys (varType sel_id) arg_tys

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

-- --------------------- Special predicates ----------------------------------

-- | 'SpecialPred' describes all the special predicates
-- that are currently used in GHC.
--
-- These are different from the special typeclasses
-- (such as `KnownNat`, `Typeable`, `Coercible`, ...), as special predicates
-- can't be expressed as typeclasses, as they hold evidence of a different kind.
data SpecialPred
  -- | A @Concrete#@ predicate, to check for representation polymorphism.
  --
  -- When the first argument to the 'SpecialPred' data constructor of 'Pred'
  -- is 'ConcretePrimPred', the second argument is the type we are inspecting
  -- to decide whether it is concrete. That is, it refers to the
  -- second argument of the 'Concrete#' 'TyCon'. Recall that this 'TyCon' has kind
  --
  -- > forall k. k -> TupleRep '[]
  --
  -- See Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete for further details.
  = ConcretePrimPred
  deriving stock Eq

instance Outputable SpecialPred where
  ppr ConcretePrimPred = text "Concrete#"

-- | Obtain the 'TyCon' associated with a special predicate.
specialPredTyCon :: SpecialPred -> TyCon
specialPredTyCon ConcretePrimPred = concretePrimTyCon

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

isClassPred, isEqPred, isEqPrimPred :: PredType -> Bool
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

isCTupleClass :: Class -> Bool
isCTupleClass cls = isTupleTyCon (classTyCon cls)


{- *********************************************************************
*                                                                      *
              Implicit parameters
*                                                                      *
********************************************************************* -}

isIPTyCon :: TyCon -> Bool
isIPTyCon tc = tc `hasKey` ipClassKey
  -- Class and its corresponding TyCon have the same Unique

isIPClass :: Class -> Bool
isIPClass cls = cls `hasKey` ipClassKey

isIPLikePred :: Type -> Bool
-- See Note [Local implicit parameters]
isIPLikePred = is_ip_like_pred initIPRecTc


is_ip_like_pred :: RecTcChecker -> Type -> Bool
is_ip_like_pred rec_clss ty
  | Just (tc, tys) <- splitTyConApp_maybe ty
  , Just rec_clss' <- if isTupleTyCon tc  -- Tuples never cause recursion
                      then Just rec_clss
                      else checkRecTc rec_clss tc
  , Just cls       <- tyConClass_maybe tc
  = isIPClass cls || has_ip_super_classes rec_clss' cls tys

  | otherwise
  = False -- Includes things like (D []) where D is
          -- a Constraint-ranged family; #7785

hasIPSuperClasses :: Class -> [Type] -> Bool
-- See Note [Local implicit parameters]
hasIPSuperClasses = has_ip_super_classes initIPRecTc

has_ip_super_classes :: RecTcChecker -> Class -> [Type] -> Bool
has_ip_super_classes rec_clss cls tys
  = any ip_ish (classSCSelIds cls)
  where
    -- Check that the type of a superclass determines its value
    -- sc_sel_id :: forall a b. C a b -> <superclass type>
    ip_ish sc_sel_id = is_ip_like_pred rec_clss $
                       classMethodInstTy sc_sel_id tys

initIPRecTc :: RecTcChecker
initIPRecTc = setRecTcMaxBound 1 initRecTc

-- --------------------- CallStack predicates ---------------------------------

isCallStackPredTy :: Type -> Bool
-- True of HasCallStack, or IP "blah" CallStack
isCallStackPredTy ty
  | Just (tc, tys) <- splitTyConApp_maybe ty
  , Just cls <- tyConClass_maybe tc
  , Just {} <- isCallStackPred cls tys
  = True
  | otherwise
  = False

-- | Is a 'PredType' a 'CallStack' implicit parameter?
--
-- If so, return the name of the parameter.
isCallStackPred :: Class -> [Type] -> Maybe FastString
isCallStackPred cls tys
  | [ty1, ty2] <- tys
  , isIPClass cls
  , isCallStackTy ty2
  = isStrLitTy ty1
  | otherwise
  = Nothing

-- | Is a type a 'CallStack'?
isCallStackTy :: Type -> Bool
isCallStackTy ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` callStackTyConKey
  | otherwise
  = False


-- | Decomposes a predicate if it is an implicit parameter. Does not look in
-- superclasses. See also [Local implicit parameters].
isIPPred_maybe :: Type -> Maybe (FastString, Type)
isIPPred_maybe ty =
  do (tc,[t1,t2]) <- splitTyConApp_maybe ty
     guard (isIPTyCon tc)
     x <- isStrLitTy t1
     return (x,t2)

{- Note [Local implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function isIPLikePred tells if this predicate, or any of its
superclasses, is an implicit parameter.

Why are implicit parameters special?  Unlike normal classes, we can
have local instances for implicit parameters, in the form of
   let ?x = True in ...
So in various places we must be careful not to assume that any value
of the right type will do; we must carefully look for the innermost binding.
So isIPLikePred checks whether this is an implicit parameter, or has
a superclass that is an implicit parameter.

Several wrinkles

* We must be careful with superclasses, as #18649 showed.  Haskell
  doesn't allow an implicit parameter as a superclass
    class (?x::a) => C a where ...
  but with a constraint tuple we might have
     (% Eq a, ?x::Int %)
  and /its/ superclasses, namely (Eq a) and (?x::Int), /do/ include an
  implicit parameter.

  With ConstraintKinds this can apply to /any/ class, e.g.
     class sc => C sc where ...
  Then (C (?x::Int)) has (?x::Int) as a superclass.  So we must
  instantiate and check each superclass, one by one, in
  hasIPSuperClasses.

* With -XUndecidableSuperClasses, the superclass hunt can go on forever,
  so we need a RecTcChecker to cut it off.

* Another apparent additional complexity involves type families. For
  example, consider
         type family D (v::*->*) :: Constraint
         type instance D [] = ()
         f :: D v => v Char -> Int
  If we see a call (f "foo"), we'll pass a "dictionary"
    () |> (g :: () ~ D [])
  and it's good to specialise f at this dictionary.

So the question is: can an implicit parameter "hide inside" a
type-family constraint like (D a).  Well, no.  We don't allow
        type instance D Maybe = ?x:Int
Hence the umbrella 'otherwise' case in is_ip_like_pred.  See #7785.

Small worries (Sept 20):
* I don't see what stops us having that 'type instance'. Indeed I
  think nothing does.
* I'm a little concerned about type variables; such a variable might
  be instantiated to an implicit parameter.  I don't think this
  matters in the cases for which isIPLikePred is used, and it's pretty
  obscure anyway.
* The superclass hunt stops when it encounters the same class again,
  but in principle we could have the same class, differently instantiated,
  and the second time it could have an implicit parameter
I'm going to treat these as problems for another day. They are all exotic.  -}

{- *********************************************************************
*                                                                      *
              Evidence variables
*                                                                      *
********************************************************************* -}

isEvVar :: Var -> Bool
isEvVar var = isEvVarType (varType var)

isDictId :: Id -> Bool
isDictId id = isDictTy (varType id)

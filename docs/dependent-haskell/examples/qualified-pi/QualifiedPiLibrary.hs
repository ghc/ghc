{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module QualifiedPiLibrary (QPi, LiftQ, exposeQ, beneathQ, withQ, ignoreQ, Demand (..), DPi, exposeDemand, ignoreProofQ, TupleQ, tupleTransport, makeTuple, keepTupleTransformer, headTuple, duplicateQ, keepDuplicate, Box (..), unwrapBox, fixedQ, DataBox (..), ignoreBoxFixed, ignoreDataFixed, keepBoxFunction, keepDataFunction, PlainPi, plainFunction, transportRefl) where

import Data.Type.Equality ((:~:) (Refl))
import PackedVecLibrary
import QualifiedPiValues (fixedQ, plainFunction)

-- | Keep an ordinary class context below the runtime binder.
type QPi = foreach (n :: Nat) -> (Eq Int) => Vec Int n

-- | Keep the outer index in the vector argument.
type LiftQ = foreach (m :: Nat) -> Vec Bool m -> QPi

-- | Expose the qualified function through an incoming nominal equality.
exposeQ :: forall value. (value ~ QPi) => value -> QPi
exposeQ f = f

-- | Use the incoming equality beneath another runtime binder.
beneathQ ::
    forall value.
    (value ~ QPi) =>
    (foreach (m :: Nat) -> Vec Bool m -> value) -> LiftQ
beneathQ f = f

-- | Obtain the function only after forcing its equality witness.
withQ :: forall value. (value :~: QPi) -> value -> QPi
withQ proof f = case proof of Refl -> f

-- | Ignore the function and its ordinary class context.
ignoreQ :: QPi -> Int
ignoreQ _ = 42

-- | Supply one method without a superclass or a global instance.
class Demand a where
    -- | Return the supplied method value.
    demand :: a

-- | Keep the injected dictionary below the runtime index argument.
type DPi = foreach (n :: Nat) -> (Demand Int) => Vec Int n

-- | Expose a function with an injected ordinary dictionary context.
exposeDemand :: forall value. (value ~ DPi) => value -> DPi
exposeDemand f = f

-- | Ignore an equality witness without obtaining its evidence.
ignoreProofQ :: forall value. (value :~: QPi) -> value -> value
ignoreProofQ _ f = f

-- | Store a qualified function beside the outer index-dependent vector.
type TupleQ = foreach (m :: Nat) -> Vec Bool m -> (QPi, Vec Bool m)

-- | Transport a tuple field beneath a retained binder without eta expansion.
tupleTransport ::
    forall value.
    (value ~ QPi) =>
    (foreach (m :: Nat) -> Vec Bool m -> (value, Vec Bool m)) -> TupleQ
tupleTransport f = f

-- | Construct a tuple without applying its qualified function field.
makeTuple :: QPi -> TupleQ
makeTuple f m xs = (f, xs)

-- | Supply a complete expected arrow for a tuple-valued retained worker.
keepTupleTransformer :: (TupleQ -> TupleQ) -> TupleQ -> TupleQ
keepTupleTransformer f = f

-- | Supply the internal expected tuple type without a client type operand.
headTuple :: (QPi, QPi) -> QPi
headTuple (f, _) = f

-- | Return two copies after checking the incoming equality.
duplicateQ :: forall value. (value ~ QPi) => value -> (QPi, QPi)
duplicateQ f = (f, f)

-- | Supply the complete expected nested-container arrow for duplicateQ.
keepDuplicate :: (QPi -> (QPi, QPi)) -> QPi -> (QPi, QPi)
keepDuplicate f = f

-- | Store an ordinary lifted value in a representational newtype parameter.
newtype Box a = Box a

-- | Supply the internal expected newtype application without a client operand.
unwrapBox :: Box QPi -> QPi
unwrapBox (Box f) = f

-- | Store an ordinary lazy field behind a real data constructor.
data DataBox a = DataBox a

-- | Ignore a newtype field without forcing the scrutinee.
ignoreBoxFixed :: Box QPi -> QPi
ignoreBoxFixed (Box _) = fixedQ

-- | Inspect the data constructor before returning the fixed function.
ignoreDataFixed :: DataBox QPi -> QPi
ignoreDataFixed (DataBox _) = fixedQ

-- | Give a checked lambda the complete newtype-pattern function type.
keepBoxFunction :: (Box QPi -> QPi) -> Box QPi -> QPi
keepBoxFunction f = f

-- | Give a checked lambda the complete data-pattern function type.
keepDataFunction :: (DataBox QPi -> QPi) -> DataBox QPi -> QPi
keepDataFunction f = f

-- | Keep the runtime index without a class context.
type PlainPi = foreach (n :: Nat) -> Vec Int n

-- | Use the GADT pattern evidence before returning the original function.
transportRefl ::
    forall a b.
    (a :~: b) ->
    (foreach (n :: Nat) -> Vec a n) ->
    (foreach (n :: Nat) -> Vec b n)
transportRefl Refl f = f

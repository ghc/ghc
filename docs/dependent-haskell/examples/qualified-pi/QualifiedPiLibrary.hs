{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module QualifiedPiLibrary (QPi, LiftQ, exposeQ, beneathQ, withQ, ignoreQ, Demand (..), DPi, exposeDemand) where

import Data.Type.Equality ((:~:) (Refl))
import PackedVecLibrary

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

{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, ScopedTypeVariables, TypeAbstractions #-}
module VisibleRuleArguments (Reading(..), keepReading, keepAfter) where

import Data.Kind (Type)

-- | A reading indexed by a runtime optional value.
data Reading (m :: Maybe a) = Reading Int

-- | Return the reading without evaluating its index.
{-# NOINLINE keepReading #-}
keepReading :: forall (a :: Type). foreach (m :: Maybe a) -> Reading m -> Reading m
keepReading _ x = x

{-# RULES "keepReading/Bool" forall m x. keepReading @Bool m x = x #-}

-- | Ignore the Boolean before selecting the reading's element type.
{-# NOINLINE keepAfter #-}
keepAfter :: Bool -> forall (a :: Type). foreach (m :: Maybe a) -> Reading m -> Reading m
keepAfter _ @a = keepReading @a

{-# RULES "keepAfter/Bool" forall ignored m x. keepAfter ignored @Bool m x = x #-}

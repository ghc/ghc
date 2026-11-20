{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, ScopedTypeVariables #-}
module VisibleRuleArguments (Reading(..), keepReading) where

import Data.Kind (Type)

-- | A reading indexed by a runtime optional value.
data Reading (m :: Maybe a) = Reading Int

-- | Return the reading without evaluating its index.
{-# NOINLINE keepReading #-}
keepReading :: forall (a :: Type). foreach (m :: Maybe a) -> Reading m -> Reading m
keepReading _ x = x

{-# RULES "keepReading/Bool" forall m x. keepReading @Bool m x = x #-}

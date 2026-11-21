{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, ScopedTypeVariables, TypeAbstractions #-}
module VisibleRuleArguments (Reading(..), keepReading, keepAfter, TaggedReading(..), keepTagged) where

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

-- | A payload indexed by values on both sides of an erased type binder.
data TaggedReading (tag :: Bool) (m :: Maybe a) = TaggedReading Int

-- | Keep both runtime indices while selecting the element type between them.
{-# NOINLINE keepTagged #-}
keepTagged :: foreach (tag :: Bool) -> forall (a :: Type).
              foreach (m :: Maybe a) -> TaggedReading tag m -> TaggedReading tag m
keepTagged _ _ x = x

{-# RULES "keepTagged/Bool" forall tag m x. keepTagged tag @Bool m x = x #-}

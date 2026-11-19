{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}

module OpenConstructorSource (left, right, nested) where

import qualified OpenConstructorWorker as W

-- | The second field gives the result type.
left :: forall a b. a -> b -> W.pick ('W.LeftFields a b)
left _ y = y

-- | The first field of this constructor gives the result type.
right :: forall a b. a -> b -> W.pick ('W.RightFields b a)
right _ y = y

-- | The inner Just exposes the first argument.
nested :: forall a b. a -> b -> W.pickNested ('W.Nested ('Just a)) b
nested x _ = x

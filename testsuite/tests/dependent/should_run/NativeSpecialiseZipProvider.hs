{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeSpecialiseZipProvider (keepIndex, emptySpine) where

import Data.Proxy (Proxy(..))

-- | Keep the list as the index of the result.
{-# NOINLINE keepIndex #-}
keepIndex :: foreach (xs :: [(Bool, ())]) -> Proxy xs
keepIndex _ = Proxy

-- | Inspect the list under a vacuous retained binder.
{-# NOINLINE emptySpine #-}
emptySpine :: foreach (xs :: [(Bool, ())]) -> Bool
emptySpine [] = True
emptySpine (_:_) = False

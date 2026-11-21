{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -fplugin=DeferredHeterogeneousPlugin #-}

module DeferredHeterogeneous where

import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality (type (~~))

ignore :: forall k (a :: k). (a ~~ Int) => Proxy a -> Int
ignore _ = 47
{-# NOINLINE ignore #-}

-- This call generates a deferred primitive equality between different kinds.
bad :: Int
bad = ignore @Bool @'True Proxy

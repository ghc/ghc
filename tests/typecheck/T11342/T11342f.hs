{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module T11342f where

import Data.Proxy
import GHC.TypeLits

f :: forall str a b. (KnownChar a, KnownSymbol b, ConsSymbol a b ~ str) => (Char, String)
f = (charVal @a Proxy, symbolVal @b Proxy)

f' :: (Char, String)
f' = f @"hello"

g :: forall str. (KnownSymbol str, UnconsSymbol str ~ 'Nothing) => String
g = symbolVal @str Proxy

h :: forall a tail str. (KnownSymbol tail, KnownChar a, UnconsSymbol str  ~ 'Just '(a, tail) ) => (Char, String)
h = (charVal @a Proxy, symbolVal @tail Proxy)

h' :: (Char, String)
h' = h @'h' @"ello"

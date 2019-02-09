{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
module T16293a where

import Data.Coerce
import Data.Proxy
import GHC.Exts

test1a :: () -> Proxy Int
test1a _ = Proxy @Int

test1b :: () -> Proxy# Int
test1b _ = proxy# @Int

test2a :: (() -> Proxy a) -> (() -> Proxy b)
test2a = coerce

test2b :: (() -> Proxy# a) -> (() -> Proxy# b)
test2b = coerce

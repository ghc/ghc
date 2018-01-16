module Foo where

import Data.Map

f :: (a -> b) -> Map k a -> Map k b
f = fmap

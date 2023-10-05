{-# LANGUAGE NoEmptyCase #-}
module Foo where

import Data.List

f1 :: a -> a
f1 x = 5

f2 x = do case () of

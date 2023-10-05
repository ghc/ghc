module Annfail04 where

import Annfail04_Help
-- Testing that we detect the use of instances defined in the same module

instance Thing Int where
  thing = 1

{-# ANN module (thing :: Int) #-}

{-# ANN type Foo (thing :: Int) #-}
data Foo = Bar

{-# ANN f (thing :: Int) #-}
f x = x
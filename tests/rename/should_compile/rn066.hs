{-# OPTIONS_GHC -fwarn-warnings-deprecations #-}

-- Test warnings on constructors and class ops

module ShouldCompile where

import Rn066_A

instance Foo T where
  op x  = x
  bop y = y

foo = op C

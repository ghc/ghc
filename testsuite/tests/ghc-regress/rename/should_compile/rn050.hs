{-# OPTIONS_GHC -fwarn-deprecations #-}

-- Test deprecation of constructors and class ops

module ShouldCompile where

import Rn050_A

instance Foo T where
  op x  = x
  bop y = y

foo = op C

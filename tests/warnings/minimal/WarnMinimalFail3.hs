{-# LANGUAGE DefaultSignatures #-}
module WarnMinimalFail3 where

class Parent a where
  parent :: a
  default parent :: Child a => a
  parent = child

class Parent a => Child a where
  child :: a
  child = parent
  {-# MINIMAL parent | child #-}
  -- we would like this to work, but it doesn't yet.

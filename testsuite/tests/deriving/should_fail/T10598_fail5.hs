{-# LANGUAGE GHC2021 #-}
module T10598_fail5 where

data Foo = Foo
  deriving Eq
  deriving Ord

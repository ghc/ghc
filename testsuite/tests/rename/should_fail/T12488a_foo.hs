{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies #-}
module T12488a_foo ( T (type A) ) where

data T = A

class Foo a where
  type A a
  foo :: a -> Int

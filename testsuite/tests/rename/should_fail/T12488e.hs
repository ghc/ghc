{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies #-}
module T12488e
  ( C (data A)
  , D (data (+++))
  ) where

class C a where
  type A a

class D a where
  type (+++) a
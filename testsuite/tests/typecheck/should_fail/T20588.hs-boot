
{-# LANGUAGE TypeFamilies #-}

module T20588 where

import Data.Kind

class C (a :: Type) where
  meth :: a -> a

class D (a :: Type) where
  type family T a :: Type

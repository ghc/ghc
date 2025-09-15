
{-# LANGUAGE TypeFamilies #-}

module T20588 where

import Data.Kind

class C (a :: Type) where
  {-# MINIMAL meth #-}
  meth :: a -> a
  meth = id

class D (a :: Type) where
  type family T a :: Type
  type instance T a = Int

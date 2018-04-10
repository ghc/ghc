{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T14728a where

import Data.Functor.Identity
import Data.Kind

class C (a :: Type) where
  type T a (x :: a) :: Type
  type U z :: a

instance C () where
  type T () '() = Bool

deriving instance C (Identity a)

f :: T (Identity ()) ('Identity '())
f = True

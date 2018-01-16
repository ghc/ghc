{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module T8165_fail where

import Data.Kind

class C (a :: k) where
  type T k :: Type

instance C Int where
  type T Type = Int

newtype MyInt = MyInt Int
  deriving C

-----------------------------------------------------------

class D a where
  type S a = r | r -> a

instance D Int where
  type S Int = Char

newtype WrappedInt = WrapInt Int
  deriving D

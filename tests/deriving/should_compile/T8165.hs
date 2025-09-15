{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module T8165 where

import Data.Kind (Type)

-----------------------------------------------------------

class C a where
    type T a

instance C Int where
    type T Int = Bool

newtype NT = NT Int
    deriving C

-----------------------------------------------------------

class D a where
  type U a

instance D Int where
  type U Int = Int

newtype E = MkE Int
  deriving D

-----------------------------------------------------------

class C2 a b where
  type F b c a :: Type
  type G b (d :: Type -> Type) :: Type -> Type

instance C2 a y => C2 a (Either x y) where
  type F (Either x y) c a = F y c a
  type G (Either x y) d   = G y d

newtype N a = MkN (Either Int a)
  deriving (C2 x)

-----------------------------------------------------------

class HasRing a where
    type Ring a

newtype L2Norm a = L2Norm a
    deriving HasRing

newtype L1Norm a = L1Norm a
    deriving HasRing

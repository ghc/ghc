{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module T13747 where

import Data.Kind (Type)

class C a where
  type family TC a :: Type

class D a where
  data family TD a :: Type

instance C Int where
  type instance TC Int = Int

instance D Double where
  data instance TD Double = TDDouble

instance D Int where
  newtype instance TD Int = TDInt Int

instance D Char where
    data instance TD Char where
        C1 :: TD Char
        C2 :: TD Char

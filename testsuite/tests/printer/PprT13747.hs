{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module T13747 where

class C a where
  type family TC a :: *

class D a where
  data family TD a :: *

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

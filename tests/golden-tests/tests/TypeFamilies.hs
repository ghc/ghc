{-# LANGUAGE TypeFamilies #-}

module TypeFamilies where

-- | Type family G
type family G a :: *

-- | A class with an associated type
class A a where
  -- | An associated type
  data B a :: * -> *
  -- | A method
  f :: B a Int

-- | Doc for family
type family F a


-- | Doc for G Int
type instance G Int = Bool
type instance G Float = Int


instance A Int where
  data B Int x = Con x
  f = Con 3

g = Con 5

{-# LANGUAGE TypeFamilies, FlexibleInstances, OverlappingInstances #-}
module T4246 where

class Stupid a where
   type F a

instance Stupid a where
   type F a = a

instance Stupid Int where
   type F Int = Bool

type family G a :: *
type instance G Int = Int
type instance G Int = Bool

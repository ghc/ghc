{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module T4246 where

class Stupid a where
   type F a

instance {-# OVERLAPPABLE #-} Stupid a where
   type F a = a

instance {-# OVERLAPPING #-} Stupid Int where
   type F Int = Bool

type family G a :: *
type instance G Int = Int
type instance G Int = Bool

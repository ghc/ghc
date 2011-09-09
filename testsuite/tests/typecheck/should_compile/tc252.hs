{-# LANGUAGE TypeFamilies #-}
module ShouldCompile where

class Cls a where
    type Fam a :: *
    type Fam a = Maybe a

instance Cls Int where
    -- Overriding default
    type Fam Int = Bool

nott :: (Fam a ~ Bool) => a -> Fam a -> Fam a
nott _proxy False = True
nott _proxy True  = False

foo :: Bool -> Bool
foo = nott (undefined :: Int)

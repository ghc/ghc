{-# LANGUAGE TypeFamilies, GADTs #-}

module T3440 where

type family Fam a :: *

data GADT :: * -> * where
   GADT :: a -> Fam a -> GADT (Fam a)

unwrap :: GADT (Fam a) -> (a, Fam a)
unwrap (GADT x y) = (x, y)

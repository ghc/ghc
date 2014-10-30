{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies #-}

module T5837 where

type family TF a :: *
type instance TF (a,b) = (TF a, TF b)

t :: (a ~ TF (a,Int)) => Int
t = undefined

{-

    [G] a ~ TF (a,Int)
-->
    TF (a,Int) ~ fsk
    fsk ~ a
--->
    fsk ~ (TF a, TF Int)
    fsk ~ a
--->
    a ~ (TF a, TF Int)

-}
{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}
module T8557 where

data family Sing (a :: k)
data instance Sing (a :: [k]) = SNil

x :: Sing '[]
x = SNil

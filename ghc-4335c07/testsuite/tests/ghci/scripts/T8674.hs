{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}
module T8557 where

data family Sing (a :: k)
data instance Sing (a :: [k]) = SNil
data instance Sing Bool = SBool


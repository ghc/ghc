{-# OPTIONS -findexed-types #-}

module ShouldCompile where

data family T a
data instance T a = T

foo :: T Int -> T Char
foo T = T

type family S a
type instance S a = a

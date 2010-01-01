{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

data family C9 a b :: *
data instance C9 Int Int = C9IntInt
-- must fail: conflicting
data instance C9 Int Int = C9IntInt2

type family D9 a b :: *
type instance D9 Int Int = Char
-- must fail: conflicting
type instance D9 Int Int = Int

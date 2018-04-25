{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

data family C9 a b :: *
data instance C9 Int Int = C9IntInt
data instance C9 [a] Int = C9ListInt
-- must fail: conflicting
data instance C9 [a] Int = C9ListInt2

type family D9 a b :: *
type instance D9 Int Int = Int
type instance D9 [a] Int = [a]
-- must fail: conflicting
type instance D9 [a] Int = Maybe a

type instance D9 Int [a] = [a]
type instance D9 Int [b] = [b]  -- must not conflict!

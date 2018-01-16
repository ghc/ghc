{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

data family C9 a b :: *
data instance C9 Int Int = C9IntInt
data instance C9 [a] Int = C9ListInt
-- must fail: conflicting
data instance C9 [Int] Int = C9ListInt2

type family D9 a b :: *
type instance D9 Int Int = Int
type instance D9 [a] Int = [a]
-- must fail: conflicting
type instance D9 [Int] Int = [Bool]

type family E9 a b :: *
type instance E9 Int   Int = Int
type instance E9 [a]   Int = [a]
type instance E9 [Int] Int = [Int]   -- does *not* conflict!
type instance E9 b     Int = b

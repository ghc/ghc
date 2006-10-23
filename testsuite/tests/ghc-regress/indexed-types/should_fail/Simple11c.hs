{-# OPTIONS -findexed-types #-}

module ShouldFail where

data family C9 a b :: *
data instance C9 Int Int = C9IntInt
data instance C9 [a] Int = C9ListInt
-- must fail: conflicting
data instance C9 [Int] Int = C9ListInt2

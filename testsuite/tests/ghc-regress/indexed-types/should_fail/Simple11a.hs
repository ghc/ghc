{-# OPTIONS -findexed-types #-}

module ShouldFail where

data family C9 a b :: *
data instance C9 Int Int = C9IntInt
-- must fail: conflicting
data instance C9 Int Int = C9IntInt2

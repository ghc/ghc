{-# OPTIONS -findexed-types #-}

module ShouldFail where

-- must fail: Repeated type variable
class C4 a where
  data S4 a a :: *

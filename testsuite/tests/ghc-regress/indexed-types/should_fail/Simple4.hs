{-# OPTIONS -findexed-types #-}

module ShouldFail where

-- must fail: defaults have no patterns
class C2 a b where
  type S2 a :: *
  type S2 Int = Char

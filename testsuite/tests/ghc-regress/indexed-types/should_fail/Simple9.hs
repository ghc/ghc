{-# OPTIONS -findexed-types -fglasgow-exts #-}

module ShouldFail where

class C7 a b where
  data S7 b :: *

instance C7 Char (a, Bool) where
  data S7 (a, Bool) = S7_1

-- must fail: type indexes don't match the instance types
instance C7 Char (a, Int) where
  data S7 (b, Int) = S7_2

{-# OPTIONS -findexed-types #-}

module ShouldFail where

class C1 a where
  newtype S1 a :: *

-- must fail: wrong category of type instance
instance C1 Char where
  data S1 Char = S1Char ()

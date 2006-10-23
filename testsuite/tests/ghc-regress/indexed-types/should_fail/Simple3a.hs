{-# OPTIONS -findexed-types #-}

module ShouldFail where

class C1 a where
  newtype S1 a :: *

-- must fail: wrong category of type instance
instance C1 Int where
  type S1 Int = Bool

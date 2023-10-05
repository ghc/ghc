{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

-- must fail: AT must be in class instance
class C5 a where
  data S5 a :: *
data instance S5 Int = S5

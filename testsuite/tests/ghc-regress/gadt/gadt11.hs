{-# OPTIONS -fglasgow-exts #-}

module ShouldFail where

-- Wrong return type
data X f = X (f ())

data B a where
  B1 :: X []
  B2 :: B [Int]



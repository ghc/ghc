{-# OPTIONS -fglasgow-exts #-}

-- Tests record syntax for GADTs

module ShouldFail where

data T a where
  T1 { x :: a, y :: b } :: T (a,b)
  T4 { x :: Int } :: T [a]

	
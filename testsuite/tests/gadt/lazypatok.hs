{-# LANGUAGE GADTs #-}

-- It's not clear whether this one should succeed or fail,
-- Arguably it should succeed because the type refinement on
-- T1 should make (y::Int).  Currently, though, it fails.

module ShouldFail where

data T a where
  T1 :: Int -> T Int

f :: (T a, a) -> Int
f ~(T1 x, y) = x+y


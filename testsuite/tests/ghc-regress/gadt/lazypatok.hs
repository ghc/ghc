{-# OPTIONS -fglasgow-exts #-}

-- It's not clear whether this one should succed or fail,
-- Arguably it should succeed because the type refinement on
-- T1 should make (y::Int).  Currently, though, it succeeds

module ShouldFail where

data T a where
  T1 :: Int -> T Int

f :: (T a, a) -> Int
f ~(T1 x, y) = x+y


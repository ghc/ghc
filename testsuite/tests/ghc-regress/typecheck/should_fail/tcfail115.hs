{-# OPTIONS -fglasgow-exts #-}

-- Unboxed tuples; c.f. tcfail120, tc209

module ShouldFail where

type T a = Int -> (# Int, Int #)

g t =  case t of r -> (r :: (# Int, Int #)) 

f :: T a -> T a
f t = \x -> case t x of r -> r


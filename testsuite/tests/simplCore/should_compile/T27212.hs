module T27212 where

import Unsafe.Coerce

class Given a where given :: a
newtype Gift a r = Gift (Given a => r)

useIt :: Given Int => Int
useIt = given + 1
{-# NOINLINE useIt #-}

foo :: [Int] -> [Int]
foo xs = case unsafeEqualityProof @(Gift Int Int) @(Int -> Int) of
  UnsafeRefl -> map (\y -> (Gift useIt :: Gift Int Int) y) xs
    -- That (\y. glah y) can eta-reduce to `glah`
    -- but doing doing so makes a FunCo coercion that crosses from
    -- Type to constraint. Tricky.

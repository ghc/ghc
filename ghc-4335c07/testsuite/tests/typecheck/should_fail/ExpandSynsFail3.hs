-- We test two things here:
--
-- 1. We expand only as much as necessary. In this case, we shouldn't expand T.
-- 2. When we find a difference(T3 and T5 in this case), we do minimal expansion
--    e.g. we don't expand both of them to T1, instead we expand T5 to T3.

module Main where

type T5 = T4
type T4 = T3
type T3 = T2
type T2 = T1
type T1 = Int

type T a = Int -> Bool -> a -> String

f :: T (T3, T5, Int) -> Int
f = undefined

a :: Int
a = f (undefined :: T (T5, T3, Bool))

main = print a

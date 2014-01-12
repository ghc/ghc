module TestImport where

import TestList

{-# CONTRACT t2 :: _ #-}
t2 = head1 [True]  -- same as TestList res2

{-# CONTRACT t3 :: {x | True} #-}
t3 = head1 [True]  -- same as TestList.res3

t4 = head1 []      -- same as TestList.res4

t5 = head1 [True]  -- same as TestList.res5

t2a = res2
t3a = res3
t4a = res4
t5a = res5

{-# CONTRACT tail1 :: {xs | not1 (null1 xs)} ->  {r | True} #-}
tail1 :: [Int] -> [Int]
tail1 (x:xs) = xs

{-# CONTRACT tail2 :: {xs | not (null xs)} ->  {r | True} #-}
tail2 :: [Int] -> [Int]
tail2 (x:xs) = xs


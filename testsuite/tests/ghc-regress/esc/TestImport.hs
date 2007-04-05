module TestImport where

import TestList

t1 = head1 [True]

t2 = head1 [] 

{-# CONTRACT t3 :: {x | True} #-}
t3 = head [True]

{-# CONTRACT t4 :: _ #-}
t4 = head [True]

{-# CONTRACT tail1 :: {xs | not1 (null1 xs)} ->  {x | True} #-}
tail1 :: [Int] -> [Int]
tail1 (x:xs) = xs

{-# CONTRACT tail2 :: {xs | not (null xs)} ->  {x | True} #-}
tail2 :: [Int] -> [Int]
tail2 (x:xs) = xs

t5 = res2
t6 = res3
t7 = res5
t8 = res4


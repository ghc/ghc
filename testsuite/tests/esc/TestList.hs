module TestList where

{-# CONTRACT head1 :: {ys | not1 (null1 ys)} -> {r | True} #-}
-- {-# CONTRACT head1 :: {xs | not (null xs)} -> {r | True} #-}
head1 :: [Bool] -> Bool
head1 (x:xs) = x

not1 True = False
not1 False = True

null1 [] = True
null1 xs = False

{-# CONTRACT res2 :: _ #-}
res2 = head1 [True]

{-# CONTRACT res3 :: {x | True} #-}
res3 = head1 [True]

res4 = head1 []

res5 = head1 [True]



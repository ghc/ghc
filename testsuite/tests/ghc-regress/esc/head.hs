module Head where

(==>) True x = True
(==>) False x = True

and1 True x = x
and1 False x = False

{-# CONTRACT head1 :: {xs | not1 (null1 xs)} -> {x | True} #-}
-- {-# CONTRACT head1 :: {xs | not1 (null1 xs)} -> {r | r > 0} #-}
head1 :: [Int] -> Int
head1 (x:xs) = x

not1 True = False
not1 False = True

null1 [] = True
null1 xs = False

{-# CONTRACT f :: {x | x > 0} -> any #-}
f :: Int -> Int
f x = x + 1


{-# CONTRACT res2 :: any #-}
res2 = head1 [5]

{-# CONTRACT res3 :: {x | True} #-}
res3 = head1 [6]

res5 = head1 [2]

res4 = head1 []


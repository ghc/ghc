module Head where

(==>) True x = True
(==>) False x = True

and1 True x = x
and1 False x = False

{-# CONTRACT head1 :: {xs | not (null xs)} -> {r | not (null r)} #-}
-- {-# CONTRACT head1 :: {xs | not1 (null1 xs)} -> {r | r > 0} #-}
head1 :: [Int] -> [Int]
head1 (x:xs) = [x]

not1 True = False
not1 False = True

null1 [] = True
null1 xs = False

{-# CONTRACT f :: {x | x > 0 && x < 5} -> any #-}
f :: Int -> Int
f x = x + 1


{-# CONTRACT res2 :: any #-}
res2 = head1 [5]

res3 = head1 [6]

res4 = head1 []


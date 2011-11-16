{-# OPTIONS_GHC -fpedantic-bottoms #-}
-- The pedantic-bottom flag makes GHC pickier about bottoms
-- See the ticket #5587

module Main(main) where

hiddenError = error "hidden error"

main = print $ seq (head (map (\a -> \b -> hiddenError) (hiddenError::[] Bool))) id [1]

{-  See notes in Trac #5587
f a b =  a
he = hiddenError::[Bool]
main = print $ seq (head (map f he)) id [1]


head (map f he)
= head (build (\cn. foldr (mapFB c f) n he))
= (\cn. foldr (mapFB c f) n he) (\x _ -> x) badHead
= foldr (mapFB (\x _ -> x) f) badHead he
= let go [] = badHead
       go (y:ys) = mapFB (\x _ -> x) f y (go ys)
   in go he
= let go []     = badHead
       go (y:ys) = mapFB (\x _ -> x) f y (go ys)
                 = (\x _. x) (f y) (go ys)
                 = f y
                 = \b. y
   in go he
-}

{-# LANGUAGE DataKinds, GADTs #-}
module Main where

import Filter (filterVec, partitionVec, toList)
import Vector (Nat(..), Vec(..))

main :: IO ()
main = do
    let one = Cons (7 :: Int) (error "filter forced the unseen tail")
        three = Cons 2 (Cons (7 :: Int) (Cons 9 Nil))
    print (take 1 (toList (filterVec odd one)), toList (filterVec odd three),
           toList (filterVec (const False) three), toList (filterVec odd (Nil :: Vec 'Z Int)))
    let first x _ = Just x
    print (foldr first Nothing (fst (partitionVec odd one)),
           foldr first Nothing (snd (partitionVec odd three)))

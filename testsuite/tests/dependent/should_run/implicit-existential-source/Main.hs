{-# LANGUAGE DataKinds, GADTs #-}
module Main where

import Filter (filterVec, toList)
import Vector (Nat(..), Vec(..))

main :: IO ()
main = do
    let one = Cons (7 :: Int) (error "filter forced the unseen tail")
        three = Cons 2 (Cons (7 :: Int) (Cons 9 Nil))
    print (take 1 (toList (filterVec odd one)), toList (filterVec odd three),
           toList (filterVec (const False) three), toList (filterVec odd (Nil :: Vec 'Z Int)))

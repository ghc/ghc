
module Main where
import Data.Char

{-# NOINLINE f #-}
f :: Int -> String
f x = "NOT FIRED"

{-# NOINLINE neg #-}
neg :: Int -> Int
neg = negate

{-# NOINLINE myord #-}
myord :: Char -> Int
myord = ord

{-# RULES
     "f" forall (c::Char->Int) (x::Char). f (c x) = "RULE FIRED"
 #-}

main = do { print (f (myord 'a'))       -- Rule should fire
          ; print (f (neg 1)) }         -- Rule should not fire

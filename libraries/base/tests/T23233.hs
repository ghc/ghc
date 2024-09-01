{-# LANGUAGE BangPatterns #-}
module Main (main) where

import GHC.Conc
import GHC.Exts
import Debug.Trace

data StrictPair a b = SP !a !b
  deriving Show

f :: a -> Int -> StrictPair a Int
{-# NOINLINE f #-}
f x y = SP x (y * y)

fun :: a -> b -> (b -> Bool) -> StrictPair a Int
fun x y g = case pseq x y of
  !u -> case g u of
    True -> f x 12
    False -> f x 14

p :: Int
{-# NOINLINE p #-}
p = trace "eval p" 3

q :: Int
{-# NOINLINE q #-}
q = trace "eval q" 4

main :: IO ()
main = print (fun p q even)

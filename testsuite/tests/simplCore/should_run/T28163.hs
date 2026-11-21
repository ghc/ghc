{-# LANGUAGE MagicHash #-}
module Main (main) where

import GHC.Exts
import System.Environment (getArgs)

{-# NOINLINE k #-}
k :: Double -> Double -> Int
k _ _ = 3

unI :: Int -> Int#
unI (I# x) = x

{-# INLINE [0] mk #-}
mk :: Int -> Int -> Maybe Double
mk a b = Just (D# (int2Double# (unI (a `div` b))))

{-# NOINLINE f #-}
f :: Int -> Int -> Int
f a b = case mk a b of
  Just x -> k x x
  Nothing -> 0

main :: IO ()
main = print (f 1 0)

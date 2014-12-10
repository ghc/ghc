-- A test to show that Static Pointers can be listed.
{-# LANGUAGE StaticPointers #-}
module Main where

import Control.Monad (when)
import Data.List ((\\))
import GHC.StaticPtr
import System.Exit

main = when (not $ eqBags staticPtrKeys expected) $ do
    print ("expected", expected)
    print ("found", staticPtrKeys)
    exitFailure
  where

    expected =
      [ staticKey $  static (\x -> x :: Int)
      , staticKey   (static return :: StaticPtr (Int -> IO Int))
      , staticKey $  static g
      ]

    eqBags :: Eq a => [a] -> [a] -> Bool
    eqBags xs ys = null (xs \\ ys) && null (ys \\ xs)

g :: Int -> Int
g = (+1)

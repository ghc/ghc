{- test GHCi support for unlifted types -}

{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -fbyte-code #-}

module Main (main) where

import GHC.Exts
import GHC.Arr

import Data.Kind
import Control.Exception

import T19628a

x1 :: Int
x1 = case test of Force _ -> 10
  where
    test :: Strict Int
    test = Force undefined

x2 :: Int
x2 = case arr of _ -> 15
    where
        Array _ _ _ arr = listArray (1::Int, 10) [1..]

x3 :: Int
x3 = case test of Force2 y z -> y + z
  where
    test :: Strict2 Int
    test = Force2 15 20

x4 :: Int
x4 = 40
  where
    test :: Maybe Int
    test = undefined

x5 :: Int
x5 = 45
  where
    test :: Strict Int
    test = undefined

x6 :: Int
x6 = case test of Force y -> y
  where
    test :: Strict Int
    test = Force undefined

x7 :: Int
x7 = case addStrict (Force 4) (Force 5) of (Force y) -> y

x8 :: (Int, Int)
x8 = (y1, y2)
  where
    y1 = unStrict (Force 8)
    y2 = case toStrict 7 of Force z -> z

main :: IO ()
main = do
  print x1
  print x2
  print x3
  print x4
  print x5 `catch` \(e::SomeExceptionWithLocation) -> putStrLn "x5: exception"
  print x6 `catch` \(e::SomeExceptionWithLocation) -> putStrLn "x6: exception"
  print x7
  print x8

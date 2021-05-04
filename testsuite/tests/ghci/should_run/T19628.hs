{- test GHCi support for unlifted types -}

{-# LANGUAGE UnliftedDatatypes #-}

module Main (main) where

import GHC.Exts
import GHC.Arr

import Data.Kind
import Control.Exception

-- unlifted but boxed datatypes
type Strict :: Type -> TYPE ('BoxedRep 'Unlifted)
data Strict a = Force a

type Strict2 :: Type -> TYPE ('BoxedRep 'Unlifted)
data Strict2 a = Force2 a a

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

main :: IO ()
main = do
  print x1
  print x2
  print x3
  print x4
  print x5 `catch` \(e::SomeException) -> putStrLn "x5: exception"
  print x6 `catch` \(e::SomeException) -> putStrLn "x6: exception"

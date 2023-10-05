{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StaticPointers       #-}

module Ppr054 where

import Data.Typeable
import GHC.StaticPtr

main = putStr $ unlines $ map show names
  where
    names =
      [ staticPtrInfo $ static g
      , staticPtrInfo $ (static id :: StaticPtr (Int -> Int))
      , staticPtrInfo $ (p0 :: StaticPtr (Int -> Int))
      , staticPtrInfo $ (static method :: StaticPtr (Char -> Int))
      , staticPtrInfo $ (static t_field :: StaticPtr (T Int -> Int))
      ]

g :: Int -> Int
g = id

p0 :: Typeable a => StaticPtr (a -> a)
p0 = static (\x -> x)

data T a = T { t_field :: a }
  deriving Typeable

class C1 a where
  method :: a -> Int

instance C1 Char where
  method = const 0

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StaticPointers     #-}

module StaticPointers02 where

import GHC.StaticPtr
import Data.Typeable

f2 :: Typeable a => StaticPtr (a -> a)
f2 = static id

f3 :: StaticPtr (Char -> Int)
f3 = static method

f4 :: Typeable a => StaticPtr (T a -> a)
f4 = static t_field

g :: Int -> Int
g = id

f5 :: Typeable a => StaticPtr (a -> a)
f5 = static (id . id)

f6 :: Typeable a => StaticPtr (a -> IO a)
f6 = static return

f7 :: Typeable a => StaticPtr (a -> IO a)
f7 = static (\x -> getLine >> return x)

data T a = T { t_field :: a }
  deriving Typeable

class C a where
  method :: a -> Int

instance C Char where
  method = const 0

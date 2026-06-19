{-# LANGUAGE GADTs, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
module Main where

class D a where
  m :: a -> Int
  m _ = 0
  n :: a -> Int
  n _ = 0

class (D a, D a) => C a

data T a

instance D a => D (T a)
instance C a => C (T a)

instance D ()
instance C ()

data G where
  MkG :: forall a. C (T a) => T a -> G

sh :: G -> Int
sh (MkG x) = m x

f :: forall b. C b => G
f = MkG (undefined :: T b)
{-# NOINLINE f #-}

main :: IO ()
main = print (sh (f @()))

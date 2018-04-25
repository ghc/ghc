{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Data.Proxy

class C a where
  c :: proxy a -> Int
  c _ = 42

instance C Int where
  c _ = 27

newtype Foo = MkFoo Int
  deriving          Eq
  deriving anyclass C
deriving newtype instance Show Foo

main :: IO ()
main = do
  print $ MkFoo 100
  print $ c (Proxy :: Proxy Foo)

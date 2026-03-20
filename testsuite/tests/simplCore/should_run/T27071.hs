{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O -fworker-wrapper-cbv #-}
module Main where

class MyClass a where
  myVal :: Int

instance MyClass Bool where
  myVal = 0

showMyVal :: forall a. MyClass a => String
showMyVal = show (myVal @a)
{-# NOINLINE showMyVal #-}

main :: IO ()
main = putStrLn (showMyVal @Bool)

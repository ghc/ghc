{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}

module Main (main) where

data Ur a where
  Ur :: a -> Ur a

unur :: Ur a -> a
unur (Ur a) = a

segvGHCi :: Ur ()
segvGHCi = Ur $ ()

main :: IO ()
main = print (unur segvGHCi)


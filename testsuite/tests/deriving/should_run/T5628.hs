{-# LANGUAGE EmptyDataDecls, StandaloneDeriving #-}
module Main where

data Z
deriving instance Eq Z

g :: Z
g = g

main :: IO ()
main = print (g == g)

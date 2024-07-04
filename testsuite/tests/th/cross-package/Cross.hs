{-# language TemplateHaskell #-}

module Main where

import GHC.Prim
import CrossLocal (splc)

a :: Int
a = $(splc)

main :: IO ()
main = putStrLn (show a)

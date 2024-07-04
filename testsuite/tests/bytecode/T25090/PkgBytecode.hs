{-# language TemplateHaskell #-}

module Main where

import GHC.Prim
import Local (splc)

a :: Int
a = $(splc)

main :: IO ()
main = putStrLn (show a)

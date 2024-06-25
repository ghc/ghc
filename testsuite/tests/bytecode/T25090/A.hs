{-# language TemplateHaskell #-}
module Main where

import D

main :: IO ()
main = putStrLn (show ($splc :: Int))

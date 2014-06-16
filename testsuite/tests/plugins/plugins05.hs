{-# OPTIONS_GHC -fplugin HomePackagePlugin #-}

-- Tests home-package plugins from OPTIONS pragma
module Main where

main :: IO ()
main = putStrLn "Hello From The Program"
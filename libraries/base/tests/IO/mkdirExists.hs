module Main where

import System.Directory

main :: IO ()
main = do
  createDirectory "foo"
  createDirectory "foo"

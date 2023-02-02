module Main where

import Data.Proxy

main :: IO ()
main = print (read "Thing Proxy" :: Thing (Proxy Int))

data Thing a = Thing a
  deriving (Read,Show)

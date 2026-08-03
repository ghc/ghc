module Main where

import Bar () -- resolves to src/Bar.hs, which declares module Foo

main :: IO ()
main = return ()

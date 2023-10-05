module Main (main) where

import System.Posix.Internals (hostIsThreaded)

main :: IO ()
main = print hostIsThreaded

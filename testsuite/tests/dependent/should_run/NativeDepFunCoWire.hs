module Main where

import NativeDepFunCoClient

main :: IO ()
main = print (observe (error "unused retained index"))

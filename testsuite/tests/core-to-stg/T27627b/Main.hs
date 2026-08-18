module Main where

import Callee
import Caller

main :: IO ()
main = print (a (MkId 3 :: Id Int))

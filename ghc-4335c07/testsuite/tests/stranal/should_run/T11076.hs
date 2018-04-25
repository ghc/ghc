{-
   Test case for a problem where GHC had incorrect strictness
   information for foreign calls with lifted arguments
 -}
{-# OPTIONS_GHC -O0 #-}
module Main where

import T11076A
import Control.Exception
x :: Bool
x = error "OK: x has been forced"

main :: IO ()
main = print (testBool x) `catch`
          \(ErrorCall e) -> putStrLn e -- x should be forced

{-# LANGUAGE ImpredicativeTypes, RebindableSyntax #-}

module Main where

import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import qualified Prelude as P
import Prelude hiding ((>>))


(>>) :: HasCallStack => String -> String -> String
_ >> _ = prettyCallStack callStack

x :: String
x = do
    "ddd"
    "fff"

y :: String
y = "ddd" >> "fff"

main :: IO ()
main = putStrLn x P.>> putStrLn y

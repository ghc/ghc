{-# LANGUAGE LinearTypes, TypeOperators #-}
module Main (main) where

import Data.Typeable
import Data.Maybe

x :: Maybe ((Int -> Int) :~: (Int %1 -> Int))
x = eqT

main = print (isJust x)

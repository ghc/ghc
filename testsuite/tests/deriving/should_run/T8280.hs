{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Prim

data A = A Word# deriving Show

main = print (A (int2Word# 4#))

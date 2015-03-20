{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Prim

data P = Positives Int# Float# Double# Char# Word# deriving Show
data N = Negatives Int# Float# Double# deriving Show

main = do
  print $ Positives 42# 4.23# 4.23## '4'# 4##
  print $ Negatives -4# -4.0# -4.0##

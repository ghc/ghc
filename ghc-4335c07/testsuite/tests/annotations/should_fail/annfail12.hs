module Annfail12 where

-- Testing errors hidden in annotations

{-# ANN f (error "You were meant to see this error!" :: Int) #-}
f x = x
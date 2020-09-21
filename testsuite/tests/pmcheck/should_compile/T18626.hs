{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}

module Lib where

x :: ()
x | let y = True, y = ()

f :: Int -> ()
f _ | y = ()
  where
    y = True

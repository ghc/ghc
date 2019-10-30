{-# LANGUAGE BangPatterns #-}

module Proposal229d ((!)) where

(!) :: a -> b -> (a, b)
x ! y = (x,y)   -- parsed as an operator even with BangPatterns enabled

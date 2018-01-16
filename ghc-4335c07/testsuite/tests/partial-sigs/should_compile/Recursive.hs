{-# LANGUAGE PartialTypeSignatures #-}
module Recursive where

orr :: a -> a -> a
orr = undefined

g :: _
g = f `orr` True

f :: _
f = g

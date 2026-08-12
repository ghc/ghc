-- The raised threshold persuades GHC to inline the worker's stable
-- unfolding in this small program; a larger program does that naturally.
{-# OPTIONS_GHC -funfolding-use-threshold=400 #-}
module Main where
import M2 ( f )
main = print (f 19 12)

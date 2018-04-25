{-# LANGUAGE Arrows #-}

module ShouldCompile where

import Control.Arrow

f :: ArrowApply a => a (a Int Int,Int,Int) Int
f = proc (x,y,z) -> x -<< 2+y

g :: ArrowApply a => Int -> a (a Int Int,Int) Int
g y = proc (x,z) -> x -<< 2+y

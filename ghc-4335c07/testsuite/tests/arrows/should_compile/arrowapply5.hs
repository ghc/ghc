{-# LANGUAGE Arrows #-}

module ShouldCompile where

-- variables bound inside the left argument of -< should be in scope

import Control.Arrow

f :: (Num b, Arrow a) => a b b
f = proc x -> arr (\y -> y-1) -< x

g :: (Num b, Arrow a) => a b b
g = proc x -> (proc y -> returnA -< y-1) -< x

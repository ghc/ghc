{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

import Control.Arrow

g :: Arrow a => a Int c -> a Int c
g f = proc b -> f -< b+1

-- something mysterious is going wrong with the desugarer if the pattern
-- consists of a single variable.

{-# OPTIONS -farrows #-}

module ShouldCompile where

import Control.Arrow

g :: Arrow a => a Int c -> a Int c
g f = proc b -> f -< b+2

{-# OPTIONS -fglasgow-exts -farrows #-}

module ShouldCompile where

import Control.Arrow

comp1 :: Arrow (~>) => (b~>c) -> (c~>d) -> (b~>d)
comp1 f g = proc x -> do
            b <- f -< x
            g -< b


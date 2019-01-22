{-# LANGUAGE MonadComprehensions, ParallelListComp #-}

module Foo where

foo xs ys = [ (f y True, f x 'c')
            | let f _ z = z, x <- xs
            | y <- ys ]

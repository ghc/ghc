{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

f v = (\ (x :: forall a. a->a) -> x) id 'c'

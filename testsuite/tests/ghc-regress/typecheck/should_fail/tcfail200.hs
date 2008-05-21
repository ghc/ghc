{-# OPTIONS -fglasgow-exts #-}

module ShouldFail where

f = let x = ( 1#, 'c' ) in x

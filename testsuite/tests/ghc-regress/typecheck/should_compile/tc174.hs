{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

f x = (# x, x #) :: (# Int, Int #)

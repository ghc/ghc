{-# LANGUAGE UnboxedTuples #-}

module ShouldCompile where

f x = (# x, x #) :: (# Int, Int #)

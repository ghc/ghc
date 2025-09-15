{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

plus :: (a ~ (Int -> Int)) => Int -> a
plus x y = x + y

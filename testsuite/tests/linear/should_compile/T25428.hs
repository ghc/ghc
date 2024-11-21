{-# language LinearTypes #-}

module T25428 where

f :: () %1 -> Int
f x = let !() = x in 0

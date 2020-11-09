{-# LANGUAGE LinearTypes #-}
module T18731 where

f :: a %1 -> b
f x = undefined x

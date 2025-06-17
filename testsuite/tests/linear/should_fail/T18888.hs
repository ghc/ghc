{-# LANGUAGE LinearTypes, GHC2021 #-}
module T18888 where

f :: a %001 -> b
f x = undefined x

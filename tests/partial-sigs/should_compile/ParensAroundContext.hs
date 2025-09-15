{-# LANGUAGE PartialTypeSignatures #-}
module ParensAroundContext where

f :: (_) => a -> a -> Bool
f x y = x == y

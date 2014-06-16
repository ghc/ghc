
{-# OPTIONS_GHC -fwarn-type-defaults #-}

module B (b, b_unused_bind, b_missing_sig) where

b :: Int
b = 5

b_unused_bind :: Int -> Int
b_unused_bind x = 2

b_missing_sig = ()



{-# OPTIONS_GHC -fwarn-missing-signatures #-}

module Main (main, c_type_default, c_unused_bind) where

import A (a)
import B (b)

main :: IO ()
main = print (a + b)

c_type_default :: Int
c_type_default = 2 ^ 2

c_unused_bind :: Int -> Int
c_unused_bind x = 2


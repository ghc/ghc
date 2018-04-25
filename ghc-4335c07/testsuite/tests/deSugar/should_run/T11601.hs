{-# LANGUAGE BangPatterns #-}

module Main where

-- This should fail evern though y is unused
f x = let !(Just (Just y)) = Just undefined in True

main = print (f False)

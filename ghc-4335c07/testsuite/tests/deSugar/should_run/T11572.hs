{-# LANGUAGE BangPatterns #-}

module Main where

main :: IO ()
main = let !_ = (undefined :: ()) in print 2

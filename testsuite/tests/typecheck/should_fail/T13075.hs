{-# LANGUAGE BangPatterns #-}

module Main where

!(Just x) = Nothing

main = putStrLn "hi there!"

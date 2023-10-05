{-# LANGUAGE UnboxedSums #-}

module Main where

import Lib

import Prelude (print, IO)

main :: IO ()
main = do
    print (getInt (flip (# 123 | #)))

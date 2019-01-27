{-# LANGUAGE BangPatterns #-}
-- Main.hs
module Main (main) where

import NetworkRequestHeader

import Control.Monad

main :: IO ()
main = void $ parseHeaderLines []


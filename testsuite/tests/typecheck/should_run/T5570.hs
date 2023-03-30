{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Exts

main :: IO ()
main = print $ D# $ 3.0##

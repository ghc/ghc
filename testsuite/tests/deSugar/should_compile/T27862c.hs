{-# LANGUAGE BangPatterns, MagicHash #-}

module T27862c where

import GHC.Exts

g :: Int -> Int
g !(~(I# x)) = 0

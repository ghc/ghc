{-# LANGUAGE PatternSynonyms, MagicHash #-}
module Main where

import GHC.Base

pattern P1 <- 0
pattern P2 <- 1

f :: Int -> Int#
f P1 = 42#
f P2 = 44#

main = do
    print $ I# (f 0)
    print $ I# (f 1)

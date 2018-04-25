{-# LANGUAGE PatternSynonyms, MagicHash #-}
module Main where

import GHC.Base

pattern P1 = 42#

main = do
    print $ I# P1

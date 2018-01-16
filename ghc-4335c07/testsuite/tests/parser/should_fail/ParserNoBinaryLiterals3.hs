{-# LANGUAGE MagicHash #-}

module ParserNoBinaryLiterals3 where

import GHC.Types

f :: Int -> ()
f (I# 0b0#) = ()
f _         = ()

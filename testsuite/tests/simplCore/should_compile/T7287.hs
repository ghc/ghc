{-# LANGUAGE MagicHash #-}
module T7287 where

import GHC.Prim

{-# RULES
  "int2Word#/word2Int#" forall x. int2Word# (word2Int# x) = x
  #-}

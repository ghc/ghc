{-# LANGUAGE MagicHash #-}
module T7287 where

import GHC.Prim

{-# RULES
  "int2Word#/word2Int#" forall x. int2Word# (word2Int# x) = x
  #-}

{- We get a legitmiate

   T7287.hs:7:3: warning:
       Rule int2Word#/word2Int# may never fire because
         rule "word2Int#" for ‘word2Int#’ might fire first
       Probable fix: add phase [n] or [~n] to the competing rule

because rule "word2Int#" is the constant folding rule that converts
a sufficiently-narrow Word# literal to an Int#.  There is a similar
one for int2Word#, so the whole lot is confluent. -}

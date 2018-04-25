{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MagicHash         #-}
module T11174 where

import GHC.Prim (Int#)

data IntHash a = IntHash Int#
  deriving (Functor, Foldable, Traversable)
data IntHashFun a = IntHashFun ((a -> Int#) -> a)
  deriving Functor
data IntHashTuple a = IntHashTuple Int# a (a, Int, IntHashTuple (a, Int))
  deriving (Functor, Foldable, Traversable)

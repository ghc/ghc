module T5252a( S(..), T ) where

data T = MkT Int Int

data S = MkS {-# UNPACK #-}!T Int

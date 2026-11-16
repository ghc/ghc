{-# LANGUAGE DeriveGeneric #-}
module Micro.Types (Tree(..)) where

import GHC.Generics

data Tree = Leaf | Fork Tree Tree
    deriving (Eq, Ord, Generic)


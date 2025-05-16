{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module T11822 where

import Data.Sequence (Seq, data (:<|))
import Data.Set (Set)

newtype SiblingDependencies = SiblingDependencies Int
  deriving (Eq, Ord, Enum, Integral, Real, Num)

newtype Depth = Depth Int
  deriving (Eq, Ord, Enum, Integral, Real, Num)

data TreeNode prefix
  = OnlyChild prefix
  | LeafLast prefix
  | LeafMid prefix
  | NodeLast prefix
  | NodeMid prefix
  | PrefixedLast prefix (Seq SiblingDependencies) (Set prefix) Depth
  | PrefixedMid prefix (Seq SiblingDependencies) (Set prefix) Depth

mkTreeNode
  :: Ord prefix
  => prefix
  -> Seq SiblingDependencies
  -> Set prefix
  -> Depth
  -> TreeNode prefix
mkTreeNode t     []      _   _   = OnlyChild t
mkTreeNode t     [0]    []   _   = LeafLast t
mkTreeNode t     [_]    []   _   = LeafMid t
mkTreeNode t     [0]     _   0   = LeafLast t
mkTreeNode t     [_]     _   0   = LeafMid t
mkTreeNode t     [0]     _   _   = NodeLast t
mkTreeNode t     [_]     _   _   = NodeMid t
mkTreeNode t (0 :<| ns) ds depth = PrefixedLast t ns ds depth
mkTreeNode t (_ :<| ns) ds depth = PrefixedMid t ns ds depth


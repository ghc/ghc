{-# LANGUAGE TypeFamilies #-}
module Test10307 where

class Foldable t where
  type FoldableConstraint t x :: *
  type FoldableConstraint t x = ()

{-# LANGUAGE PackageImports #-}

module M
  ( module Export
  , cmp
  ) where

import "a" M as Export

cmp :: T -> T -> Bool
cmp x y = x <= y

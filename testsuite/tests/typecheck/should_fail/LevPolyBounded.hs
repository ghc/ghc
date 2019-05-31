-- inspired by comment:25 on #12708

{-# LANGUAGE PolyKinds, TopLevelKindSignatures #-}

module LevPolyBounded where

import Data.Kind
import GHC.Exts

type XBounded :: TYPE r -> Constraint
class XBounded a where
  minBound :: a
  maxBound :: a

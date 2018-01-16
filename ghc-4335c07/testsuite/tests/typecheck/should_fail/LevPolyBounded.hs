-- inspired by comment:25 on #12708

{-# LANGUAGE TypeInType #-}

module LevPolyBounded where

import GHC.Exts

class XBounded (a :: TYPE r) where
  minBound :: a
  maxBound :: a

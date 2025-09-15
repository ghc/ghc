{-# LANGUAGE TypeOperators, GADTs, KindSignatures #-}

-- Tests infix type constructors in GADT declarations

module ShouldCompile where

import Data.Kind

infix 1 `DArrowX`       -- (->) has precedence 0

data DArrowX :: Type -> Type -> Type where
  First   :: a `DArrowX`  a' -> (a,b) `DArrowX` (a',b)

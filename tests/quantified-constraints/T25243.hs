{-# LANGUAGE DataKinds, QuantifiedConstraints, UndecidableInstances #-}
module T25243 where

import GHC.Exts
import Data.Kind

type T :: Constraint -> Constraint -> CONSTRAINT IntRep
type T a b = a => b

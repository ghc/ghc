{-# LANGUAGE TypeFamilies #-}
module T24279 where

import GHC.Exts
import Data.Kind

type F :: (RuntimeRep -> Type) -> Type
type family F a where
  F TYPE = Int
  F CONSTRAINT = Bool

type G :: Type -> RuntimeRep -> Type
type family G a where
  G (a b) = a

-- Should be rejected
foo :: (F (G Constraint)) -> Bool
foo x = x


type family H a b where
  H a a = Int
  H a b = Bool

-- Should be rejected
bar1 :: H TYPE CONSTRAINT -> Int
bar1 x = x

-- Should be rejected
bar2 :: H Type Constraint -> Int
bar2 x = x

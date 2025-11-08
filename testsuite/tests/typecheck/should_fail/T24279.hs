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

-- Now (Nov 2025) accepted
foo :: (F (G Constraint)) -> Bool
foo x = x


type family H a b where
  H a a = Int
  H a b = Bool

-- Now (Nov 2025) accepted
bar1 :: H TYPE CONSTRAINT -> Bool
bar1 x = x

-- Now (Nov 2025) accepted
bar2 :: H Type Constraint -> Bool
bar2 x = x

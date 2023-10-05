{-# LANGUAGE UnliftedNewtypes #-}
module T20387 where

import Data.Kind (Type)
import GHC.Exts (TYPE)
import GHC.Generics (Generic1)

newtype D (a :: TYPE r) = MkD a
  deriving Generic1

type T :: forall j k -> (k -> Type) -> (j -> k) -> j -> Type
data T j k f g a = MkT j k (f (g a))
  deriving Generic1

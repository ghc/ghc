module T21447 where

import Data.Kind (Type)
import GHC.Exts  (TYPE)

type H :: TYPE r -> Type
newtype H a where
  H :: a -> H a

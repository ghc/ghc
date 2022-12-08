{-# LANGUAGE TypeFamilies #-}
module M where

import Data.Kind (Type)

data MP1 a = MP1 a

type family Fixup (f :: Type) (g :: Type) :: Type where
  Fixup f (MP1 f) = Int
  Fixup f f = f

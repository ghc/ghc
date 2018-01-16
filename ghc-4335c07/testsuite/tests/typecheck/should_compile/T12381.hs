{-# LANGUAGE TypeInType, TypeFamilies #-}
module Kinds where

import GHC.Types

type family G (a :: Type) :: Type
type instance G Int = Bool

type family F (a :: Type) :: G a
type instance F Int = True

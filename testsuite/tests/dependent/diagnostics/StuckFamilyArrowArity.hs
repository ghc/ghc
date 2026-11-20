{-# LANGUAGE Modifiers #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fprint-explicit-matchabilities #-}

module StuckFamilyArrowArity where

import Data.Kind (Type)
import GHC.Exts (M, U)

type family Expected (k :: Type) :: Type where
    Expected (Type %U -> Type) = Type

type Bad = (Maybe :: Expected (Type %M -> Type))

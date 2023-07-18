{-# OPTIONS_GHC -ddump-simpl -ddump-simpl -dsuppress-all -dno-typeable-binds -dsuppress-uniques #-}
{-# LANGUAGE UnliftedDatatypes #-}
module TopLevela where

import GHC.Exts (UnliftedType)
import Data.Kind (Type)

type UNat :: UnliftedType
data UNat = UZero | USucc UNat

type Box :: UnliftedType -> Type
data Box a = Box a

x = Box (USucc (USucc (USucc (USucc (USucc UZero)))))

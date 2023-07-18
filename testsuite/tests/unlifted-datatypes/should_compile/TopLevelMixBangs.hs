{-# OPTIONS_GHC -ddump-simpl -ddump-simpl -dsuppress-all -dno-typeable-binds -dsuppress-uniques #-}
{-# LANGUAGE UnliftedDatatypes #-}
module TopLevelMixBangs where

import GHC.Exts (UnliftedType)
import Data.Kind (Type)

type UNat :: UnliftedType
data UNat = UZero | USucc !LNat

data LNat = LZero | LSucc UNat

type Box :: UnliftedType -> Type
data Box a = Box a

x = Box (USucc xa)
xa = LSucc (USucc xb)
xb = LSucc (USucc xc)
xc = LSucc (USucc xd)
xd = LZero

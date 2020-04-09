module GHC.Core.TyCon where

import GhcPrelude
import Unique ( Uniquable )

data TyCon

instance Uniquable TyCon

isTupleTyCon        :: TyCon -> Bool
isUnboxedTupleTyCon :: TyCon -> Bool
isFunTyCon          :: TyCon -> Bool

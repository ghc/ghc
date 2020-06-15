module GHC.Core.TyCon where

import GHC.Prelude
import GHC.Types.Unique ( Uniquable )

data TyCon

instance Uniquable TyCon

isTupleTyCon        :: TyCon -> Bool
isUnboxedTupleTyCon :: TyCon -> Bool
isFunTyCon          :: TyCon -> Bool

module GHC.Core.TyCon where

import GHC.Prelude

data TyCon

isTupleTyCon        :: TyCon -> Bool
isUnboxedTupleTyCon :: TyCon -> Bool
isFunTyCon          :: TyCon -> Bool

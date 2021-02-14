module TyCon where

import GhcPrelude
import Binary

data TyCon
data PrimRep

instance Eq PrimRep
instance Binary PrimRep

isTupleTyCon        :: TyCon -> Bool
isUnboxedTupleTyCon :: TyCon -> Bool
isFunTyCon          :: TyCon -> Bool

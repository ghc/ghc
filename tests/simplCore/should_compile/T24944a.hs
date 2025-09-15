module T24944a where

data TyCon = TyCon {
        tyConNullaryTy :: TyCon,
        tyConDetails :: !TyConDetails
        }

data TyConDetails =
    AlgTyCon Bool
  | PrimTyCon Int
  | PromotedDataCon

myTyCon :: TyCon
myTyCon = TyCon { tyConDetails          = PrimTyCon 1
                , tyConNullaryTy        = id' myTyCon
                }

id' :: TyCon -> TyCon
id' a = a
{-# NOINLINE id' #-}

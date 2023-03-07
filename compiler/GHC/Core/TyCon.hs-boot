module GHC.Core.TyCon where

import GHC.Prelude
import GHC.Types.Unique ( Uniquable )
import {-# SOURCE #-} GHC.Types.Name
import GHC.Utils.Outputable
import GHC.Unit.Types ( WiredIn )

data TyCon

instance Uniquable TyCon
instance Outputable TyCon

type TyConRepName = Name

isTupleTyCon        :: TyCon -> Bool
isUnboxedTupleTyCon :: TyCon -> Bool

tyConRepName_maybe  :: TyCon -> Maybe TyConRepName
mkPrelTyConRepName  :: Name -> WiredIn TyConRepName
tyConName :: TyCon -> Name

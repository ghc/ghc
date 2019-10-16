module DataCon where

import GhcPrelude
import Var( TyCoVar )
import {-# SOURCE #-} TyCon( TyCon )
import FieldLabel ( FieldLabel )
import Unique ( Uniquable )
import Name   ( NamedThing )

data DataCon

dataConExTyCoVars  :: DataCon -> [TyCoVar]
dataConFieldLabels :: DataCon -> [FieldLabel]
dataConTyCon       :: DataCon -> TyCon
isEnumDataCon      :: DataCon -> Bool
isUnboxedSumCon    :: DataCon -> Bool

instance Uniquable  DataCon
instance NamedThing DataCon

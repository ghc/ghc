module GHC.Types.TyThing where

import {-# SOURCE #-} GHC.Core.TyCon
import {-# SOURCE #-} GHC.Core.DataCon
import {-# SOURCE #-} GHC.Types.Var
import {-# SOURCE #-} GHC.Types.Name

instance NamedThing TyThing


data TyThing
mkATyCon :: TyCon -> TyThing
mkAnId   :: Id -> TyThing
mkADataCon :: DataCon -> TyThing

module GHC.Types.TyThing where

import {-# SOURCE #-} GHC.Core.TyCon
import {-# SOURCE #-} GHC.Types.Var
import GHC.Utils.Misc

data TyThing
mkATyCon :: TyCon -> TyThing
mkAnId   :: Id -> TyThing
tyThingId :: HasDebugCallStack => TyThing -> Id

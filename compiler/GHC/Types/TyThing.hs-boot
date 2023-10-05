module GHC.Types.TyThing where

import {-# SOURCE #-} GHC.Core.TyCon
import {-# SOURCE #-} GHC.Types.Var

data TyThing
mkATyCon :: TyCon -> TyThing
mkAnId   :: Id -> TyThing

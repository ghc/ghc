module GHC.Types.RepType where

import GHC.Utils.Misc (HasDebugCallStack)
import GHC.Utils.Outputable (SDoc)
import {-# SOURCE #-} GHC.Core.TyCo.Rep (Type)
import Data.Maybe (Maybe)
import GHC.Core.TyCon (PrimRep)

runtimeRepPrimRep_maybe :: HasDebugCallStack => SDoc -> Type -> Maybe [PrimRep]    
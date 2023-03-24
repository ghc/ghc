module GHC.Types.RepType where

import Data.Bool
import GHC.Core.TyCo.Rep (Type)
import GHC.Utils.Misc (HasDebugCallStack)

isZeroBitTy :: HasDebugCallStack => Type -> Bool



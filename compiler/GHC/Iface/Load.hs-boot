module GHC.Iface.Load where

import GHC.Unit.Module (Module)
import GHC.Tc.Utils.Monad (IfM)
import GHC.Unit.Module.ModIface (SimpleModIface)
import GHC.Utils.Outputable (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl SimpleModIface

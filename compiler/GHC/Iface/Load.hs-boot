module GHC.Iface.Load where

import GHC.Unit.Module (Module)
import GHC.Tc.Utils.Monad (IfM)
import GHC.Driver.Types (ModIface)
import GHC.Utils.Outputable (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface

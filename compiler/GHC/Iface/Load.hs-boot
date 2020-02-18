module GHC.Iface.Load where

import Module (Module)
import TcRnMonad (IfM)
import GHC.Driver.Types (ModIface)
import Outputable (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface

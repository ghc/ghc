module GHC.Driver.Config.CmmToAsm
where

import GHC.CmmToAsm.Config
import GHC.Unit.Types
import {-# SOURCE #-} GHC.Driver.Session

initNCGConfig :: DynFlags -> Module -> NCGConfig

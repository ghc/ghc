module GHC.Driver.Main where

import GHC.Driver.Env
import GHC.Linker.Types
import GHC.Prelude
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface

initModDetails :: HscEnv -> ModIface -> IO ModDetails

initWholeCoreBindings :: HscEnv -> ModIface -> ModDetails -> Linkable -> IO Linkable

module GHC.Driver.Main where

import GHC.Driver.Env
import GHC.Linker.Types
import GHC.Prelude
import GHC.Unit.Module.ModIface

initWholeCoreBindingsEps :: HscEnv -> ModIface -> Linkable -> IO Linkable

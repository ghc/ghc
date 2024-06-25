module GHC.Driver.Main where

import GHC.Driver.Env
import GHC.Iface.Recomp
import GHC.Linker.Types
import GHC.Prelude
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModSummary

initModDetails :: HscEnv -> ModIface -> IO ModDetails

loadByteCode :: ModIface -> ModSummary -> IO (MaybeValidated Linkable)

initWholeCoreBindings :: HscEnv -> ModIface -> ModDetails -> Linkable -> IO Linkable

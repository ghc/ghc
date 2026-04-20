module GHC.Driver.Main.Compile where

import GHC.Driver.Env.Types (HscEnv)
import GHC.Linker.Types (Linkable)
import GHC.Prelude.Basic
import GHC.Types.TypeEnv (TypeEnv)
import GHC.Unit.Module.Location (ModLocation)
import GHC.Unit.Module.ModIface (ModIface)
import GHC.Unit.Module (ModuleName)
import GHC.Unit.Module.ModGuts (CgGuts)

loadIfaceByteCode ::
  HscEnv ->
  ModIface ->
  ModLocation ->
  TypeEnv ->
  Maybe (IO Linkable)

data CgInteractiveGuts

mkCgInteractiveGuts :: CgGuts -> CgInteractiveGuts

generateFreshByteCodeLinkable :: HscEnv
  -> ModuleName
  -> CgInteractiveGuts
  -> ModLocation
  -> IO Linkable
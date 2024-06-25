module GHC.Driver.Main where

import GHC.Driver.Env.Types (HscEnv)
import GHC.Linker.Types (Linkable)
import GHC.Prelude.Basic
import GHC.Types.TypeEnv (TypeEnv)
import GHC.Unit.Module.Location (ModLocation)
import GHC.Unit.Module.ModIface (ModIface)

loadIfaceByteCode ::
  HscEnv ->
  ModIface ->
  ModLocation ->
  TypeEnv ->
  Maybe (IO Linkable)

module GHC.Unit.Module.Env where

import GhcPrelude ()
import GHC.Types.Unique.FM

type ModuleNameEnv elt = UniqFM elt

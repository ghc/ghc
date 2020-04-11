module GHC.Types.Module.Env where

import GHC.Types.Unique.FM

type ModuleNameEnv elt = UniqFM elt

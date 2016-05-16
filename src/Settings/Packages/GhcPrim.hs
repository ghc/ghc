module Settings.Packages.GhcPrim (ghcPrimPackageArgs) where

import Expression
import GHC
import Predicates

ghcPrimPackageArgs :: Args
ghcPrimPackageArgs = package ghcPrim ?
    builder GhcCabal ? arg "--flag=include-ghc-prim"

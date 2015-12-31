module Settings.Packages.GhcPrim (ghcPrimPackageArgs) where

import Expression
import GHC (ghcPrim)
import Predicates (builder, package)

ghcPrimPackageArgs :: Args
ghcPrimPackageArgs = package ghcPrim ?
    builder GhcCabal ? arg "--flag=include-ghc-prim"

module Settings.Packages.GhcPkg (ghcPkgPackageArgs) where

import Expression
import Oracles.Flag (crossCompiling)

ghcPkgPackageArgs :: Args
ghcPkgPackageArgs = package ghcPkg ? builder GhcCabal ? crossCompiling ? arg "-f-terminfo"

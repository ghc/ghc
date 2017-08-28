module Settings.Packages.GhcPkg (ghcPkgPackageArgs) where

import Expression
import UserSettings (crossCompiling)

ghcPkgPackageArgs :: Args
ghcPkgPackageArgs = crossCompiling ? package ghcPkg ? builder GhcCabal ? arg "-f-terminfo"

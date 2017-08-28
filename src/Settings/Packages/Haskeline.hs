module Settings.Packages.Haskeline (haskelinePackageArgs) where

import Base
import Expression
import GHC
import UserSettings (crossCompiling)

haskelinePackageArgs :: Args
haskelinePackageArgs =
    package haskeline ? builder GhcCabal ? crossCompiling ? arg "-f-terminfo"

module Settings.Packages.Haskeline (haskelinePackageArgs) where

import Expression
import UserSettings (crossCompiling)

haskelinePackageArgs :: Args
haskelinePackageArgs =
    package haskeline ? builder GhcCabal ? crossCompiling ? arg "-f-terminfo"

module Settings.Packages.Ghci (ghciPackageArgs) where

import GHC
import Expression

ghciPackageArgs :: Args
ghciPackageArgs = package ghci ? notStage0 ? builder GhcCabal ? arg "--flags=ghci"

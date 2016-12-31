module Settings.Packages.Ghci (ghciPackageArgs) where

import GHC
import Predicate

ghciPackageArgs :: Args
ghciPackageArgs = notStage0 ? package ghci ? builder GhcCabal ? arg "--flags=ghci"

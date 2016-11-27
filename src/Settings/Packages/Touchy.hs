module Settings.Packages.Touchy (touchyPackageArgs) where

import GHC
import Predicate

touchyPackageArgs :: Args
touchyPackageArgs = package touchy ?
    builder Ghc ? mconcat [ arg "-no-hs-main"
                          , remove ["-hide-all-packages"] ]

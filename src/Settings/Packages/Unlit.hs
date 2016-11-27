module Settings.Packages.Unlit (unlitPackageArgs) where

import GHC
import Predicate

unlitPackageArgs :: Args
unlitPackageArgs = package unlit ?
    builder Ghc ? mconcat [ arg "-no-hs-main"
                          , remove ["-hide-all-packages"] ]

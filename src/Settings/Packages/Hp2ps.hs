module Settings.Packages.Hp2ps (hp2psPackageArgs) where

import GHC
import Predicate

hp2psPackageArgs :: Args
hp2psPackageArgs = package hp2ps ?
    builder Ghc ? mconcat [ arg "-no-hs-main"
                          , remove ["-hide-all-packages"] ]

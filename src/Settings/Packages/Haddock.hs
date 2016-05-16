module Settings.Packages.Haddock (haddockPackageArgs) where

import GHC
import Predicate

haddockPackageArgs :: Args
haddockPackageArgs = package haddock ?
    builder GhcCabal ? append ["--flag", "in-ghc-tree"]

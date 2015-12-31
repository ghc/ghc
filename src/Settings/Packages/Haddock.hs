module Settings.Packages.Haddock (haddockPackageArgs) where

import Expression
import GHC (haddock)
import Predicates (builder, package)

haddockPackageArgs :: Args
haddockPackageArgs = package haddock ?
    builder GhcCabal ? append ["--flag", "in-ghc-tree"]

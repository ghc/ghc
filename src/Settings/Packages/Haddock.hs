module Settings.Packages.Haddock (haddockPackageArgs) where

import GHC
import Expression

haddockPackageArgs :: Args
haddockPackageArgs = package haddock ?
    builder GhcCabal ? append ["--flag", "in-ghc-tree"]

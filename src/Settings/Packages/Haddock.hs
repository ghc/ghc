module Settings.Packages.Haddock (haddockPackageArgs) where

import Expression

haddockPackageArgs :: Args
haddockPackageArgs = package haddock ?
    builder GhcCabal ? pure ["--flag", "in-ghc-tree"]

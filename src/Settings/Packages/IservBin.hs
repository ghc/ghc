module Settings.Packages.IservBin (iservBinPackageArgs) where

import Expression
import GHC (iservBin)
import Predicates (builder, package)

iservBinPackageArgs :: Args
iservBinPackageArgs = package iservBin ? do
    mconcat [ builder Ghc ? arg "-no-hs-main" ]

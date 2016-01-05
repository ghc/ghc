module Settings.Packages.IservBin (iservBinPackageArgs) where

import Expression
import GHC (iservBin)
import Predicates (builderGhc, package)

iservBinPackageArgs :: Args
iservBinPackageArgs = package iservBin ? do
    mconcat [ builderGhc ?
              mconcat [ arg "-no-hs-main" ]]

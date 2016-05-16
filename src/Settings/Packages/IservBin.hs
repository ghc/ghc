module Settings.Packages.IservBin (iservBinPackageArgs) where

import GHC
import Predicate

iservBinPackageArgs :: Args
iservBinPackageArgs = package iservBin ? builder Ghc ? arg "-no-hs-main"

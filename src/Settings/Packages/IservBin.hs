module Settings.Packages.IservBin (iservBinPackageArgs) where

import Expression
import GHC
import Predicates

iservBinPackageArgs :: Args
iservBinPackageArgs = package iservBin ? builder Ghc ? arg "-no-hs-main"

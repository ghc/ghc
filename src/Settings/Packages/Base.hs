module Settings.Packages.Base (basePackageArgs) where

import Base
import Expression
import GHC (base)
import Predicates (builder, package)
import Settings

basePackageArgs :: Args
basePackageArgs = package base ?
    builder GhcCabal ? arg ("--flags=" ++ takeFileName (pkgPath integerLibrary))

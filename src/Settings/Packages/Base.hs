module Settings.Packages.Base (basePackageArgs) where

import Base
import Expression
import GHC
import Predicates
import Settings

basePackageArgs :: Args
basePackageArgs = package base ?
    builder GhcCabal ? arg ("--flags=" ++ takeFileName (pkgPath integerLibrary))

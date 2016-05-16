module Settings.Packages.Base (basePackageArgs) where

import Base
import GHC
import Predicate
import Settings

basePackageArgs :: Args
basePackageArgs = package base ?
    builder GhcCabal ? arg ("--flags=" ++ takeFileName (pkgPath integerLibrary))

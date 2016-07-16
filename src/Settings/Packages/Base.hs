module Settings.Packages.Base (basePackageArgs) where

import Base
import GHC
import Predicate
import UserSettings

basePackageArgs :: Args
basePackageArgs = package base ? mconcat
    [ builder GhcCabal ? arg ("--flags=" ++ takeFileName (pkgPath integerLibrary))
    , builder Cc ? arg "-O2" ] -- Fix the 'unknown symbol stat' issue, see #259.

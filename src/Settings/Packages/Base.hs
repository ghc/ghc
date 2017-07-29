module Settings.Packages.Base (basePackageArgs) where

import GHC
import Predicate
import Settings

basePackageArgs :: Args
basePackageArgs = package base ? mconcat
    [ builder GhcCabal ? arg ("--flags=" ++ integerLibraryName)
    , builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ] -- Fix the 'unknown symbol stat' issue, see #259.

module Settings.Packages.Base (basePackageArgs) where

import Expression
import Settings

basePackageArgs :: Args
basePackageArgs = package base ? do
    integerLibrary <- expr integerLibraryName
    mconcat [ builder GhcCabal ? arg ("--flags=" ++ integerLibrary)
            -- Fix the 'unknown symbol stat' issue, see #259.
            , builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ]

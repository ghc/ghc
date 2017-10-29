module Settings.Packages.Base (basePackageArgs) where

import Expression
import Settings

basePackageArgs :: Args
basePackageArgs = package base ? do
    integerLibrary <- expr integerLibraryName
    mconcat [ builder GhcCabal ? arg ("--flags=" ++ integerLibrary)
            -- This fixes the 'unknown symbol stat' issue.
            -- See: https://github.com/snowleopard/hadrian/issues/259.
            , builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ]

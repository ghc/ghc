module Rules.Generators.GhcSplit (generateGhcSplit) where

import Base
import Expression
import Oracles
import Rules.Generators.Common

ghcSplitSource :: FilePath
ghcSplitSource = "driver/split/ghc-split.prl"

generateGhcSplit :: Expr String
generateGhcSplit = do
    trackSource "Rules/Generators/GhcSplit.hs"
    targetPlatform <- getSetting TargetPlatform
    ghcEnableTNC   <- yesNo ghcEnableTablesNextToCode
    perlPath       <- getBuilderPath Perl
    contents       <- lift $ readFileLines ghcSplitSource
    return . unlines $
        [ "#!" ++ perlPath
        , "$TARGETPLATFORM = " ++ quote targetPlatform ++ ";"
        -- I don't see where the ghc-split tool uses TNC, but
        -- it's in the build-perl macro.
        , "$TABLES_NEXT_TO_CODE = " ++ quote ghcEnableTNC ++ ";"
        ] ++ contents

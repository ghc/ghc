module Rules.Generators.GhcSplit (generateGhcSplit) where

import Base
import Expression
import Oracles.Config.Setting
import Rules.Generators.Common
import Settings

ghcSplitSource :: FilePath
ghcSplitSource = "driver/split/ghc-split.pl"

-- | Generate the ghc-split Perl script
-- ref: rules/build-perl.mk
generateGhcSplit :: Expr String
generateGhcSplit = do
    trackSource "Rules/Generators/GhcSplit.hs"
    targetPlatform <- getSetting TargetPlatform
    ghcEnableTNC   <- yesNo ghcEnableTablesNextToCode
    perlPath       <- getBuilderPath Perl
    contents       <- expr $ readFileLines ghcSplitSource
    return . unlines $
        [ "#!" ++ perlPath
        , "my $TARGETPLATFORM = " ++ show targetPlatform ++ ";"
        -- I don't see where the ghc-split tool uses TNC, but
        -- it's in the build-perl macro.
        , "my $TABLES_NEXT_TO_CODE = " ++ show ghcEnableTNC ++ ";"
        ] ++ contents

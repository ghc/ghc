module Rules.Generators.GhcSplit (generateGhcSplit) where

import Base
import Expression
import Oracles
import Settings.User

generateGhcSplit :: Expr String
generateGhcSplit = do
    let yesNo = lift . fmap (\x -> if x then "YES" else "NO")
    perl <- getBuilderPath Perl
    let script = "driver/split/ghc-split.prl"
    when trackBuildSystem . lift $
        need [sourcePath -/- "Rules" -/- "Generators" -/- "GhcSplit.hs"]
    lift $ need [script]
    targetPlatform <- getSetting TargetPlatform
    ghcEnableTNC   <- yesNo ghcEnableTablesNextToCode
    contents       <- lift $ readFileLines script
    return . unlines $
        [ "#!" ++ perl
        , "$TARGETPLATFORM = \"" ++ targetPlatform ++ "\";"
        -- I don't see where the ghc-split tool uses TNC, but
        -- it's in the build-perl macro.
        , "$TABLES_NEXT_TO_CODE = \"" ++ ghcEnableTNC ++ "\";"
        ] ++ contents

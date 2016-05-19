module Settings.Builders.DeriveConstants (deriveConstantsBuilderArgs) where

import Base
import Oracles.Config.Flag
import Oracles.Config.Setting
import Predicate
import Settings.Builders.Common

-- TODO: do we need to support `includes_CC_OPTS += -DDYNAMIC_BY_DEFAULT`?
deriveConstantsBuilderArgs :: Args
deriveConstantsBuilderArgs = builder DeriveConstants ? do
    cFlags                <- fromDiffExpr includeCcArgs
    [outputFile, tempDir] <- getOutputs
    mconcat
        [ output "//DerivedConstants.h"             ? arg "--gen-header"
        , output "//GHCConstantsHaskellType.hs"     ? arg "--gen-haskell-type"
        , output "//platformConstants"              ? arg "--gen-haskell-value"
        , output "//GHCConstantsHaskellWrappers.hs" ? arg "--gen-haskell-wrappers"
        , output "//GHCConstantsHaskellExports.hs"  ? arg "--gen-haskell-exports"
        , arg "-o", arg outputFile
        , arg "--tmpdir", arg tempDir
        , arg "--gcc-program", arg =<< getBuilderPath (Cc Compile Stage1)
        , append . concat $ map (\a -> ["--gcc-flag", a]) cFlags
        , arg "--nm-program", arg =<< getBuilderPath Nm
        , specified Objdump ? mconcat [ arg "--objdump-program"
                                      , arg =<< getBuilderPath Objdump ]
        , arg "--target-os", argSetting TargetOs ]

includeCcArgs :: Args
includeCcArgs = do
    confCcArgs <- getSettingList $ ConfCcArgs Stage1
    mconcat [ cArgs
            , cWarnings
            , append confCcArgs
            , flag GhcUnregisterised ? arg "-DUSE_MINIINTERPRETER"
            , includesArgs
            , arg "-Irts"
            , notM ghcWithSMP ? arg "-DNOSMP"
            , arg "-fcommon" ]

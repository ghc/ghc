module Settings.Builders.DeriveConstants (deriveConstantsBuilderArgs) where

import Base
import Oracles.Config.Flag
import Oracles.Config.Setting
import Predicate
import Settings.Builders.Common
import Settings.Paths

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
        , arg "--gcc-program", arg =<< getBuilderPath (Cc CompileC Stage1)
        , append $ concatMap (\a -> ["--gcc-flag", a]) cFlags
        , arg "--nm-program", arg =<< getBuilderPath Nm
        , specified Objdump ? mconcat [ arg "--objdump-program"
                                      , arg =<< getBuilderPath Objdump ]
        , arg "--target-os", argSetting TargetOs ]

includeCcArgs :: Args
includeCcArgs = mconcat
    [ cArgs
    , cWarnings
    , argSettingList $ ConfCcArgs Stage1
    , flag GhcUnregisterised ? arg "-DUSE_MINIINTERPRETER"
    , arg "-Irts"
    , arg "-Iincludes"
    , arg $ "-I" ++ generatedPath
    , notM ghcWithSMP ? arg "-DNOSMP"
    , arg "-fcommon" ]

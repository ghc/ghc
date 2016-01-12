module Settings.Builders.DeriveConstants (deriveConstantsBuilderArgs) where

import Base
import Expression
import Oracles.Config.Flag
import Oracles.Config.Setting
import Predicates (builder, file)
import Settings.Builders.Common

-- TODO: do we need to support `includes_CC_OPTS += -DDYNAMIC_BY_DEFAULT`?
deriveConstantsBuilderArgs :: Args
deriveConstantsBuilderArgs = builder DeriveConstants ? do
    cFlags            <- fromDiffExpr includeCcArgs
    [output, tempDir] <- getOutputs
    mconcat
        [ file "//DerivedConstants.h"             ? arg "--gen-header"
        , file "//GHCConstantsHaskellType.hs"     ? arg "--gen-haskell-type"
        , file "//platformConstants"              ? arg "--gen-haskell-value"
        , file "//GHCConstantsHaskellWrappers.hs" ? arg "--gen-haskell-wrappers"
        , file "//GHCConstantsHaskellExports.hs"  ? arg "--gen-haskell-exports"
        , arg "-o", arg output
        , arg "--tmpdir", arg tempDir
        , arg "--gcc-program", arg =<< getBuilderPath (Gcc Stage1)
        , append . concat $ map (\a -> ["--gcc-flag", a]) cFlags
        , arg "--nm-program", arg =<< getBuilderPath Nm
        , specified Objdump ? mconcat [ arg "--objdump-program"
                                      , arg =<< getBuilderPath Objdump ]
        , arg "--target-os", argSetting TargetOs ]

includeCcArgs :: Args
includeCcArgs = do
    confCcArgs <- getSettingList $ ConfCcArgs Stage1
    mconcat
        [ cArgs
        , cWarnings
        , append confCcArgs
        , flag GhcUnregisterised ? arg "-DUSE_MINIINTERPRETER"
        , includesArgs
        , arg "-Irts"
        , notM ghcWithSMP ? arg "-DNOSMP"
        , arg "-fcommon" ]

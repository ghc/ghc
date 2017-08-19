module Settings.Builders.DeriveConstants (deriveConstantsBuilderArgs) where

import Settings.Builders.Common

-- TODO: do we need to support `includes_CC_OPTS += -DDYNAMIC_BY_DEFAULT`?
deriveConstantsBuilderArgs :: Args
deriveConstantsBuilderArgs = builder DeriveConstants ? do
    cFlags <- includeCcArgs
    outs   <- getOutputs
    let (outputFile, tempDir) = case outs of
            [a, b] -> (a, b)
            _      -> error $ "DeriveConstants: expected two outputs, got " ++ show outs
    mconcat
        [ output "//DerivedConstants.h"             ? arg "--gen-header"
        , output "//GHCConstantsHaskellType.hs"     ? arg "--gen-haskell-type"
        , output "//platformConstants"              ? arg "--gen-haskell-value"
        , output "//GHCConstantsHaskellWrappers.hs" ? arg "--gen-haskell-wrappers"
        , output "//GHCConstantsHaskellExports.hs"  ? arg "--gen-haskell-exports"
        , arg "-o", arg outputFile
        , arg "--tmpdir", arg tempDir
        , arg "--gcc-program", arg =<< getBuilderPath (Cc CompileC Stage1)
        , pure $ concatMap (\a -> ["--gcc-flag", a]) cFlags
        , arg "--nm-program", arg =<< getBuilderPath Nm
        , isSpecified Objdump ? mconcat [ arg "--objdump-program"
                                        , arg =<< getBuilderPath Objdump ]
        , arg "--target-os", arg =<< getSetting TargetOs ]

includeCcArgs :: Args
includeCcArgs = do
    root <- getBuildRoot
    mconcat [ cArgs
            , cWarnings
            , getSettingList $ ConfCcArgs Stage1
            , flag GhcUnregisterised ? arg "-DUSE_MINIINTERPRETER"
            , arg "-Irts"
            , arg "-Iincludes"
            , arg $ "-I" ++ root -/- generatedDir
            , notM ghcWithSMP ? arg "-DNOSMP"
            , arg "-fcommon" ]

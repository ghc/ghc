module Settings.Builders.DeriveConstants (
    deriveConstantsBuilderArgs, deriveConstantsPairs
    ) where

import Builder
import Settings.Builders.Common

deriveConstantsPairs :: [(String, String)]
deriveConstantsPairs =
  [ ("DerivedConstants.h", "--gen-header")
  , ("GHCConstantsHaskellType.hs", "--gen-haskell-type")
  , ("platformConstants", "--gen-haskell-value")
  , ("GHCConstantsHaskellWrappers.hs", "--gen-haskell-wrappers")
  , ("GHCConstantsHaskellExports.hs", "--gen-haskell-exports")
  ]

-- TODO: do we need to support `includes_CC_OPTS += -DDYNAMIC_BY_DEFAULT`?
deriveConstantsBuilderArgs :: Args
deriveConstantsBuilderArgs = builder DeriveConstants ? do
    cFlags <- includeCcArgs
    outs   <- getOutputs
    let (outputFile, tempDir) = case outs of
            [a, b] -> (a, b)
            _      -> error $ "DeriveConstants: expected two outputs, got " ++ show outs
    mconcat
        [ mconcat $ flip fmap deriveConstantsPairs $ \(fileName, flag) ->
            output ("//" ++ fileName) ? arg flag
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
    stage <- getStage
    libPath <- expr $ stageLibPath stage
    mconcat [ cArgs
            , cWarnings
            , getSettingList $ ConfCcArgs Stage1
            , flag GhcUnregisterised ? arg "-DUSE_MINIINTERPRETER"
            , arg "-Irts"
            , arg "-Iincludes"
            , arg $ "-I" ++ libPath
            , notM targetSupportsSMP ? arg "-DNOSMP"
            , arg "-fcommon" ]

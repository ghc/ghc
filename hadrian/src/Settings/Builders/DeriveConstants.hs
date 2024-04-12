module Settings.Builders.DeriveConstants (
    deriveConstantsBuilderArgs
    ) where

import Builder
import Settings.Builders.Common
import GHC.Toolchain (tgtCCompiler, ccProgram, tgtUnregisterised)
import GHC.Toolchain.Program

deriveConstantsPairs :: [(String, String)]
deriveConstantsPairs =
  [ ("Constants.hs", "--gen-haskell-type")
  , ("DerivedConstants.h", "--gen-header")
  ]

deriveConstantsBuilderArgs :: Args
deriveConstantsBuilderArgs = builder DeriveConstants ? do
    cFlags <- includeCcArgs
    outs   <- getOutputs
    let (outputFile, mode, tempDir) = case outs of
            [ofile, mode, tmpdir] -> (ofile,mode,tmpdir)
            [ofile, tmpdir]
               | Just mode <- lookup (takeFileName ofile) deriveConstantsPairs
               -> (ofile, mode, tmpdir)
               | otherwise
               -> error $ "DeriveConstants: invalid output file, got " ++ show (takeFileName ofile)
            _  -> error $ "DeriveConstants: unexpected outputs, got " ++ show outs
    mconcat
        [ arg mode
        , arg "-o", arg outputFile
        , arg "--tmpdir", arg tempDir
        , arg "--gcc-program", arg =<< getBuilderPath (Cc CompileC Stage1)
        , pure $ concatMap (\a -> ["--gcc-flag", a]) cFlags
        , arg "--nm-program", arg =<< getBuilderPath Nm
        , isSpecified Objdump ? mconcat [ arg "--objdump-program"
                                        , arg =<< getBuilderPath Objdump ]
        , arg "--target-os", arg =<< queryTarget queryOS ]

includeCcArgs :: Args
includeCcArgs = do
    stage <- getStage
    rtsPath <- expr $ rtsBuildPath stage
    mconcat [ cArgs
            , cWarnings
            , prgFlags . ccProgram . tgtCCompiler <$> expr (targetStage Stage1)
            , queryTargetTarget tgtUnregisterised ? arg "-DUSE_MINIINTERPRETER"
            , arg "-Irts"
            , arg "-Irts/include"
            , arg $ "-I" ++ rtsPath </> "include"
            , notM targetSupportsSMP ? arg "-DNOSMP"
            , arg "-fcommon" ]

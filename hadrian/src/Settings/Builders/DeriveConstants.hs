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

-- MP: Why is Stage1 hard-coded here, looks wrong
deriveConstantsBuilderArgs :: Args
deriveConstantsBuilderArgs = builder DeriveConstants ? do
    cFlags <- includeCcArgs
    outs   <- getOutputs
    stage <- getStage
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
        , arg "--gcc-program", arg =<< getBuilderPath (Cc CompileC stage)
        , pure $ concatMap (\a -> ["--gcc-flag", a]) cFlags
        , arg "--nm-program", arg =<< getBuilderPath (Nm stage)
        , isSpecified Objdump ? mconcat [ arg "--objdump-program"
                                        , arg =<< getBuilderPath Objdump ]
        , arg "--target-os", arg =<< queryTarget stage queryOS ]

includeCcArgs :: Args
includeCcArgs = do
    stage <- getStage
    rtsPath <- expr $ rtsBuildPath stage
    mconcat [ cArgs
            , cWarnings
            , prgFlags . ccProgram . tgtCCompiler <$> expr (targetStage stage)
            , queryTargetTarget stage tgtUnregisterised ? arg "-DUSE_MINIINTERPRETER"
            , arg "-Irts"
            , arg "-Irts/include"
            , arg $ "-I" ++ rtsPath </> "include"
            , notM (targetSupportsSMP stage) ? arg "-DNOSMP"
            , arg "-fcommon" ]

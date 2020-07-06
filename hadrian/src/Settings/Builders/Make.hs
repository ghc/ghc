module Settings.Builders.Make (makeBuilderArgs, validateBuilderArgs) where

import Oracles.Setting
import Packages
import Rules.Gmp
import Settings.Builders.Common
import Settings.Program (programContext)
import CommandLine

makeBuilderArgs :: Args
makeBuilderArgs = do
    threads    <- shakeThreads <$> expr getShakeOptions
    stage      <- getStage
    gmpPath    <- expr (gmpBuildPath stage)
    libffiPaths <- forM [Stage1 ..] $ \s -> expr (libffiBuildPath s)
    let t = show $ max 4 (threads - 2) -- Don't use all Shake's threads
    mconcat $
        (builder (Make gmpPath   ) ? pure ["MAKEFLAGS=-j" ++ t]) :
        [ builder (Make libffiPath) ? pure ["MAKEFLAGS=-j" ++ t, "install"]
        | libffiPath <- libffiPaths ]

validateBuilderArgs :: Args
validateBuilderArgs = builder (Make "testsuite/tests") ? do
    threads             <- shakeThreads <$> expr getShakeOptions
    top                 <- expr topDirectory
    compiler            <- expr $ fullpath ghc
    checkPpr            <- expr $ fullpath checkPpr
    checkExact          <- expr $ fullpath checkExact
    checkApiAnnotations <- expr $ fullpath checkApiAnnotations
    args                <- expr $ userSetting defaultTestArgs
    return [ setTestSpeed $ testSpeed args
           , "THREADS=" ++ show threads
           , "TEST_HC=" ++ (top -/- compiler)
           , "CHECK_PPR=" ++ (top -/- checkPpr)
           , "CHECK_EXACT=" ++ (top -/- checkExact)
           , "CHECK_API_ANNOTATIONS=" ++ (top -/- checkApiAnnotations)
           ]
  where
    fullpath :: Package -> Action FilePath
    fullpath pkg = programPath =<< programContext Stage1 pkg

-- | Support for speed of validation
setTestSpeed :: TestSpeed -> String
setTestSpeed TestFast   = "fasttest"
setTestSpeed TestNormal = "test"
setTestSpeed TestSlow   = "slowtest"

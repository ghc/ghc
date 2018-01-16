{-# OPTIONS_GHC -Wall #-}

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import System.Exit
import System.FilePath
import System.Process (rawSystem)

main :: IO ()
main = defaultMainWithHooks
       simpleUserHooks { hookedPreProcessors = [("pre", myCustomPreprocessor)] }
  where
    myCustomPreprocessor :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
    myCustomPreprocessor _bi lbi _clbi =
      PreProcessor {
        platformIndependent = True,
        runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
          do info verbosity ("Preprocessing " ++ inFile ++ " to " ++ outFile)
             callProcess progPath [inFile, outFile]
        }
      where
        builddir = buildDir lbi
        progName = "my-custom-preprocessor"
        progPath = builddir </> progName </> progName

    -- Backwards compat with process < 1.2.
    callProcess :: FilePath -> [String] -> IO ()
    callProcess path args =
      do exitCode <- rawSystem path args
         case exitCode of ExitSuccess       -> return ()
                          f@(ExitFailure _) -> fail $ "callProcess " ++ show path
                                               ++ " " ++ show args ++ " failed: "
                                               ++ show f

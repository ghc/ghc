module Main (main) where

import Development.Shake

import qualified CmdLineFlag
import qualified Environment
import qualified Rules
import qualified Rules.Clean
import qualified Rules.Install
import qualified Rules.SourceDist
import qualified Rules.Selftest
import qualified Rules.Test
import qualified Settings.Path

main :: IO ()
main = shakeArgsWith options CmdLineFlag.cmdFlags $ \cmdLineFlags targets -> do
    CmdLineFlag.putCmdLineFlags cmdLineFlags
    Environment.setupEnvironment
    return . Just $ if null targets
                    then rules
                    else want targets >> withoutActions rules
  where
    rules :: Rules ()
    rules = do
        Rules.buildRules
        Rules.Clean.cleanRules
        Rules.Install.installRules
        Rules.oracleRules
        Rules.Selftest.selftestRules
        Rules.SourceDist.sourceDistRules
        Rules.Test.testRules
        Rules.topLevelTargets
    options :: ShakeOptions
    options = shakeOptions
        { shakeChange   = ChangeModtimeAndDigest
        , shakeFiles    = Settings.Path.shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True }

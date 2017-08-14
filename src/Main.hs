module Main (main) where

import Development.Shake
import Hadrian.Utilities

import qualified CommandLine
import qualified Environment
import qualified Rules
import qualified Rules.Clean
import qualified Rules.Install
import qualified Rules.SourceDist
import qualified Rules.Selftest
import qualified Rules.Test
import qualified Settings.Path
import qualified UserSettings

main :: IO ()
main = do
    -- Provide access to command line arguments and some user settings through
    -- Shake's type-indexed map 'shakeExtra'.
    argsMap <- CommandLine.cmdLineArgsMap
    let extra = insertExtra UserSettings.buildProgressColour
              $ insertExtra UserSettings.successColour argsMap

        options :: ShakeOptions
        options = shakeOptions
            { shakeChange   = ChangeModtimeAndDigest
            , shakeFiles    = Settings.Path.shakeFilesPath
            , shakeProgress = progressSimple
            , shakeTimings  = True
            , shakeExtra    = extra }

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

    shakeArgsWith options CommandLine.optDescrs $ \_ targets -> do
        Environment.setupEnvironment
        return . Just $ if null targets
                        then rules
                        else want targets >> withoutActions rules

module Main (main) where

import Development.Shake

import qualified Base
import qualified CmdLineFlag
import qualified Environment
import qualified Rules
import qualified Rules.Clean
import qualified Rules.Oracles
import qualified Selftest
import qualified Test

main :: IO ()
main = shakeArgsWith options CmdLineFlag.cmdFlags $ \cmdLineFlags targets -> do
    CmdLineFlag.putCmdLineFlags cmdLineFlags
    Environment.setupEnvironment
    return . Just $ if null targets
                    then rules
                    else want targets >> withoutActions rules
  where
    rules :: Rules ()
    rules = mconcat
        [ Rules.Clean.cleanRules
        , Rules.Oracles.oracleRules
        , Rules.buildRules
        , Rules.topLevelTargets
        , Selftest.selftestRules
        , Test.testRules ]
    options = shakeOptions
        { shakeChange   = ChangeModtimeAndDigest
        , shakeFiles    = Base.shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True }

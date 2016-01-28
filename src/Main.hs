module Main (main) where

import Development.Shake

import qualified Base
import qualified CmdLineFlag
import qualified Environment
import qualified Rules
import qualified Rules.Cabal
import qualified Rules.Clean
import qualified Rules.Config
import qualified Rules.Generate
import qualified Rules.Gmp
import qualified Rules.Libffi
import qualified Rules.Oracles
import qualified Rules.Perl
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
        [ Rules.Cabal.cabalRules
        , Rules.Clean.cleanRules
        , Rules.Config.configRules
        , Rules.Generate.generateRules
        , Rules.Generate.copyRules
        , Rules.Gmp.gmpRules
        , Rules.Libffi.libffiRules
        , Rules.Oracles.oracleRules
        , Rules.Perl.perlScriptRules
        , Rules.generateTargets
        , Rules.packageRules
        , Selftest.selftestRules
        , Test.testRules ]
    options = shakeOptions
        { shakeChange   = ChangeModtimeAndDigest
        , shakeFiles    = Base.shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True }

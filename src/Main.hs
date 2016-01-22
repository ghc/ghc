module Main (main) where

import Development.Shake

import qualified Base
import           CmdLineFlag
import qualified Rules
import qualified Rules.Cabal
import qualified Rules.Clean
import qualified Rules.Config
import qualified Rules.Generate
import qualified Rules.Gmp
import qualified Rules.Libffi
import qualified Rules.Oracles
import qualified Rules.Perl
import qualified Test

main :: IO ()
main = shakeArgsWith options flags $ \cmdLineFlags targets -> do
    putCmdLineFlags cmdLineFlags
    return . Just $ if null targets
                    then rules
                    else want targets >> withoutActions rules
  where
    rules :: Rules ()
    rules = mconcat
        [ Rules.Cabal.cabalRules
        , Rules.Clean.cleanRules
        , Rules.Config.configRules
        , Rules.Generate.copyRules
        , Rules.Generate.generateRules
        , Rules.Perl.perlScriptRules
        , Rules.generateTargets
        , Rules.Gmp.gmpRules
        , Rules.Libffi.libffiRules
        , Rules.Oracles.oracleRules
        , Rules.packageRules
        , Test.testRules ]
    options = shakeOptions
        { shakeChange   = ChangeModtimeAndDigest
        , shakeFiles    = Base.shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True }

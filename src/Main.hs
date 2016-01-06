module Main (main) where

import Development.Shake

import qualified Base             as B
import qualified Rules            as R
import qualified Rules.Cabal      as RCabal
import qualified Rules.Config     as RConfig
import qualified Rules.Copy       as RCopy
import qualified Rules.Generate   as RGen
import qualified Rules.IntegerGmp as RInt
import qualified Rules.Libffi     as RFfi
import qualified Rules.Oracles    as ROracle

main :: IO ()
main = shakeArgs options rules
  where
    rules = mconcat
        [ RCabal.cabalRules
        , RConfig.configRules
        , RCopy.copyRules
        , R.generateTargets
        , RGen.generateRules
        , RFfi.libffiRules
        , RInt.integerGmpRules
        , ROracle.oracleRules
        , R.packageRules ]
    options = shakeOptions
        { shakeFiles    = B.shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True }

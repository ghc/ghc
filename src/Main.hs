import Base
import Rules
import Rules.Cabal
import Rules.Config
import Rules.Generate
import Rules.Libffi
import Rules.IntegerGmp
import Rules.Oracles

main :: IO ()
main = shakeArgs options $ do
    Rules.Cabal.cabalRules
    Rules.Config.configRules
    Rules.Generate.copyRules
    Rules.generateTargets
    Rules.Generate.generateRules
    Rules.Libffi.libffiRules
    Rules.IntegerGmp.integerGmpRules
    Rules.Oracles.oracleRules
    Rules.packageRules
  where
    options = shakeOptions
        { shakeFiles    = shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True }

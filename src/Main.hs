import Base
import Rules
import Rules.Cabal
import Rules.Config
import Rules.Generate
import Rules.Copy
import Rules.Libffi
import Rules.IntegerGmp
import Rules.Oracles

main :: IO ()
main = shakeArgs options $ do
    cabalRules      -- see Rules.Cabal
    configRules     -- see Rules.Config
    copyRules       -- see Rules.Copy
    generateTargets -- see Rules
    generateRules   -- see Rules.Generate
    libffiRules     -- see Rules.Libffi
    integerGmpRules -- see Rules.IntegerGmp
    oracleRules     -- see Rules.Oracles
    packageRules    -- see Rules
  where
    options = shakeOptions
        { shakeFiles    = shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True }

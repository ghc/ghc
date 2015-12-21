import Base
import Rules
import Rules.Cabal
import Rules.Config
import Rules.Oracles

main :: IO ()
main = shakeArgs options $ do
    generateTargets -- see Rules
    packageRules    -- see Rules
    cabalRules      -- see Rules.Cabal
    configRules     -- see Rules.Config
    oracleRules     -- see Rules.Oracles
  where
    options = shakeOptions
        { shakeFiles    = shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True }

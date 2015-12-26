import Base
import Rules
import Rules.Cabal
import Rules.Config
import Rules.Generate
import Rules.Oracles

main :: IO ()
main = shakeArgs options $ do
    cabalRules      -- see Rules.Cabal
    configRules     -- see Rules.Config
    generateTargets -- see Rules
    generateRules   -- see Rules.Generate
    oracleRules     -- see Rules.Oracles
    packageRules    -- see Rules
  where
    options = shakeOptions
        { shakeFiles    = shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True }

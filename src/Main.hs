import Base
import Rules
import Rules.Cabal
import Rules.Config
import Rules.Oracles

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles = shakeFilesPath} $ do
    generateTargets -- see module Rules
    packageRules    -- see module Rules
    cabalRules      -- see module Rules.Cabal
    configRules     -- see module Rules.Config
    oracleRules     -- see module Rules.Oracles

import Base
import Rules

main = shakeArgs shakeOptions{shakeFiles = shakeFilesPath} $ do
    oracleRules     -- see module Rules.Oracles
    packageRules    -- see module Rules
    configRules     -- see module Rules.Config
    generateTargets -- see module Rules

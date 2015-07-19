import Rules
import Development.Shake

main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    oracleRules     -- see module Rules.Oracles
    packageRules    -- see module Rules
    configRules     -- see module Rules.Config
    generateTargets -- see module Rules

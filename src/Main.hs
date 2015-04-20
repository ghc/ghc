import Base
import Rules
import Config
import Oracles

main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    oracleRules     -- see module Oracles
    packageRules    -- see module Rules
    autoconfRules   -- see module Config
    configureRules  -- see module Config
    generateTargets -- see module Rules


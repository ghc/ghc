import Base
import Config
import Oracles
import Package
import Targets
import Settings

main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    oracleRules
    autoconfRules
    configureRules
    --packageRules

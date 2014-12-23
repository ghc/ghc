import Base
import Config
import Oracles
import Package

main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    oracleRules        
    autoconfRules        
    configureRules
    packageRules

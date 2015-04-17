import Base
import Config
import Oracles
import Package
import Targets
import Settings
import Expression.Simplify

main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    oracleRules
    autoconfRules
    configureRules
    --packageRules

    action $ do
        putNormal $ "targetPackages = " ++ show (simplify targetPackages)
        putNormal $ "\ntargetWays = " ++ show (simplify targetWays)


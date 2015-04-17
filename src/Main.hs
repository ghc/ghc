import Base
import Config
import Oracles
import Package
import Targets
import Settings
import Expression.Base
import Expression.Simplify
import Expression.Resolve

main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    oracleRules
    autoconfRules
    configureRules
    --packageRules

    action $ do
        putNormal $ "\ntargetPackages = " ++ show (simplify targetPackages)
        putNormal $ "\n\ntargetWays = " ++ show (simplify targetWays)
        putNormal $ "\n\n=============================\n"

        -- Read config file
        targetPackages' <- resolveConfig targetPackages
        targetWays' <- resolveConfig targetWays

        -- Build stages
        forM_ [Stage0 ..] $ \stage -> do
            putNormal $ "Stage = " ++ show stage
            let packages = setStage stage targetPackages'
                ways     = setStage stage targetWays'
            putNormal $ "\n packages = " ++ show (simplify packages)
            putNormal $ "\n ways = " ++ show (simplify ways)

        --forM_ targetPackages $ \pkg @ (Package name path _ todo) -> do
        --        forM_ todo $ \todoItem @ (stage, dist, settings) -> do

        --            -- Want top .o and .a files for the pkg/todo combo
        --            -- We build *only one* vanilla .o file (not sure why)
        --            -- We build .way_a file for each way (or its dynamic version).
        --            -- TODO: Check BUILD_GHCI_LIB flag to decide if .o is needed
        --            -- TODO: move this into a separate file (perhaps, to Targets.hs?)
        --            action $ when (buildWhen settings) $ do
        --                let pathDist = path </> dist
        --                    buildDir = pathDist </> "build"
        --                key <- showArg (PackageKey pathDist)
        --                let oFile = buildDir </> "Hs" ++ key <.> "o"
        --                ways'  <- ways settings
        --                libFiles <- forM ways' $ \way -> do
        --                    extension <- libsuf way
        --                    return $ buildDir </> "libHs" ++ key <.> extension
        --                need $ [oFile] ++ libFiles

        --            -- Build rules for the package
        --            buildPackage pkg todoItem

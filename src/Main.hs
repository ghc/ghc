import Base
import Config
import Oracles
import Package
import Targets
import Settings
import Expression.Base
import Expression.Simplify
import Expression.Resolve
import Util

buildSettings = empty

setBuildDir = undefined

buildPackage :: Package -> Ways -> Settings -> Action ()
buildPackage = undefined

main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    oracleRules
    autoconfRules
    configureRules
    --packageRules

    action $ do
        forM_ [Stage0 ..] $ \stage -> do
            pkgs <- resolve $ setStage stage targetPackages
            case linearise pkgs of
                Nothing      -> redError "Cannot determine target packages."
                Just pkgList ->
                    forM_ pkgList $ \pkg -> do
                        let eval = setPackage pkg . setStage stage
                        dirs <- resolve $ eval targetDirectories
                        case linearise dirs of
                            Just [dir] -> do
                                let eval' = setBuildDir dir . eval
                                ways <- resolve $ eval' targetWays
                                stgs <- resolve $ eval' buildSettings
                                buildPackage pkg ways stgs
                            _ -> redError "Cannot determine target directory."




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

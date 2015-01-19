module Package (packageRules) where

import Package.Base
import Package.Data
import Package.Compile
import Package.Library
import Package.Dependencies
import Targets

-- Rule buildPackageX is defined in module Package.X
buildPackage :: Package -> TodoItem -> Rules ()
buildPackage = buildPackageData
            <> buildPackageDependencies
            <> buildPackageCompile
            <> buildPackageLibrary

packageRules :: Rules ()
packageRules = do
    -- TODO: control targets from command line arguments

    -- The package list (targetPackages) is defined in Targets.hs
    forM_ targetPackages $ \pkg @ (Package name path todo) -> do
        forM_ todo $ \todoItem @ (stage, dist, settings) -> do

            -- Want top .o and .a files for the pkg/todo combo
            -- We build *only one* vanilla .o file (not sure why)
            -- We build .way_a file for each way.
            -- TODO: Check BUILD_GHCI_LIB flag to decide if .o is needed
            -- TODO: move this into buildPackage
            action $ do
                let pathDist = path </> dist
                    buildDir = pathDist </> "build"
                key <- showArg (PackageKey pathDist)
                let oFile = buildDir </> "Hs" ++ key <.> "o"
                ways'  <- ways settings
                libFiles <- forM ways' $ \way -> do
                    extension <- libsuf way
                    return $ buildDir </> "libHs" ++ key <.> extension
                need $ [oFile] ++ libFiles

            -- Build rules for the package
            buildPackage pkg todoItem

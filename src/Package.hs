module Package (packageRules) where

import Package.Base
import Package.Data
import Package.Compile
import Package.Library
import Package.Dependencies
import Targets

-- See Package.Base for definitions of basic types

packages :: [Package]
packages = map lib $ libraryPackageNames Stage1
  where
    lib name =
        libraryPackage
            name
            [s | s <- [Stage0, Stage1], name `elem` (libraryPackageNames s)]
            defaultSettings

-- Rule buildPackageX is defined in module Package.X
buildPackage :: Package -> TodoItem -> Rules ()
buildPackage = buildPackageData
            <> buildPackageDependencies
            <> buildPackageCompile
            <> buildPackageLibrary

packageRules :: Rules ()
packageRules = do
    -- TODO: control targets from command line arguments

    forM_ packages $ \pkg @ (Package name path todo) -> do
        forM_ todo $ \todoItem @ (stage, dist, settings) -> do

            -- Want top .o and .a files for the pkg/todo combo
            -- TODO: Check BUILD_GHCI_LIB flag to decide if .o is needed
            action $ do
                let pathDist = path </> dist
                    buildDir = pathDist </> "build"
                [key] <- arg (PackageKey pathDist)
                let oFile = buildDir </> "Hs" ++ key <.> "o"
                ways'  <- ways settings
                aFiles <- forM ways' $ \way -> do
                    extension <- libsuf way
                    return $ buildDir </> "libHs" ++ key <.> extension
                need $ [oFile] ++ aFiles

            -- Build rules for the package
            buildPackage pkg todoItem

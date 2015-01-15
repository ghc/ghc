module Package (packageRules) where

import Package.Base
import Package.Data
import Package.Compile
import Package.Library
import Package.Dependencies

-- See Package.Base for definitions of basic types

-- These are the packages we build:
packages :: [Package]
packages = [ libraryPackage "array"            Stage1 defaultSettings
           , libraryPackage "bin-package-db"   Stage1 defaultSettings
           , libraryPackage "binary"           Stage1 defaultSettings
           , libraryPackage "deepseq"          Stage1 defaultSettings
           , libraryPackage "Cabal/Cabal"      Stage1 defaultSettings
           , libraryPackage "containers"       Stage1 defaultSettings
           , libraryPackage "filepath"         Stage1 defaultSettings
           , libraryPackage "hoopl"            Stage1 defaultSettings
           , libraryPackage "hpc"              Stage1 defaultSettings
           , libraryPackage "parallel"         Stage1 defaultSettings
           , libraryPackage "pretty"           Stage1 defaultSettings
           , libraryPackage "stm"              Stage1 defaultSettings
           , libraryPackage "template-haskell" Stage1 defaultSettings
           , libraryPackage "transformers"     Stage1 defaultSettings ]

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
                let buildDir = path </> dist </> "build"
                    pkgData  = path </> dist </> "package-data.mk"
                [key] <- arg (PackageKey pkgData)
                let oFile = buildDir </> "Hs" ++ key <.> "o"
                ways'  <- ways settings
                aFiles <- forM ways' $ \way -> do
                    extension <- libsuf way
                    return $ buildDir </> "libHs" ++ key <.> extension
                need $ [oFile] ++ aFiles

            -- Build rules for the package
            buildPackage pkg todoItem

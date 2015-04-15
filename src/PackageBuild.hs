module Package (Package, library, setCabal, packageRules) where

--import Package.Base
--import Package.Data
--import Package.Compile
--import Package.Library
--import Package.Dependencies
--import Targets
--import Settings

-- pkgPath is the path to the source code relative to the root
data Package = Package
     {
         pkgName  :: String,   -- Examples: "deepseq", "Cabal/Cabal"
         pkgPath  :: FilePath, -- "libraries/deepseq", "libraries/Cabal/Cabal"
         pkgCabal :: FilePath  -- "deepseq.cabal", "Cabal.cabal"
     }

instance Eq Package where
    (==) = (==) `on` pkgName

libraryPackage :: String -> String -> Package
libraryPackage name cabalName =
    Package
        name
        (unifyPath $ "libraries" </> name)
        cabalName

library :: String -> Package
library name = libraryPackage name (name <.> "cabal")

setCabal :: Package -> FilePath -> Package
setCabal pkg cabalName = pkg { pkgCabal = cabalName }

-- Rule buildPackageX is defined in module Package.X
buildPackage :: Package -> TodoItem -> Rules ()
buildPackage = mempty -- buildPackageData
            --<> buildPackageDependencies
            --<> buildPackageCompile
            --<> buildPackageLibrary

--packageRules :: Rules ()
--packageRules = do
--    -- TODO: control targets from command line arguments

--    -- The package list (targetPackages) is defined in Targets.hs
--    forM_ targetPackages $ \pkg @ (Package name path _ todo) -> do
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

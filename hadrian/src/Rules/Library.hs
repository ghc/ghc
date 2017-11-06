module Rules.Library (
    buildPackageLibrary, buildPackageGhciLibrary, buildDynamicLib
    ) where

import Hadrian.Haskell.Cabal
import qualified System.Directory as IO

import Base
import Context
import Expression hiding (way, package)
import Flavour
import Oracles.ModuleFiles
import Oracles.PackageData
import Oracles.Setting
import Rules.Gmp
import Settings
import Target
import Utilities

libraryObjects :: Context -> Action [FilePath]
libraryObjects context@Context{..} = do
    hsObjs   <- hsObjects    context
    noHsObjs <- nonHsObjects context

    -- This will create split objects if required (we don't track them
    -- explicitly as this would needlessly bloat the Shake database).
    need $ noHsObjs ++ hsObjs

    split <- interpretInContext context =<< splitObjects <$> flavour
    let getSplitObjs = concatForM hsObjs $ \obj -> do
            let dir = dropExtension obj ++ "_" ++ osuf way ++ "_split"
            contents <- liftIO $ IO.getDirectoryContents dir
            return . map (dir -/-) $ filter (not . all (== '.')) contents

    (noHsObjs ++) <$> if split then getSplitObjs else return hsObjs

buildDynamicLib :: Context -> Rules ()
buildDynamicLib context@Context{..} = do
    let libPrefix = "//" ++ contextDir context -/- "libHS" ++ pkgName package
    -- OS X
    libPrefix ++ "*.dylib" %> buildDynamicLibUnix
    -- Linux
    libPrefix ++ "*.so"    %> buildDynamicLibUnix
    -- TODO: Windows
  where
    buildDynamicLibUnix lib = do
        deps <- contextDependencies context
        need =<< mapM pkgLibraryFile deps
        objs <- libraryObjects context
        build $ target context (Ghc LinkHs stage) objs [lib]

buildPackageLibrary :: Context -> Rules ()
buildPackageLibrary context@Context {..} = do
    let libPrefix = "//" ++ contextDir context -/- "libHS" ++ pkgName package
    libPrefix ++ "*" ++ (waySuffix way <.> "a") %%> \a -> do
        objs <- libraryObjects context
        asuf <- libsuf way
        let isLib0 = ("//*-0" ++ asuf) ?== a
        removeFile a
        if isLib0 then build $ target context (Ar Pack stage) []   [a] -- TODO: Scan for dlls
                  else build $ target context (Ar Pack stage) objs [a]

        synopsis <- traverse pkgSynopsis (pkgCabalFile package)
        unless isLib0 . putSuccess $ renderLibrary
            (quote (pkgName package) ++ " (" ++ show stage ++ ", way "
            ++ show way ++ ").") a synopsis

buildPackageGhciLibrary :: Context -> Rules ()
buildPackageGhciLibrary context@Context {..} = priority 2 $ do
    let libPrefix = "//" ++ contextDir context -/- "HS" ++ pkgName package
    libPrefix ++ "*" ++ (waySuffix way <.> "o") %> \obj -> do
        objs <- allObjects context
        need objs
        build $ target context Ld objs [obj]

allObjects :: Context -> Action [FilePath]
allObjects context = (++) <$> nonHsObjects context <*> hsObjects context

nonHsObjects :: Context -> Action [FilePath]
nonHsObjects context = do
    path    <- buildPath context
    cObjs   <- cObjects context
    cmmSrcs <- pkgDataList (CmmSrcs path)
    cmmObjs <- mapM (objectPath context) cmmSrcs
    eObjs   <- extraObjects context
    return $ cObjs ++ cmmObjs ++ eObjs

cObjects :: Context -> Action [FilePath]
cObjects context = do
    path <- buildPath context
    srcs <- pkgDataList (CSrcs path)
    objs <- mapM (objectPath context) srcs
    return $ if way context == threaded
        then objs
        else filter ((`notElem` ["Evac_thr", "Scav_thr"]) . takeBaseName) objs

extraObjects :: Context -> Action [FilePath]
extraObjects context
    | package context == integerGmp = do
        gmpPath <- gmpBuildPath
        need [gmpPath -/- gmpLibraryH]
        map unifyPath <$> getDirectoryFiles "" [gmpPath -/- gmpObjectsDir -/- "*.o"]
    | otherwise         = return []

module Rules.Library (
    buildPackageLibrary, buildPackageGhciLibrary, buildDynamicLib
    ) where

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.PackageData as PD
import Hadrian.Haskell.Cabal.Parse (parseCabalPkgId)

import Base
import Context
import Expression hiding (way, package)
import Flavour
import GHC.Packages
import Oracles.ModuleFiles
import Oracles.Setting
import Rules.Gmp
import Settings
import Target
import Utilities

import qualified System.Directory as IO

archive :: Way -> String -> String
archive way pkgId = "libHS" ++ pkgId ++ (waySuffix way <.> "a")

-- | Building a library consist of building
-- the artifacts, and copying it somewhere
-- with cabal, and finally registering it
-- with the compiler via cabal in the
-- package database.
--
-- So we'll assume rules to build all the
-- package artifacts, and provide rules for
-- the any of the library artifacts.
library :: Context -> Rules ()
library context@Context{..} = do
    root <- buildRootRules
    pkgId <- case pkgCabalFile package of
      Just file -> liftIO $ parseCabalPkgId file
      Nothing   -> return (pkgName package)

    root -/- libDir context -/- pkgId -/- archive way pkgId %> \_ -> do
        need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) [pkgId]
        return ()

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
    root <- buildRootRules
    pkgId <- case pkgCabalFile package of
      Just file -> liftIO $ parseCabalPkgId file
      Nothing   -> return (pkgName package)
    let libPrefix = root -/- buildDir context -/- "libHS" ++ pkgId
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
    root <- buildRootRules
    pkgId <- case pkgCabalFile package of
      Just file -> liftIO (parseCabalPkgId file)
      Nothing   -> return (pkgName package)
    let libPrefix = root -/- buildDir context -/- "libHS" ++ pkgId
        archive = libPrefix ++ (waySuffix way <.> "a")
    archive %%> \a -> do
        objs <- libraryObjects context
        asuf <- libsuf way
        let isLib0 = ("//*-0" ++ asuf) ?== a
        removeFile a
        if isLib0 then build $ target context (Ar Pack stage) []   [a] -- TODO: Scan for dlls
                  else build $ target context (Ar Pack stage) objs [a]

        synopsis <- pkgSynopsis context
        unless isLib0 . putSuccess $ renderLibrary
            (quote (pkgName package) ++ " (" ++ show stage ++ ", way "
            ++ show way ++ ").") a synopsis

    library context

buildPackageGhciLibrary :: Context -> Rules ()
buildPackageGhciLibrary context@Context {..} = priority 2 $ do
    root <- buildRootRules
    pkgId <- case pkgCabalFile package of
      Just file -> liftIO $ parseCabalPkgId file
      Nothing   -> return (pkgName package)

    let libPrefix = root -/- buildDir context -/- "HS" ++ pkgId
        o = libPrefix ++ "*" ++ (waySuffix way <.> "o")
    o %> \obj -> do
        objs <- allObjects context
        need objs
        build $ target context (Ld stage) objs [obj]

allObjects :: Context -> Action [FilePath]
allObjects context = (++) <$> nonHsObjects context <*> hsObjects context

nonHsObjects :: Context -> Action [FilePath]
nonHsObjects context = do
    cObjs   <- cObjects context
    cmmSrcs <- interpretInContext context (getPackageData PD.cmmSrcs)
    cmmObjs <- mapM (objectPath context) cmmSrcs
    eObjs   <- extraObjects context
    return $ cObjs ++ cmmObjs ++ eObjs

cObjects :: Context -> Action [FilePath]
cObjects context = do
    srcs <- interpretInContext context (getPackageData PD.cSrcs)
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

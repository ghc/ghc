{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles.Dependencies (
    fileDependencies, contextDependencies, needContext, dependenciesOracles
    ) where

import qualified Data.HashMap.Strict as Map

import Base
import Context
import Expression
import Oracles.PackageData
import Settings
import Settings.Builders.GhcCabal

newtype ObjDepsKey = ObjDepsKey (FilePath, FilePath)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | 'Action' @fileDependencies context file@ looks up dependencies of a @file@
-- in a generated dependecy file @path/.dependencies@, where @path@ is the build
-- path of the given @context@. The action returns a pair @(source, files)@,
-- such that the @file@ can be produced by compiling @source@, which in turn
-- also depends on a number of other @files@.
fileDependencies :: Context -> FilePath -> Action (FilePath, [FilePath])
fileDependencies context obj = do
    let path = buildPath context -/- ".dependencies"
    -- If no dependencies found, try to drop the way suffix (for *.c sources).
    deps <- firstJustM (askOracle . ObjDepsKey . (,) path) [obj, obj -<.> "o"]
    case deps of
        Nothing -> error $ "No dependencies found for file " ++ obj
        Just [] -> error $ "No source file found for file " ++ obj
        Just (source : files) -> return (source, files)

newtype PkgDepsKey = PkgDepsKey String
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | Given a 'Context' this 'Action' looks up its package dependencies in
-- 'Settings.Paths.packageDependencies' using 'packageDependenciesOracle', and
-- wraps found dependencies in appropriate contexts. The only subtlety here is
-- that we never depend on packages built in 'Stage2' or later, therefore the
-- stage of the resulting dependencies is bounded from above at 'Stage1'. To
-- compute package dependencies we scan package cabal files, see "Rules.Cabal".
contextDependencies :: Context -> Action [Context]
contextDependencies context@Context {..} = do
    maybeDeps <- askOracle . PkgDepsKey $ pkgNameString package
    deps <- case maybeDeps of
        Nothing -> error $ "Context dependencies not found for " ++ show context
        Just ds -> return $ map PackageName ds
    let pkgContext = \pkg -> Context (min stage Stage1) pkg way
    pkgs <- interpretInContext (pkgContext package) getPackages
    return . map pkgContext $ matchPackageNames (sort pkgs) deps

-- | Given a sorted list of packages and a sorted list of package names, returns
-- packages whose names appear in the list of names.
matchPackageNames :: [Package] -> [PackageName] -> [Package]
matchPackageNames = intersectOrd (\pkg name -> compare (pkgName pkg) name)

-- | Coarse-grain 'need': make sure given contexts are fully built.
needContext :: [Context] -> Action ()
needContext cs = do
    libs <- fmap concat . forM cs $ \context -> do
        libFile  <- pkgLibraryFile     context
        lib0File <- pkgLibraryFile0    context
        lib0     <- buildDll0          context
        ghciLib  <- pkgGhciLibraryFile context
        ghciFlag <- interpretInContext context $ getPkgData BuildGhciLib
        let ghci = ghciFlag == "YES" && stage context == Stage1
        return $ [ libFile ] ++ [ lib0File | lib0 ] ++ [ ghciLib | ghci ]
    confs <- mapM pkgConfFile cs
    need $ libs ++ confs

-- | Oracles for the package dependencies and 'path/dist/.dependencies' files.
dependenciesOracles :: Rules ()
dependenciesOracles = do
    deps <- newCache $ \file -> do
        putLoud $ "Reading dependencies from " ++ file ++ "..."
        contents <- map words <$> readFileLines file
        return . Map.fromList $ map (\(x:xs) -> (x, xs)) contents
    void $ addOracle $ \(ObjDepsKey (file, obj)) -> Map.lookup obj <$> deps file

    pkgDeps <- newCache $ \_ -> do
        putLoud $ "Reading package dependencies..."
        contents <- readFileLines packageDependencies
        return $ Map.fromList [ (p, ps) | s <- contents, let p:ps = words s ]
    void $ addOracle $ \(PkgDepsKey pkg) -> Map.lookup pkg <$> pkgDeps ()

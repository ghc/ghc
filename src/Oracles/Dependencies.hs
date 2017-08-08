{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Oracles.Dependencies (
    fileDependencies, contextDependencies, libraryTargets, needLibrary,
    dependenciesOracles, pkgDependencies, topsortPackages
    ) where

import qualified Data.HashMap.Strict as Map
import Hadrian.Utilities

import Base
import Context
import Expression hiding (stage)
import Oracles.PackageData
import Settings
import Settings.Builders.GhcCabal
import Settings.Path

newtype Dependency = Dependency (FilePath, FilePath)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | 'Action' @fileDependencies context file@ looks up dependencies of a @file@
-- in a generated dependency file @path/.dependencies@, where @path@ is the build
-- path of the given @context@. The action returns a pair @(source, files)@,
-- such that the @file@ can be produced by compiling @source@, which in turn
-- also depends on a number of other @files@.
fileDependencies :: Context -> FilePath -> Action (FilePath, [FilePath])
fileDependencies context obj = do
    let path = buildPath context -/- ".dependencies"
    deps <- askOracle $ Dependency (path, obj)
    case deps of
        Nothing -> error $ "No dependencies found for file " ++ obj
        Just [] -> error $ "No source file found for file " ++ obj
        Just (source : files) -> return (source, files)

-- | Given a 'Context' this 'Action' looks up its package dependencies in
-- 'Settings.Paths.packageDependencies' using 'packageDependenciesOracle', and
-- wraps found dependencies in appropriate contexts. The only subtlety here is
-- that we never depend on packages built in 'Stage2' or later, therefore the
-- stage of the resulting dependencies is bounded from above at 'Stage1'. To
-- compute package dependencies we scan package cabal files, see "Rules.Cabal".
contextDependencies :: Context -> Action [Context]
contextDependencies context@Context {..} = do
    let pkgContext = \pkg -> Context (min stage Stage1) pkg way
        unpack     = fromMaybe . error $ "No dependencies for " ++ show context
    deps <- unpack <$> askOracle (Dependency (packageDependencies, pkgNameString package))
    pkgs <- sort <$> interpretInContext (pkgContext package) getPackages
    return . map pkgContext $ intersectOrd (compare . pkgNameString) pkgs deps

-- | Given a `Package`, this `Action` looks up its package dependencies
-- 'Settings.Paths.packageDependencies' using 'packageDependenciesOracle'
-- The context will be the vanilla context with stage equal to 1
pkgDependencies :: Package -> Action [Package]
pkgDependencies = fmap (map Context.package) . contextDependencies . vanillaContext Stage1

-- | Given a library 'Package' this action computes all of its targets.
libraryTargets :: Context -> Action [FilePath]
libraryTargets context = do
    confFile <- pkgConfFile        context
    libFile  <- pkgLibraryFile     context
    lib0File <- pkgLibraryFile0    context
    lib0     <- buildDll0          context
    ghciLib  <- pkgGhciLibraryFile context
    ghciFlag <- interpretInContext context $ getPkgData BuildGhciLib
    let ghci = ghciFlag == "YES" && (stage context == Stage1 || stage1Only)
    return $ [ confFile, libFile ] ++ [ lib0File | lib0 ] ++ [ ghciLib | ghci ]

-- | Coarse-grain 'need': make sure all given libraries are fully built.
needLibrary :: [Context] -> Action ()
needLibrary cs = need =<< concatMapM libraryTargets cs

-- | Oracles for the package dependencies and 'path/dist/.dependencies' files.
dependenciesOracles :: Rules ()
dependenciesOracles = do
    deps <- newCache $ \file -> do
        putLoud $ "Reading dependencies from " ++ file ++ "..."
        contents <- map words <$> readFileLines file
        return $ Map.fromList [ (key, values) | (key:values) <- contents ]
    void $ addOracle $ \(Dependency (file, key)) -> Map.lookup key <$> deps file

-- | Topological sort of packages according to their dependencies.
-- HACK (izgzhen): See https://github.com/snowleopard/hadrian/issues/344 for details
topsortPackages :: [Package] -> Action [Package]
topsortPackages pkgs = do
    elems <- mapM (\p -> (p,) <$> pkgDependencies p) pkgs
    return $ map fst $ topSort elems
  where
    annotateInDeg es e =
     (foldr (\e' s -> if fst e' `elem` snd e then s + 1 else s) (0 :: Int) es, e)
    topSort [] = []
    topSort es =
      let annotated = map (annotateInDeg es) es
          inDegZero = map snd $ filter ((== 0). fst) annotated
      in  inDegZero ++ topSort (es \\ inDegZero)

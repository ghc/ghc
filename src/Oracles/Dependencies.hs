{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Oracles.Dependencies (
    fileDependencies, contextDependencies, needContext, dependenciesOracles,
    pkgDependencies, sortPkgsByDep
    ) where

import qualified Data.HashMap.Strict as Map

import Base
import Context
import Expression
import Oracles.PackageData
import Settings
import Settings.Builders.GhcCabal
import Settings.Path

newtype ObjDepsKey = ObjDepsKey (FilePath, FilePath)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | 'Action' @fileDependencies context file@ looks up dependencies of a @file@
-- in a generated dependency file @path/.dependencies@, where @path@ is the build
-- path of the given @context@. The action returns a pair @(source, files)@,
-- such that the @file@ can be produced by compiling @source@, which in turn
-- also depends on a number of other @files@.
fileDependencies :: Context -> FilePath -> Action (FilePath, [FilePath])
fileDependencies context obj = do
    let path = buildPath context -/- ".dependencies"
    deps <- askOracle $ ObjDepsKey (path, obj)
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
    let pkgContext = \pkg -> Context (min stage Stage1) pkg way
        unpack     = fromMaybe . error $ "No dependencies for " ++ show context
    deps <- unpack <$> askOracle (PkgDepsKey $ pkgNameString package)
    pkgs <- sort <$> interpretInContext (pkgContext package) getPackages
    return . map pkgContext $ intersectOrd (compare . pkgNameString) pkgs deps

-- | Given a `Package`, this `Action` looks up its package dependencies
-- 'Settings.Paths.packageDependencies' using 'packageDependenciesOracle'
-- The context will be the vanilla context with stage equal to 1
pkgDependencies :: Package -> Action [Package]
pkgDependencies = fmap (map Context.package) . contextDependencies . vanillaContext Stage1

-- | Coarse-grain 'need': make sure given contexts are fully built.
needContext :: [Context] -> Action ()
needContext cs = do
    libs <- concatForM cs $ \context -> do
        libFile  <- pkgLibraryFile     context
        lib0File <- pkgLibraryFile0    context
        lib0     <- buildDll0          context
        ghciLib  <- pkgGhciLibraryFile context
        ghciFlag <- interpretInContext context $ getPkgData BuildGhciLib
        let ghci = ghciFlag == "YES" && (stage context == Stage1 || stage1Only)
        return $ [ libFile ] ++ [ lib0File | lib0 ] ++ [ ghciLib | ghci ]
    confs <- mapM pkgConfFile cs
    need $ libs ++ confs

-- | Oracles for the package dependencies and 'path/dist/.dependencies' files.
dependenciesOracles :: Rules ()
dependenciesOracles = do
    deps <- newCache readDependencies
    void $ addOracle $ \(ObjDepsKey (file, obj)) -> Map.lookup obj <$> deps file

    pkgDeps <- newCache $ \_ -> readDependencies packageDependencies
    void $ addOracle $ \(PkgDepsKey pkg) -> Map.lookup pkg <$> pkgDeps ()
  where
    readDependencies file = do
        putLoud $ "Reading dependencies from " ++ file ++ "..."
        contents <- map words <$> readFileLines file
        return $ Map.fromList [ (key, values) | (key:values) <- contents ]

-- | Sort packages by their dependency
-- HACK (izgzhen): See https://github.com/snowleopard/hadrian/issues/344 for details
sortPkgsByDep :: [Package] -> Action [Package]
sortPkgsByDep pkgs = do
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

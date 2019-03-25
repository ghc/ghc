module Utilities (
    build, buildWithResources, buildWithCmdOptions,
    askWithResources,
    runBuilder, runBuilderWith,
    needLibrary, needBinDistLibrary, contextDependencies, stage1Dependencies,
    libraryTargets, topsortPackages, cabalDependencies
    ) where

import qualified Hadrian.Builder as H
import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type
import Hadrian.Utilities
import System.Directory (getCurrentDirectory)
import Development.Shake.FilePath (makeRelativeEx)
import Control.Monad.Extra

import Context
import Expression hiding (stage)
import Settings
import Target

build :: Target -> Action ()
build target = H.build target getArgs

buildWithResources :: [(Resource, Int)] -> Target -> Action ()
buildWithResources rs target = H.buildWithResources rs target getArgs

buildWithCmdOptions :: [CmdOption] -> Target -> Action ()
buildWithCmdOptions opts target = H.buildWithCmdOptions opts target getArgs

askWithResources :: [(Resource, Int)] -> Target -> Action String
askWithResources rs target = H.askWithResources rs target getArgs

-- TODO: Cache the computation.
-- | Given a 'Context' this 'Action' looks up the package dependencies and wraps
-- the results in appropriate contexts.
-- To compute package dependencies we transitively scan Cabal files using
-- 'pkgDependencies' defined in "Hadrian.Haskell.Cabal".
contextDependencies :: Context -> Action [Context]
contextDependencies Context {..} = do
    depPkgs <- go [package]
    return [ Context stage pkg way | pkg <- depPkgs, pkg /= package ]
  where
    go pkgs  = do
        deps <- concatMapM step pkgs
        let newPkgs = nubOrd $ sort (deps ++ pkgs)
        if pkgs == newPkgs then return pkgs else go newPkgs
    step pkg = do
        deps   <- pkgDependencies pkg
        active <- sort <$> stagePackages stage
        return $ intersectOrd (compare . pkgName) active deps

cabalDependencies :: Context -> Action [String]
cabalDependencies ctx = interpretInContext ctx $ getContextData depIds

-- | Lookup dependencies of a 'Package' in the @vanilla Stage1 context@.
stage1Dependencies :: Package -> Action [Package]
stage1Dependencies =
    fmap (map Context.package) . contextDependencies . vanillaContext Stage1

-- | Given a library 'Package' this action computes all of its targets. See
-- 'packageTargets' for the explanation of the @includeGhciLib@ parameter.
libraryTargets :: Bool -> Context -> Action [FilePath]
libraryTargets includeGhciLib context@Context {..} = do
    libFile  <- pkgLibraryFile     context
    ghciLib  <- pkgGhciLibraryFile context
    ghci     <- if includeGhciLib && not (wayUnit Dynamic way)
                then interpretInContext context $ getContextData buildGhciLib
                else return False
    return $ [ libFile ] ++ [ ghciLib | ghci ]

-- | Coarse-grain 'need': make sure all given libraries are fully built.
needLibrary :: [Context] -> Action ()
needLibrary cs = need =<< concatMapM (libraryTargets True) cs

-- | Coarse-grain 'need': make sure all given libraries are fully built and
-- registered/copied to the package database.
needBinDistLibrary :: [(Resource, Int)] -> [Context] -> Action ()
needBinDistLibrary rs cs = do
    hiDirs <- concatMapM (pkgImportDirs rs) cs
    hiFiles <- nub <$> concatMapM
                        (\ hiDir -> do
                            hiFilesRel <- getDirectoryFiles hiDir
                                    ["//*." ++ hisuf (Context.way c) | c <- cs]
                            return $ fmap (hiDir -/-) hiFilesRel
                            )
                        hiDirs
    need hiFiles

-- | Path to package database include paths (contain the packages *.hi files).
pkgImportDirs :: [(Resource, Int)] -> Context -> Action [FilePath]
pkgImportDirs rs context@Context{..} = do
    -- Needing the config file's ensures the libraries are registered.
    confFile <- pkgConfFile context
    -- need [confFile]
    liftIO $ print confFile
    stdOut <- askWithResources rs $
                target context (GhcPkg ImportDirs stage) [pkgName package] []
    let
        -- Extract the path from stdout.
        rawPaths = concatMap words $ lines stdOut
    -- Ensure the path is relative. (That may not be possible of on a
    -- diferent drive).
    cwd <- liftIO $ getCurrentDirectory
    paths <- liftIO $ mapMaybeM (makeRelativeEx cwd) rawPaths
    return paths

-- HACK (izgzhen), see https://github.com/snowleopard/hadrian/issues/344.
-- | Topological sort of packages according to their dependencies.
topsortPackages :: [Package] -> Action [Package]
topsortPackages pkgs = do
    elems <- mapM (\p -> (p,) <$> stage1Dependencies p) pkgs
    return $ map fst $ topSort elems
  where
    annotateInDeg es e =
     (foldr (\e' s -> if fst e' `elem` snd e then s + 1 else s) (0 :: Int) es, e)
    topSort [] = []
    topSort es =
      let annotated = map (annotateInDeg es) es
          inDegZero = map snd $ filter ((== 0). fst) annotated
      in  inDegZero ++ topSort (es \\ inDegZero)
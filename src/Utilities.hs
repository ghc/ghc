module Utilities (
    build, buildWithResources, buildWithCmdOptions, runBuilder, runBuilderWith,
    builderEnvironment, needLibrary, applyPatch, installDirectory, installData,
    installScript, installProgram, linkSymbolic, contextDependencies,
    stage1Dependencies, libraryTargets, topsortPackages
    ) where

import qualified System.Directory.Extra as IO

import qualified Hadrian.Builder as H
import Hadrian.Haskell.Cabal
import Hadrian.Oracles.Path
import Hadrian.Utilities

import Context
import Expression hiding (stage)
import Oracles.Setting
import Oracles.PackageData
import Settings
import Target
import UserSettings

build :: Target -> Action ()
build target = H.build target getArgs

buildWithResources :: [(Resource, Int)] -> Target -> Action ()
buildWithResources rs target = H.buildWithResources rs target getArgs

buildWithCmdOptions :: [CmdOption] -> Target -> Action ()
buildWithCmdOptions opts target = H.buildWithCmdOptions opts target getArgs

-- | Apply a patch by executing the 'Patch' builder in a given directory.
applyPatch :: FilePath -> FilePath -> Action ()
applyPatch dir patch = do
    let file = dir -/- patch
    needBuilder Patch
    path <- builderPath Patch
    putBuild $ "| Apply patch " ++ file
    quietly $ cmd Shell [Cwd dir] [path, "-p0 <", patch]

-- | Install a directory.
installDirectory :: FilePath -> Action ()
installDirectory dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallDir
    putBuild $ "| Install directory " ++ dir
    quietly $ cmd path dir

-- | Install data files to a directory and track them.
installData :: [FilePath] -> FilePath -> Action ()
installData fs dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallData
    need fs
    forM_ fs $ \f -> putBuild $ "| Install data " ++ f ++ " to " ++ dir
    quietly $ cmd path fs dir

-- | Install an executable file to a directory and track it.
installProgram :: FilePath -> FilePath -> Action ()
installProgram f dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallProgram
    need [f]
    putBuild $ "| Install program " ++ f ++ " to " ++ dir
    quietly $ cmd path f dir

-- | Install an executable script to a directory and track it.
installScript :: FilePath -> FilePath -> Action ()
installScript f dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallScript
    need [f]
    putBuild $ "| Install script " ++ f ++ " to " ++ dir
    quietly $ cmd path f dir

-- | Create a symbolic link from source file to target file (when symbolic links
-- are supported) and track the source file.
linkSymbolic :: FilePath -> FilePath -> Action ()
linkSymbolic source target = do
    lns <- setting LnS
    unless (null lns) $ do
        need [source] -- Guarantee source is built before printing progress info.
        let dir = takeDirectory target
        liftIO $ IO.createDirectoryIfMissing True dir
        putProgressInfo =<< renderAction "Create symbolic link" source target
        quietly $ cmd lns source target

-- | Write a Builder's path into a given environment variable.
builderEnvironment :: String -> Builder -> Action CmdOption
builderEnvironment variable builder = do
    needBuilder builder
    path <- builderPath builder
    return $ AddEnv variable path

-- | Given a 'Context' this 'Action' looks up its package dependencies and wraps
-- the results in appropriate contexts. The only subtlety here is that we never
-- depend on packages built in 'Stage2' or later, therefore the stage of the
-- resulting dependencies is bounded from above at 'Stage1'. To compute package
-- dependencies we scan package @.cabal@ files, see 'pkgDependencies' defined
-- in "Hadrian.Haskell.Cabal".
contextDependencies :: Context -> Action [Context]
contextDependencies Context {..} = case pkgCabalFile package of
    Nothing        -> return [] -- Non-Cabal packages have no dependencies.
    Just cabalFile -> do
        let depStage   = min stage Stage1
            depContext = \pkg -> Context depStage pkg way
        deps <- pkgDependencies cabalFile
        pkgs <- sort <$> stagePackages depStage
        return . map depContext $ intersectOrd (compare . pkgName) pkgs deps

-- | Lookup dependencies of a 'Package' in the vanilla Stage1 context.
stage1Dependencies :: Package -> Action [Package]
stage1Dependencies =
    fmap (map Context.package) . contextDependencies . vanillaContext Stage1

-- | Given a library 'Package' this action computes all of its targets. See
-- 'packageTargets' for the explanation of the @includeGhciLib@ parameter.
libraryTargets :: Bool -> Context -> Action [FilePath]
libraryTargets includeGhciLib context = do
    confFile <- pkgConfFile        context
    libFile  <- pkgLibraryFile     context
    lib0File <- pkgLibraryFile0    context
    lib0     <- buildDll0          context
    ghciLib  <- pkgGhciLibraryFile context
    ghciFlag <- if includeGhciLib
                then interpretInContext context $ getPkgData BuildGhciLib
                else return "NO"
    let ghci = ghciFlag == "YES" && (stage context == Stage1 || stage1Only)
    return $ [ confFile, libFile ] ++ [ lib0File | lib0 ] ++ [ ghciLib | ghci ]

-- | Coarse-grain 'need': make sure all given libraries are fully built.
needLibrary :: [Context] -> Action ()
needLibrary cs = need =<< concatMapM (libraryTargets True) cs

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

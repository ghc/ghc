module Utilities (
    build, buildWithCmdOptions, buildWithResources, applyPatch, runBuilder,
    runBuilderWith, builderEnvironment, needBuilder, needLibrary,
    installDirectory, installData, installScript, installProgram, linkSymbolic,
    contextDependencies, pkgDependencies, libraryTargets, topsortPackages
    ) where

import qualified System.Directory.Extra as IO

import Hadrian.Oracles.ArgsHash
import Hadrian.Oracles.KeyValue
import Hadrian.Oracles.Path
import Hadrian.Utilities

import CommandLine
import Context
import Expression hiding (builder, inputs, outputs, way, stage, package)
import GHC
import Oracles.Setting
import Oracles.PackageData
import Settings
import Settings.Path
import Settings.Builders.Ar
import Target
import UserSettings

-- | Build a 'Target' with the right 'Builder' and command line arguments.
-- Force a rebuild if the argument list has changed since the last build.
build :: Target -> Action ()
build = customBuild [] []

-- | Build a 'Target' with the right 'Builder' and command line arguments,
-- acquiring necessary resources. Force a rebuild if the argument list has
-- changed since the last build.
buildWithResources :: [(Resource, Int)] -> Target -> Action ()
buildWithResources rs = customBuild rs []

-- | Build a 'Target' with the right 'Builder' and command line arguments,
-- using given options when executing the build command. Force a rebuild if
-- the argument list has changed since the last build.
buildWithCmdOptions :: [CmdOption] -> Target -> Action ()
buildWithCmdOptions = customBuild []

customBuild :: [(Resource, Int)] -> [CmdOption] -> Target -> Action ()
customBuild rs opts target = do
    let targetBuilder = builder target
    needBuilder targetBuilder
    path    <- builderPath targetBuilder
    argList <- interpret target getArgs
    verbose <- interpret target verboseCommands
    let quietlyUnlessVerbose = if verbose then withVerbosity Loud else quietly
    trackArgsHash target -- Rerun the rule if the hash of argList has changed.
    withResources rs $ do
        putInfo target
        quietlyUnlessVerbose $ case targetBuilder of
            Ar _ -> do
                output <- interpret target getOutput
                if "//*.a" ?== output
                then arCmd path argList
                else do
                    input <- interpret target getInput
                    top   <- topDirectory
                    echo  <- cmdEcho
                    cmd echo [Cwd output] [path] "x" (top -/- input)

            Configure dir -> do
                -- Inject /bin/bash into `libtool`, instead of /bin/sh, otherwise Windows breaks.
                -- TODO: Figure out why.
                bash <- bashPath
                echo <- cmdEcho
                let env = AddEnv "CONFIG_SHELL" bash
                cmd Shell echo env [Cwd dir] [path] opts argList

            HsCpp    -> captureStdout target path argList
            GenApply -> captureStdout target path argList

            GenPrimopCode -> do
                src  <- interpret target getInput
                file <- interpret target getOutput
                input <- readFile' src
                Stdout output <- cmd (Stdin input) [path] argList
                writeFileChanged file output

            Make dir -> do
                echo <- cmdEcho
                cmd Shell echo path ["-C", dir] argList

            _  -> do
                echo <- cmdEcho
                cmd echo [path] argList

-- | Suppress build output depending on the @--progress-info@ flag.
cmdEcho :: Action CmdOption
cmdEcho = do
    progressInfo <- cmdProgressInfo
    return $ EchoStdout (progressInfo `elem` [Normal, Unicorn])

-- | Run a builder, capture the standard output, and write it to a given file.
captureStdout :: Target -> FilePath -> [String] -> Action ()
captureStdout target path argList = do
    file <- interpret target getOutput
    Stdout output <- cmd [path] argList
    writeFileChanged file output

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

isInternal :: Builder -> Bool
isInternal = isJust . builderProvenance

-- | Make sure a 'Builder' exists and rebuild it if out of date.
needBuilder :: Builder -> Action ()
needBuilder (Configure dir) = need [dir -/- "configure"]
needBuilder (Make      dir) = need [dir -/- "Makefile"]
needBuilder builder         = when (isInternal builder) $ do
    path <- builderPath builder
    need [path]

-- | Write a Builder's path into a given environment variable.
builderEnvironment :: String -> Builder -> Action CmdOption
builderEnvironment variable builder = do
    needBuilder builder
    path <- builderPath builder
    return $ AddEnv variable path

runBuilder :: Builder -> [String] -> Action ()
runBuilder = runBuilderWith []

-- | Run a builder with given list of arguments using custom 'cmd' options.
runBuilderWith :: [CmdOption] -> Builder -> [String] -> Action ()
runBuilderWith options builder args = do
    needBuilder builder
    path <- builderPath builder
    let note = if null args then "" else " (" ++ intercalate ", " args ++ ")"
    putBuild $ "| Run " ++ show builder ++ note
    quietly $ cmd options [path] args

-- | Given a 'Context' this 'Action' looks up its package dependencies in
-- 'Settings.Paths.packageDependencies' and wraps the results in appropriate
-- contexts. The only subtlety here is that we never depend on packages built in
-- 'Stage2' or later, therefore the stage of the resulting dependencies is
-- bounded from above at 'Stage1'. To compute package dependencies we scan
-- package cabal files, see "Rules.Cabal".
contextDependencies :: Context -> Action [Context]
contextDependencies Context {..} = do
    let pkgContext = \pkg -> Context (min stage Stage1) pkg way
    deps <- lookupValuesOrError packageDependencies (pkgNameString package)
    pkgs <- sort <$> interpretInContext (pkgContext package) getPackages
    return . map pkgContext $ intersectOrd (compare . pkgNameString) pkgs deps

-- | Lookup dependencies of a 'Package' in the vanilla Stage1 context.
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

-- HACK (izgzhen), see https://github.com/snowleopard/hadrian/issues/344.
-- | Topological sort of packages according to their dependencies.
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

-- | Print out information about the command being executed.
putInfo :: Target -> Action ()
putInfo t = putProgressInfo =<< renderAction
    ("Run " ++ show (builder t) ++ contextInfo)
    (digest $ inputs  t)
    (digest $ outputs t)
  where
    contextInfo = concat $ [ " (" ]
        ++ [ "stage = "     ++ show (stage $ context t) ]
        ++ [ ", package = " ++ pkgNameString (package $ context t) ]
        ++ [ ", way = "     ++ show (way $ context t) | (way $ context t) /= vanilla ]
        ++ [ ")" ]
    digest [] = "none"
    digest [x] = x
    digest (x:xs) = x ++ " (and " ++ show (length xs) ++ " more)"

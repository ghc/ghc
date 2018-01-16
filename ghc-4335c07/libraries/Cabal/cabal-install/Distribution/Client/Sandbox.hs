{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- UI for the sandboxing functionality.
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox (
    sandboxInit,
    sandboxDelete,
    sandboxAddSource,
    sandboxAddSourceSnapshot,
    sandboxDeleteSource,
    sandboxListSources,
    sandboxHcPkg,
    dumpPackageEnvironment,
    withSandboxBinDirOnSearchPath,

    getSandboxConfigFilePath,
    loadConfigOrSandboxConfig,
    findSavedDistPref,
    initPackageDBIfNeeded,
    maybeWithSandboxDirOnSearchPath,

    WereDepsReinstalled(..),
    reinstallAddSourceDeps,
    maybeReinstallAddSourceDeps,

    SandboxPackageInfo(..),
    maybeWithSandboxPackageInfo,

    tryGetIndexFilePath,
    sandboxBuildDir,
    getInstalledPackagesInSandbox,
    updateSandboxConfigFileFlag,
    updateInstallDirs,

    getPersistOrConfigCompiler
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.Setup
  ( SandboxFlags(..), ConfigFlags(..), ConfigExFlags(..), InstallFlags(..)
  , GlobalFlags(..), configCompilerAux', configPackageDB'
  , defaultConfigExFlags, defaultInstallFlags
  , defaultSandboxLocation, withRepoContext )
import Distribution.Client.Sandbox.Timestamp  ( listModifiedDeps
                                              , maybeAddCompilerTimestampRecord
                                              , withAddTimestamps
                                              , removeTimestamps )
import Distribution.Client.Config
  ( SavedConfig(..), defaultUserInstall, loadConfig )
import Distribution.Client.Dependency         ( foldProgress )
import Distribution.Client.IndexUtils         ( BuildTreeRefType(..) )
import Distribution.Client.Install            ( InstallArgs,
                                                makeInstallContext,
                                                makeInstallPlan,
                                                processInstallPlan )
import Distribution.Utils.NubList            ( fromNubList )

import Distribution.Client.Sandbox.PackageEnvironment
  ( PackageEnvironment(..), PackageEnvironmentType(..)
  , createPackageEnvironmentFile, classifyPackageEnvironment
  , tryLoadSandboxPackageEnvironmentFile, loadUserConfig
  , commentPackageEnvironment, showPackageEnvironmentWithComments
  , sandboxPackageEnvironmentFile, userPackageEnvironmentFile
  , sandboxPackageDBPath )
import Distribution.Client.Sandbox.Types      ( SandboxPackageInfo(..)
                                              , UseSandbox(..) )
import Distribution.Client.SetupWrapper
  ( SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Types              ( PackageLocation(..) )
import Distribution.Client.Utils              ( inDir, tryCanonicalizePath
                                              , tryFindAddSourcePackageDesc)
import Distribution.PackageDescription.Configuration
                                              ( flattenPackageDescription )
import Distribution.PackageDescription.Parsec ( readGenericPackageDescription )
import Distribution.Simple.Compiler           ( Compiler(..), PackageDB(..) )
import Distribution.Simple.Configure          ( configCompilerAuxEx
                                              , getPackageDBContents
                                              , maybeGetPersistBuildConfig
                                              , findDistPrefOrDefault
                                              , findDistPref )
import qualified Distribution.Simple.LocalBuildInfo as LocalBuildInfo
import Distribution.Simple.PreProcess         ( knownSuffixHandlers )
import Distribution.Simple.Program            ( ProgramDb )
import Distribution.Simple.Setup              ( Flag(..), HaddockFlags(..)
                                              , fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.SrcDist            ( prepareTree )
import Distribution.Simple.Utils              ( die', debug, notice, info, warn
                                              , debugNoWrap, defaultPackageDesc
                                              , topHandlerWith
                                              , createDirectoryIfMissingVerbose )
import Distribution.Package                   ( Package(..) )
import Distribution.System                    ( Platform )
import Distribution.Text                      ( display )
import Distribution.Verbosity                 ( Verbosity )
import Distribution.Compat.Environment        ( lookupEnv, setEnv )
import Distribution.Client.Compat.FilePerms   ( setFileHidden )
import qualified Distribution.Client.Sandbox.Index as Index
import Distribution.Simple.PackageIndex       ( InstalledPackageIndex )
import qualified Distribution.Simple.PackageIndex  as InstalledPackageIndex
import qualified Distribution.Simple.Register      as Register

import Distribution.Solver.Types.SourcePackage

import qualified Data.Map                          as M
import qualified Data.Set                          as S
import Data.Either                            (partitionEithers)
import Control.Exception                      ( assert, bracket_ )
import Control.Monad                          ( forM, mapM, mapM_ )
import Data.Bits                              ( shiftL, shiftR, xor )
import Data.IORef                             ( newIORef, writeIORef, readIORef )
import Data.List                              ( delete
                                              , groupBy )
import Data.Maybe                             ( fromJust )
import Numeric                                ( showHex )
import System.Directory                       ( canonicalizePath
                                              , createDirectory
                                              , doesDirectoryExist
                                              , doesFileExist
                                              , getCurrentDirectory
                                              , removeDirectoryRecursive
                                              , removeFile
                                              , renameDirectory )
import System.FilePath                        ( (</>), equalFilePath
                                              , getSearchPath
                                              , searchPathSeparator
                                              , splitSearchPath
                                              , takeDirectory )

--
-- * Constants
--

-- | The name of the sandbox subdirectory where we keep snapshots of add-source
-- dependencies.
snapshotDirectoryName :: FilePath
snapshotDirectoryName = "snapshots"

-- | Non-standard build dir that is used for building add-source deps instead of
-- "dist". Fixes surprising behaviour in some cases (see issue #1281).
sandboxBuildDir :: FilePath -> FilePath
sandboxBuildDir sandboxDir = "dist/dist-sandbox-" ++ showHex sandboxDirHash ""
  where
    sandboxDirHash = jenkins sandboxDir

    -- See http://en.wikipedia.org/wiki/Jenkins_hash_function
    jenkins :: String -> Word32
    jenkins str = loop_finish $ foldl' loop 0 str
      where
        loop :: Word32 -> Char -> Word32
        loop hash key_i' = hash'''
          where
            key_i   = toEnum . ord $ key_i'
            hash'   = hash + key_i
            hash''  = hash' + (shiftL hash' 10)
            hash''' = hash'' `xor` (shiftR hash'' 6)

        loop_finish :: Word32 -> Word32
        loop_finish hash = hash'''
          where
            hash'   = hash + (shiftL hash 3)
            hash''  = hash' `xor` (shiftR hash' 11)
            hash''' = hash'' + (shiftL hash'' 15)

--
-- * Basic sandbox functions.
--

-- | If @--sandbox-config-file@ wasn't given on the command-line, set it to the
-- value of the @CABAL_SANDBOX_CONFIG@ environment variable, or else to
-- 'NoFlag'.
updateSandboxConfigFileFlag :: GlobalFlags -> IO GlobalFlags
updateSandboxConfigFileFlag globalFlags =
  case globalSandboxConfigFile globalFlags of
    Flag _ -> return globalFlags
    NoFlag -> do
      f' <- fmap (maybe NoFlag Flag) . lookupEnv $ "CABAL_SANDBOX_CONFIG"
      return globalFlags { globalSandboxConfigFile = f' }

-- | Return the path to the sandbox config file - either the default or the one
-- specified with @--sandbox-config-file@.
getSandboxConfigFilePath :: GlobalFlags -> IO FilePath
getSandboxConfigFilePath globalFlags = do
  let sandboxConfigFileFlag = globalSandboxConfigFile globalFlags
  case sandboxConfigFileFlag of
    NoFlag -> do pkgEnvDir <- getCurrentDirectory
                 return (pkgEnvDir </> sandboxPackageEnvironmentFile)
    Flag path -> return path

-- | Load the @cabal.sandbox.config@ file (and possibly the optional
-- @cabal.config@). In addition to a @PackageEnvironment@, also return a
-- canonical path to the sandbox. Exit with error if the sandbox directory or
-- the package environment file do not exist.
tryLoadSandboxConfig :: Verbosity -> GlobalFlags
                        -> IO (FilePath, PackageEnvironment)
tryLoadSandboxConfig verbosity globalFlags = do
  path <- getSandboxConfigFilePath globalFlags
  tryLoadSandboxPackageEnvironmentFile verbosity path
    (globalConfigFile globalFlags)

-- | Return the name of the package index file for this package environment.
tryGetIndexFilePath :: Verbosity -> SavedConfig -> IO FilePath
tryGetIndexFilePath verbosity config = tryGetIndexFilePath' verbosity (savedGlobalFlags config)

-- | The same as 'tryGetIndexFilePath', but takes 'GlobalFlags' instead of
-- 'SavedConfig'.
tryGetIndexFilePath' :: Verbosity -> GlobalFlags -> IO FilePath
tryGetIndexFilePath' verbosity globalFlags = do
  let paths = fromNubList $ globalLocalRepos globalFlags
  case paths of
    []  -> die' verbosity $ "Distribution.Client.Sandbox.tryGetIndexFilePath: " ++
           "no local repos found. " ++ checkConfiguration
    _   -> return $ (last paths) </> Index.defaultIndexFileName
  where
    checkConfiguration = "Please check your configuration ('"
                         ++ userPackageEnvironmentFile ++ "')."

-- | Try to extract a 'PackageDB' from 'ConfigFlags'. Gives a better error
-- message than just pattern-matching.
getSandboxPackageDB :: Verbosity -> ConfigFlags -> IO PackageDB
getSandboxPackageDB verbosity configFlags = do
  case configPackageDBs configFlags of
    [Just sandboxDB@(SpecificPackageDB _)] -> return sandboxDB
    -- TODO: should we allow multiple package DBs (e.g. with 'inherit')?

    []                                     ->
      die' verbosity $ "Sandbox package DB is not specified. " ++ sandboxConfigCorrupt
    [_]                                    ->
      die' verbosity $ "Unexpected contents of the 'package-db' field. "
            ++ sandboxConfigCorrupt
    _                                      ->
      die' verbosity $ "Too many package DBs provided. " ++ sandboxConfigCorrupt

  where
    sandboxConfigCorrupt = "Your 'cabal.sandbox.config' is probably corrupt."


-- | Which packages are installed in the sandbox package DB?
getInstalledPackagesInSandbox :: Verbosity -> ConfigFlags
                                 -> Compiler -> ProgramDb
                                 -> IO InstalledPackageIndex
getInstalledPackagesInSandbox verbosity configFlags comp progdb = do
    sandboxDB <- getSandboxPackageDB verbosity configFlags
    getPackageDBContents verbosity comp sandboxDB progdb

-- | Temporarily add $SANDBOX_DIR/bin to $PATH.
withSandboxBinDirOnSearchPath :: FilePath -> IO a -> IO a
withSandboxBinDirOnSearchPath sandboxDir = bracket_ addBinDir rmBinDir
  where
    -- TODO: Instead of modifying the global process state, it'd be better to
    -- set the environment individually for each subprocess invocation. This
    -- will have to wait until the Shell monad is implemented; without it the
    -- required changes are too intrusive.
    addBinDir :: IO ()
    addBinDir = do
      mbOldPath <- lookupEnv "PATH"
      let newPath = maybe sandboxBin ((++) sandboxBin . (:) searchPathSeparator)
                    mbOldPath
      setEnv "PATH" newPath

    rmBinDir :: IO ()
    rmBinDir = do
      oldPath <- getSearchPath
      let newPath = intercalate [searchPathSeparator]
                    (delete sandboxBin oldPath)
      setEnv "PATH" newPath

    sandboxBin = sandboxDir </> "bin"

-- | Initialise a package DB for this compiler if it doesn't exist.
initPackageDBIfNeeded :: Verbosity -> ConfigFlags
                         -> Compiler -> ProgramDb
                         -> IO ()
initPackageDBIfNeeded verbosity configFlags comp progdb = do
  SpecificPackageDB dbPath <- getSandboxPackageDB verbosity configFlags
  packageDBExists <- doesDirectoryExist dbPath
  unless packageDBExists $
    Register.initPackageDB verbosity comp progdb dbPath
  when packageDBExists $
    debug verbosity $ "The package database already exists: " ++ dbPath

-- | Entry point for the 'cabal sandbox dump-pkgenv' command.
dumpPackageEnvironment :: Verbosity -> SandboxFlags -> GlobalFlags -> IO ()
dumpPackageEnvironment verbosity _sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity globalFlags
  commentPkgEnv        <- commentPackageEnvironment sandboxDir
  putStrLn . showPackageEnvironmentWithComments (Just commentPkgEnv) $ pkgEnv

-- | Entry point for the 'cabal sandbox init' command.
sandboxInit :: Verbosity -> SandboxFlags  -> GlobalFlags -> IO ()
sandboxInit verbosity sandboxFlags globalFlags = do
  -- Warn if there's a 'cabal-dev' sandbox.
  isCabalDevSandbox <- liftM2 (&&) (doesDirectoryExist "cabal-dev")
                       (doesFileExist $ "cabal-dev" </> "cabal.config")
  when isCabalDevSandbox $
    warn verbosity $
    "You are apparently using a legacy (cabal-dev) sandbox. "
    ++ "Legacy sandboxes may interact badly with native Cabal sandboxes. "
    ++ "You may want to delete the 'cabal-dev' directory to prevent issues."

  -- Create the sandbox directory.
  let sandboxDir' = fromFlagOrDefault defaultSandboxLocation
                    (sandboxLocation sandboxFlags)
  createDirectoryIfMissingVerbose verbosity True sandboxDir'
  sandboxDir <- tryCanonicalizePath sandboxDir'
  setFileHidden sandboxDir

  -- Determine which compiler to use (using the value from ~/.cabal/config).
  userConfig <- loadConfig verbosity (globalConfigFile globalFlags)
  (comp, platform, progdb) <- configCompilerAuxEx (savedConfigureFlags userConfig)

  -- Create the package environment file.
  pkgEnvFile <- getSandboxConfigFilePath globalFlags
  createPackageEnvironmentFile verbosity sandboxDir pkgEnvFile comp platform
  (_sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity globalFlags
  let config      = pkgEnvSavedConfig pkgEnv
      configFlags = savedConfigureFlags config

  -- Create the index file if it doesn't exist.
  indexFile <- tryGetIndexFilePath verbosity config
  indexFileExists <- doesFileExist indexFile
  if indexFileExists
    then notice verbosity $ "Using an existing sandbox located at " ++ sandboxDir
    else notice verbosity $ "Creating a new sandbox at " ++ sandboxDir
  Index.createEmpty verbosity indexFile

  -- Create the package DB for the default compiler.
  initPackageDBIfNeeded verbosity configFlags comp progdb
  maybeAddCompilerTimestampRecord verbosity sandboxDir indexFile
    (compilerId comp) platform

-- | Entry point for the 'cabal sandbox delete' command.
sandboxDelete :: Verbosity -> SandboxFlags -> GlobalFlags -> IO ()
sandboxDelete verbosity _sandboxFlags globalFlags = do
  (useSandbox, _) <- loadConfigOrSandboxConfig
                       verbosity
                       globalFlags { globalRequireSandbox = Flag False }
  case useSandbox of
    NoSandbox -> warn verbosity "Not in a sandbox."
    UseSandbox sandboxDir -> do
      curDir     <- getCurrentDirectory
      pkgEnvFile <- getSandboxConfigFilePath globalFlags

      -- Remove the @cabal.sandbox.config@ file, unless it's in a non-standard
      -- location.
      let isNonDefaultConfigLocation = not $ equalFilePath pkgEnvFile $
                                       curDir </> sandboxPackageEnvironmentFile

      if isNonDefaultConfigLocation
        then warn verbosity $ "Sandbox config file is in non-default location: '"
                    ++ pkgEnvFile ++ "'.\n Please delete manually."
        else removeFile pkgEnvFile

      -- Remove the sandbox directory, unless we're using a shared sandbox.
      let isNonDefaultSandboxLocation = not $ equalFilePath sandboxDir $
                                        curDir </> defaultSandboxLocation

      when isNonDefaultSandboxLocation $
        die' verbosity $ "Non-default sandbox location used: '" ++ sandboxDir
        ++ "'.\nAssuming a shared sandbox. Please delete '"
        ++ sandboxDir ++ "' manually."

      absSandboxDir <- canonicalizePath sandboxDir
      notice verbosity $ "Deleting the sandbox located at " ++ absSandboxDir
      removeDirectoryRecursive absSandboxDir

      let
        pathInsideSandbox = isPrefixOf absSandboxDir

        -- Warn the user if deleting the sandbox deleted a package database
        -- referenced in the current environment.
        checkPackagePaths var = do
          let
            checkPath path = do
              absPath <- canonicalizePath path
              (when (pathInsideSandbox absPath) . warn verbosity)
                (var ++ " refers to package database " ++ path
                 ++ " inside the deleted sandbox.")
          liftM (maybe [] splitSearchPath) (lookupEnv var) >>= mapM_ checkPath

      checkPackagePaths "CABAL_SANDBOX_PACKAGE_PATH"
      checkPackagePaths "GHC_PACKAGE_PATH"
      checkPackagePaths "GHCJS_PACKAGE_PATH"

-- Common implementation of 'sandboxAddSource' and 'sandboxAddSourceSnapshot'.
doAddSource :: Verbosity -> [FilePath] -> FilePath -> PackageEnvironment
               -> BuildTreeRefType
               -> IO ()
doAddSource verbosity buildTreeRefs sandboxDir pkgEnv refType = do
  let savedConfig       = pkgEnvSavedConfig pkgEnv
  indexFile            <- tryGetIndexFilePath verbosity savedConfig

  -- If we're running 'sandbox add-source' for the first time for this compiler,
  -- we need to create an initial timestamp record.
  (comp, platform, _) <- configCompilerAuxEx . savedConfigureFlags $ savedConfig
  maybeAddCompilerTimestampRecord verbosity sandboxDir indexFile
    (compilerId comp) platform

  withAddTimestamps verbosity sandboxDir $ do
    -- Path canonicalisation is done in addBuildTreeRefs, but we do it
    -- twice because of the timestamps file.
    buildTreeRefs' <- mapM tryCanonicalizePath buildTreeRefs
    Index.addBuildTreeRefs verbosity indexFile buildTreeRefs' refType
    return buildTreeRefs'

-- | Entry point for the 'cabal sandbox add-source' command.
sandboxAddSource :: Verbosity -> [FilePath] -> SandboxFlags -> GlobalFlags
                    -> IO ()
sandboxAddSource verbosity buildTreeRefs sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity globalFlags

  if fromFlagOrDefault False (sandboxSnapshot sandboxFlags)
    then sandboxAddSourceSnapshot verbosity buildTreeRefs sandboxDir pkgEnv
    else doAddSource verbosity buildTreeRefs sandboxDir pkgEnv LinkRef

-- | Entry point for the 'cabal sandbox add-source --snapshot' command.
sandboxAddSourceSnapshot :: Verbosity -> [FilePath] -> FilePath
                            -> PackageEnvironment
                            -> IO ()
sandboxAddSourceSnapshot verbosity buildTreeRefs sandboxDir pkgEnv = do
  let snapshotDir = sandboxDir </> snapshotDirectoryName

  -- Use 'D.S.SrcDist.prepareTree' to copy each package's files to our private
  -- location.
  createDirectoryIfMissingVerbose verbosity True snapshotDir

  -- Collect the package descriptions first, so that if some path does not refer
  -- to a cabal package, we fail immediately.
  pkgs      <- forM buildTreeRefs $ \buildTreeRef ->
    inDir (Just buildTreeRef) $
    return . flattenPackageDescription
            =<< readGenericPackageDescription verbosity
            =<< defaultPackageDesc     verbosity

  -- Copy the package sources to "snapshots/$PKGNAME-$VERSION-tmp". If
  -- 'prepareTree' throws an error at any point, the old snapshots will still be
  -- in consistent state.
  tmpDirs <- forM (zip buildTreeRefs pkgs) $ \(buildTreeRef, pkg) ->
    inDir (Just buildTreeRef) $ do
      let targetDir    = snapshotDir </> (display . packageId $ pkg)
          targetTmpDir = targetDir ++ "-tmp"
      dirExists <- doesDirectoryExist targetTmpDir
      when dirExists $
        removeDirectoryRecursive targetDir
      createDirectory targetTmpDir
      prepareTree verbosity pkg Nothing targetTmpDir knownSuffixHandlers
      return (targetTmpDir, targetDir)

  -- Now rename the "snapshots/$PKGNAME-$VERSION-tmp" dirs to
  -- "snapshots/$PKGNAME-$VERSION".
  snapshots <- forM tmpDirs $ \(targetTmpDir, targetDir) -> do
    dirExists <- doesDirectoryExist targetDir
    when dirExists $
      removeDirectoryRecursive targetDir
    renameDirectory targetTmpDir targetDir
    return targetDir

  -- Once the packages are copied, just 'add-source' them as usual.
  doAddSource verbosity snapshots sandboxDir pkgEnv SnapshotRef

-- | Entry point for the 'cabal sandbox delete-source' command.
sandboxDeleteSource :: Verbosity -> [FilePath] -> SandboxFlags -> GlobalFlags
                       -> IO ()
sandboxDeleteSource verbosity buildTreeRefs _sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity globalFlags
  indexFile            <- tryGetIndexFilePath verbosity (pkgEnvSavedConfig pkgEnv)

  (results, convDict) <-
    Index.removeBuildTreeRefs verbosity indexFile buildTreeRefs

  let (failedPaths, removedPaths) = partitionEithers results
      removedRefs = fmap convDict removedPaths

  unless (null removedPaths) $ do
    removeTimestamps verbosity sandboxDir removedPaths

    notice verbosity $ "Success deleting sources: " ++
      showL removedRefs ++ "\n\n"

  unless (null failedPaths) $ do
    let groupedFailures = groupBy errorType failedPaths
    mapM_ handleErrors groupedFailures
    die' verbosity $ "The sources with the above errors were skipped. (" ++
      showL (fmap getPath failedPaths) ++ ")"

  notice verbosity $ "Note: 'sandbox delete-source' only unregisters the " ++
    "source dependency, but does not remove the package " ++
    "from the sandbox package DB.\n\n" ++
    "Use 'sandbox hc-pkg -- unregister' to do that."
  where
    getPath (Index.ErrNonregisteredSource p) = p
    getPath (Index.ErrNonexistentSource p) = p

    showPaths f = concat . intersperse " " . fmap (show . f)

    showL = showPaths id

    showE [] = return ' '
    showE errs = showPaths getPath errs

    errorType Index.ErrNonregisteredSource{} Index.ErrNonregisteredSource{} =
      True
    errorType Index.ErrNonexistentSource{} Index.ErrNonexistentSource{} = True
    errorType _ _ = False

    handleErrors [] = return ()
    handleErrors errs@(Index.ErrNonregisteredSource{}:_) =
      warn verbosity ("Sources not registered: " ++ showE errs ++ "\n\n")
    handleErrors errs@(Index.ErrNonexistentSource{}:_)   =
      warn verbosity
      ("Source directory not found for paths: " ++ showE errs ++ "\n"
       ++ "If you are trying to delete a reference to a removed directory, "
       ++ "please provide the full absolute path "
       ++ "(as given by `sandbox list-sources`).\n\n")

-- | Entry point for the 'cabal sandbox list-sources' command.
sandboxListSources :: Verbosity -> SandboxFlags -> GlobalFlags
                      -> IO ()
sandboxListSources verbosity _sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity globalFlags
  indexFile            <- tryGetIndexFilePath verbosity (pkgEnvSavedConfig pkgEnv)

  refs <- Index.listBuildTreeRefs verbosity
          Index.ListIgnored Index.LinksAndSnapshots indexFile
  when (null refs) $
    notice verbosity $ "Index file '" ++ indexFile
    ++ "' has no references to local build trees."
  when (not . null $ refs) $ do
    notice verbosity $ "Source dependencies registered "
      ++ "in the current sandbox ('" ++ sandboxDir ++ "'):\n\n"
    mapM_ putStrLn refs
    notice verbosity $ "\nTo unregister source dependencies, "
                       ++ "use the 'sandbox delete-source' command."

-- | Entry point for the 'cabal sandbox hc-pkg' command. Invokes the @hc-pkg@
-- tool with provided arguments, restricted to the sandbox.
sandboxHcPkg :: Verbosity -> SandboxFlags -> GlobalFlags -> [String] -> IO ()
sandboxHcPkg verbosity _sandboxFlags globalFlags extraArgs = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity globalFlags
  let configFlags = savedConfigureFlags . pkgEnvSavedConfig $ pkgEnv
  -- Invoke hc-pkg for the most recently configured compiler (if any),
  -- using the right package-db for the compiler (see #1935).
  (comp, platform, progdb) <- getPersistOrConfigCompiler configFlags
  let dir         = sandboxPackageDBPath sandboxDir comp platform
      dbStack     = [GlobalPackageDB, SpecificPackageDB dir]
  Register.invokeHcPkg verbosity comp progdb dbStack extraArgs

updateInstallDirs :: Flag Bool
                  -> (UseSandbox, SavedConfig) -> (UseSandbox, SavedConfig)
updateInstallDirs userInstallFlag (useSandbox, savedConfig) =
  case useSandbox of
    NoSandbox ->
      let savedConfig' = savedConfig {
            savedConfigureFlags = configureFlags {
                configInstallDirs = installDirs
            }
          }
      in (useSandbox, savedConfig')
    _ -> (useSandbox, savedConfig)
  where
    configureFlags = savedConfigureFlags savedConfig
    userInstallDirs = savedUserInstallDirs savedConfig
    globalInstallDirs = savedGlobalInstallDirs savedConfig
    installDirs | userInstall = userInstallDirs
                | otherwise   = globalInstallDirs
    userInstall = fromFlagOrDefault defaultUserInstall
                  (configUserInstall configureFlags `mappend` userInstallFlag)

-- | Check which type of package environment we're in and return a
-- correctly-initialised @SavedConfig@ and a @UseSandbox@ value that indicates
-- whether we're working in a sandbox.
loadConfigOrSandboxConfig :: Verbosity
                          -> GlobalFlags  -- ^ For @--config-file@ and
                                          -- @--sandbox-config-file@.
                          -> IO (UseSandbox, SavedConfig)
loadConfigOrSandboxConfig verbosity globalFlags = do
  let configFileFlag        = globalConfigFile        globalFlags
      sandboxConfigFileFlag = globalSandboxConfigFile globalFlags
      ignoreSandboxFlag     = globalIgnoreSandbox     globalFlags

  pkgEnvDir  <- getPkgEnvDir sandboxConfigFileFlag
  pkgEnvType <- classifyPackageEnvironment pkgEnvDir sandboxConfigFileFlag
                                           ignoreSandboxFlag
  case pkgEnvType of
    -- A @cabal.sandbox.config@ file (and possibly @cabal.config@) is present.
    SandboxPackageEnvironment -> do
      (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity globalFlags
                              -- Prints an error message and exits on error.
      let config = pkgEnvSavedConfig pkgEnv
      return (UseSandbox sandboxDir, config)

    -- Only @cabal.config@ is present.
    UserPackageEnvironment    -> do
      config <- loadConfig verbosity configFileFlag
      userConfig <- loadUserConfig verbosity pkgEnvDir Nothing
      let config' = config `mappend` userConfig
      dieIfSandboxRequired config'
      return (NoSandbox, config')

    -- Neither @cabal.sandbox.config@ nor @cabal.config@ are present.
    AmbientPackageEnvironment -> do
      config <- loadConfig verbosity configFileFlag
      let globalConstraintsOpt =
            flagToMaybe . globalConstraintsFile . savedGlobalFlags $ config
      globalConstraintConfig <-
        loadUserConfig verbosity pkgEnvDir globalConstraintsOpt
      let config' = config `mappend` globalConstraintConfig
      dieIfSandboxRequired config
      return (NoSandbox, config')

  where
    -- Return the path to the package environment directory - either the
    -- current directory or the one that @--sandbox-config-file@ resides in.
    getPkgEnvDir :: (Flag FilePath) -> IO FilePath
    getPkgEnvDir sandboxConfigFileFlag = do
      case sandboxConfigFileFlag of
        NoFlag    -> getCurrentDirectory
        Flag path -> tryCanonicalizePath . takeDirectory $ path

    -- Die if @--require-sandbox@ was specified and we're not inside a sandbox.
    dieIfSandboxRequired :: SavedConfig -> IO ()
    dieIfSandboxRequired config = checkFlag flag
      where
        flag = (globalRequireSandbox . savedGlobalFlags $ config)
               `mappend` (globalRequireSandbox globalFlags)
        checkFlag (Flag True)  =
          die' verbosity $ "'require-sandbox' is set to True, but no sandbox is present. "
             ++ "Use '--no-require-sandbox' if you want to override "
             ++ "'require-sandbox' temporarily."
        checkFlag (Flag False) = return ()
        checkFlag (NoFlag)     = return ()

-- | Return the saved \"dist/\" prefix, or the default prefix.
findSavedDistPref :: SavedConfig -> Flag FilePath -> IO FilePath
findSavedDistPref config flagDistPref = do
    let defDistPref = useDistPref defaultSetupScriptOptions
        flagDistPref' = configDistPref (savedConfigureFlags config)
                        `mappend` flagDistPref
    findDistPref defDistPref flagDistPref'

-- | If we're in a sandbox, call @withSandboxBinDirOnSearchPath@, otherwise do
-- nothing.
maybeWithSandboxDirOnSearchPath :: UseSandbox -> IO a -> IO a
maybeWithSandboxDirOnSearchPath NoSandbox               act = act
maybeWithSandboxDirOnSearchPath (UseSandbox sandboxDir) act =
  withSandboxBinDirOnSearchPath sandboxDir $ act

-- | Had reinstallAddSourceDeps actually reinstalled any dependencies?
data WereDepsReinstalled = ReinstalledSomeDeps | NoDepsReinstalled

-- | Reinstall those add-source dependencies that have been modified since
-- we've last installed them. Assumes that we're working inside a sandbox.
reinstallAddSourceDeps :: Verbosity
                          -> ConfigFlags  -> ConfigExFlags
                          -> InstallFlags -> GlobalFlags
                          -> FilePath
                          -> IO WereDepsReinstalled
reinstallAddSourceDeps verbosity configFlags' configExFlags
                       installFlags globalFlags sandboxDir = topHandler' $ do
  let sandboxDistPref     = sandboxBuildDir sandboxDir
      configFlags         = configFlags'
                            { configDistPref  = Flag sandboxDistPref }
      haddockFlags        = mempty
                            { haddockDistPref = Flag sandboxDistPref }
  (comp, platform, progdb) <- configCompilerAux' configFlags
  retVal                   <- newIORef NoDepsReinstalled

  withSandboxPackageInfo verbosity configFlags globalFlags
                         comp platform progdb sandboxDir $ \sandboxPkgInfo ->
    unless (null $ modifiedAddSourceDependencies sandboxPkgInfo) $ do

      withRepoContext verbosity globalFlags $ \repoContext -> do
        let args :: InstallArgs
            args = ((configPackageDB' configFlags)
                  ,repoContext
                  ,comp, platform, progdb
                  ,UseSandbox sandboxDir, Just sandboxPkgInfo
                  ,globalFlags, configFlags, configExFlags, installFlags
                  ,haddockFlags)

        -- This can actually be replaced by a call to 'install', but we use a
        -- lower-level API because of layer separation reasons. Additionally, we
        -- might want to use some lower-level features this in the future.
        withSandboxBinDirOnSearchPath sandboxDir $ do
          installContext <- makeInstallContext verbosity args Nothing
          installPlan    <- foldProgress logMsg die'' return =<<
                            makeInstallPlan verbosity args installContext

          processInstallPlan verbosity args installContext installPlan
          writeIORef retVal ReinstalledSomeDeps

  readIORef retVal

    where
      die'' message = die' verbosity (message ++ installFailedInSandbox)
      -- TODO: use a better error message, remove duplication.
      installFailedInSandbox =
        "Note: when using a sandbox, all packages are required to have "
        ++ "consistent dependencies. Try reinstalling/unregistering the "
        ++ "offending packages or recreating the sandbox."
      logMsg message rest = debugNoWrap verbosity message >> rest

      topHandler' = topHandlerWith $ \_ -> do
        warn verbosity "Couldn't reinstall some add-source dependencies."
        -- Here we can't know whether any deps have been reinstalled, so we have
        -- to be conservative.
        return ReinstalledSomeDeps

-- | Produce a 'SandboxPackageInfo' and feed it to the given action. Note that
-- we don't update the timestamp file here - this is done in
-- 'postInstallActions'.
withSandboxPackageInfo :: Verbosity -> ConfigFlags -> GlobalFlags
                          -> Compiler -> Platform -> ProgramDb
                          -> FilePath
                          -> (SandboxPackageInfo -> IO ())
                          -> IO ()
withSandboxPackageInfo verbosity configFlags globalFlags
                       comp platform progdb sandboxDir cont = do
  -- List all add-source deps.
  indexFile              <- tryGetIndexFilePath' verbosity globalFlags
  buildTreeRefs          <- Index.listBuildTreeRefs verbosity
                            Index.DontListIgnored Index.OnlyLinks indexFile
  let allAddSourceDepsSet = S.fromList buildTreeRefs

  -- List all packages installed in the sandbox.
  installedPkgIndex <- getInstalledPackagesInSandbox verbosity
                       configFlags comp progdb
  let err = "Error reading sandbox package information."
  -- Get the package descriptions for all add-source deps.
  depsCabalFiles <- mapM (flip (tryFindAddSourcePackageDesc verbosity) err) buildTreeRefs
  depsPkgDescs   <- mapM (readGenericPackageDescription verbosity) depsCabalFiles
  let depsMap           = M.fromList (zip buildTreeRefs depsPkgDescs)
      isInstalled pkgid = not . null
        . InstalledPackageIndex.lookupSourcePackageId installedPkgIndex $ pkgid
      installedDepsMap  = M.filter (isInstalled . packageId) depsMap

  -- Get the package ids of modified (and installed) add-source deps.
  modifiedAddSourceDeps <- listModifiedDeps verbosity sandboxDir
                           (compilerId comp) platform installedDepsMap
  -- 'fromJust' here is safe because 'modifiedAddSourceDeps' are guaranteed to
  -- be a subset of the keys of 'depsMap'.
  let modifiedDeps    = [ (modDepPath, fromJust $ M.lookup modDepPath depsMap)
                        | modDepPath <- modifiedAddSourceDeps ]
      modifiedDepsMap = M.fromList modifiedDeps

  assert (all (`S.member` allAddSourceDepsSet) modifiedAddSourceDeps) (return ())
  if (null modifiedDeps)
    then info   verbosity $ "Found no modified add-source deps."
    else notice verbosity $ "Some add-source dependencies have been modified. "
                            ++ "They will be reinstalled..."

  -- Get the package ids of the remaining add-source deps (some are possibly not
  -- installed).
  let otherDeps = M.assocs (depsMap `M.difference` modifiedDepsMap)

  -- Finally, assemble a 'SandboxPackageInfo'.
  cont $ SandboxPackageInfo (map toSourcePackage modifiedDeps)
    (map toSourcePackage otherDeps) installedPkgIndex allAddSourceDepsSet

  where
    toSourcePackage (path, pkgDesc) = SourcePackage
      (packageId pkgDesc) pkgDesc (LocalUnpackedPackage path) Nothing

-- | Same as 'withSandboxPackageInfo' if we're inside a sandbox and the
-- identity otherwise.
maybeWithSandboxPackageInfo :: Verbosity -> ConfigFlags -> GlobalFlags
                               -> Compiler -> Platform -> ProgramDb
                               -> UseSandbox
                               -> (Maybe SandboxPackageInfo -> IO ())
                               -> IO ()
maybeWithSandboxPackageInfo verbosity configFlags globalFlags
                            comp platform progdb useSandbox cont =
  case useSandbox of
    NoSandbox             -> cont Nothing
    UseSandbox sandboxDir -> withSandboxPackageInfo verbosity
                             configFlags globalFlags
                             comp platform progdb sandboxDir
                             (\spi -> cont (Just spi))

-- | Check if a sandbox is present and call @reinstallAddSourceDeps@ in that
-- case.
maybeReinstallAddSourceDeps :: Verbosity
                               -> Flag (Maybe Int) -- ^ The '-j' flag
                               -> ConfigFlags      -- ^ Saved configure flags
                                                   -- (from dist/setup-config)
                               -> GlobalFlags
                               -> (UseSandbox, SavedConfig)
                               -> IO WereDepsReinstalled
maybeReinstallAddSourceDeps verbosity numJobsFlag configFlags'
                            globalFlags' (useSandbox, config) = do
  case useSandbox of
    NoSandbox             -> return NoDepsReinstalled
    UseSandbox sandboxDir -> do
      -- Reinstall the modified add-source deps.
      let configFlags    = savedConfigureFlags config
                           `mappendSomeSavedFlags`
                           configFlags'
          configExFlags  = defaultConfigExFlags
                           `mappend` savedConfigureExFlags config
          installFlags'  = defaultInstallFlags
                           `mappend` savedInstallFlags config
          installFlags   = installFlags' {
            installNumJobs    = installNumJobs installFlags'
                                `mappend` numJobsFlag
            }
          globalFlags    = savedGlobalFlags config
          -- This makes it possible to override things like 'remote-repo-cache'
          -- from the command line. These options are hidden, and are only
          -- useful for debugging, so this should be fine.
                           `mappend` globalFlags'
      reinstallAddSourceDeps
        verbosity configFlags configExFlags
        installFlags globalFlags sandboxDir

  where

    -- NOTE: we can't simply do @sandboxConfigFlags `mappend` savedFlags@
    -- because we don't want to auto-enable things like 'library-profiling' for
    -- all add-source dependencies even if the user has passed
    -- '--enable-library-profiling' to 'cabal configure'. These options are
    -- supposed to be set in 'cabal.config'.
    mappendSomeSavedFlags :: ConfigFlags -> ConfigFlags -> ConfigFlags
    mappendSomeSavedFlags sandboxConfigFlags savedFlags =
      sandboxConfigFlags {
        configHcFlavor     = configHcFlavor sandboxConfigFlags
                             `mappend` configHcFlavor savedFlags,
        configHcPath       = configHcPath sandboxConfigFlags
                             `mappend` configHcPath savedFlags,
        configHcPkg        = configHcPkg sandboxConfigFlags
                             `mappend` configHcPkg savedFlags,
        configProgramPaths = configProgramPaths sandboxConfigFlags
                             `mappend` configProgramPaths savedFlags,
        configProgramArgs  = configProgramArgs sandboxConfigFlags
                             `mappend` configProgramArgs savedFlags,
        -- NOTE: Unconditionally choosing the value from
        -- 'dist/setup-config'. Sandbox package DB location may have been
        -- changed by 'configure -w'.
        configPackageDBs   = configPackageDBs savedFlags
        -- FIXME: Is this compatible with the 'inherit' feature?
        }

--
-- Utils (transitionary)
--

-- | Try to read the most recently configured compiler from the
-- 'localBuildInfoFile', falling back on 'configCompilerAuxEx' if it
-- cannot be read.
getPersistOrConfigCompiler :: ConfigFlags
                           -> IO (Compiler, Platform, ProgramDb)
getPersistOrConfigCompiler configFlags = do
  distPref <- findDistPrefOrDefault (configDistPref configFlags)
  mlbi <- maybeGetPersistBuildConfig distPref
  case mlbi of
    Nothing  -> do configCompilerAux' configFlags
    Just lbi -> return ( LocalBuildInfo.compiler lbi
                       , LocalBuildInfo.hostPlatform lbi
                       , LocalBuildInfo.withPrograms lbi
                       )

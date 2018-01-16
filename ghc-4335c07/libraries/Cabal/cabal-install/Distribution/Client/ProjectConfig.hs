{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, DeriveDataTypeable #-}

-- | Handling project configuration.
--
module Distribution.Client.ProjectConfig (

    -- * Types for project config
    ProjectConfig(..),
    ProjectConfigBuildOnly(..),
    ProjectConfigShared(..),
    ProjectConfigProvenance(..),
    PackageConfig(..),
    MapLast(..),
    MapMappend(..),

    -- * Project root
    findProjectRoot,
    ProjectRoot(..),
    BadProjectRoot(..),

    -- * Project config files
    readProjectConfig,
    readGlobalConfig,
    readProjectLocalFreezeConfig,
    writeProjectLocalExtraConfig,
    writeProjectLocalFreezeConfig,
    writeProjectConfigFile,
    commandLineFlagsToProjectConfig,

    -- * Packages within projects
    ProjectPackageLocation(..),
    BadPackageLocations(..),
    BadPackageLocation(..),
    BadPackageLocationMatch(..),
    findProjectPackages,
    readSourcePackage,

    -- * Resolving configuration
    lookupLocalPackageConfig,
    projectConfigWithBuilderRepoContext,
    projectConfigWithSolverRepoContext,
    SolverSettings(..),
    resolveSolverSettings,
    BuildTimeSettings(..),
    resolveBuildTimeSettings,

    -- * Checking configuration
    checkBadPerPackageCompilerPaths,
    BadPerPackageCompilerPaths(..)
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.ProjectConfig.Types
import Distribution.Client.ProjectConfig.Legacy
import Distribution.Client.RebuildMonad
import Distribution.Client.Glob
         ( isTrivialFilePathGlob )

import Distribution.Client.Types
import Distribution.Client.DistDirLayout
         ( DistDirLayout(..), CabalDirLayout(..), ProjectRoot(..) )
import Distribution.Client.GlobalFlags
         ( RepoContext(..), withRepoContext' )
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )
import Distribution.Client.Config
         ( loadConfig, getConfigFilePath )

import Distribution.Solver.Types.SourcePackage
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.PackageConstraint
         ( PackageProperty(..) )

import Distribution.Package
         ( PackageName, PackageId, packageId, UnitId )
import Distribution.Types.Dependency
import Distribution.System
         ( Platform )
import Distribution.PackageDescription
         ( SourceRepo(..) )
import Distribution.PackageDescription.Parsec
         ( readGenericPackageDescription )
import Distribution.Simple.Compiler
         ( Compiler, compilerInfo )
import Distribution.Simple.Program
         ( ConfiguredProgram(..) )
import Distribution.Simple.Setup
         ( Flag(Flag), toFlag, flagToMaybe, flagToList
         , fromFlag, fromFlagOrDefault )
import Distribution.Client.Setup
         ( defaultSolver, defaultMaxBackjumps )
import Distribution.Simple.InstallDirs
         ( PathTemplate, fromPathTemplate
         , toPathTemplate, substPathTemplate, initialPathTemplateEnv )
import Distribution.Simple.Utils
         ( die', warn )
import Distribution.Client.Utils
         ( determineNumJobs )
import Distribution.Utils.NubList
         ( fromNubList )
import Distribution.Verbosity
         ( Verbosity, modifyVerbosity, verbose )
import Distribution.Text
import Distribution.ParseUtils
         ( ParseResult(..), locatedErrorMsg, showPWarning )

import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Exception
import Data.Either
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath hiding (combine)
import System.Directory
import Network.URI (URI(..), URIAuth(..), parseAbsoluteURI)


----------------------------------------
-- Resolving configuration to settings
--

-- | Look up a 'PackageConfig' field in the 'ProjectConfig' for a specific
-- 'PackageName'. This returns the configuration that applies to all local
-- packages plus any package-specific configuration for this package.
--
lookupLocalPackageConfig :: (Semigroup a, Monoid a)
                         => (PackageConfig -> a)
                         -> ProjectConfig
                         -> PackageName -> a
lookupLocalPackageConfig field ProjectConfig {
                           projectConfigLocalPackages,
                           projectConfigSpecificPackage
                         } pkgname =
    field projectConfigLocalPackages
 <> maybe mempty field
          (Map.lookup pkgname (getMapMappend projectConfigSpecificPackage))


-- | Use a 'RepoContext' based on the 'BuildTimeSettings'.
--
projectConfigWithBuilderRepoContext :: Verbosity
                                    -> BuildTimeSettings
                                    -> (RepoContext -> IO a) -> IO a
projectConfigWithBuilderRepoContext verbosity BuildTimeSettings{..} =
    withRepoContext'
      verbosity
      buildSettingRemoteRepos
      buildSettingLocalRepos
      buildSettingCacheDir
      buildSettingHttpTransport
      (Just buildSettingIgnoreExpiry)


-- | Use a 'RepoContext', but only for the solver. The solver does not use the
-- full facilities of the 'RepoContext' so we can get away with making one
-- that doesn't have an http transport. And that avoids having to have access
-- to the 'BuildTimeSettings'
--
projectConfigWithSolverRepoContext :: Verbosity
                                   -> ProjectConfigShared
                                   -> ProjectConfigBuildOnly
                                   -> (RepoContext -> IO a) -> IO a
projectConfigWithSolverRepoContext verbosity
                                   ProjectConfigShared{..}
                                   ProjectConfigBuildOnly{..} =
    withRepoContext'
      verbosity
      (fromNubList projectConfigRemoteRepos)
      (fromNubList projectConfigLocalRepos)
      (fromFlagOrDefault (error "projectConfigWithSolverRepoContext: projectConfigCacheDir")
                         projectConfigCacheDir)
      (flagToMaybe projectConfigHttpTransport)
      (flagToMaybe projectConfigIgnoreExpiry)


-- | Resolve the project configuration, with all its optional fields, into
-- 'SolverSettings' with no optional fields (by applying defaults).
--
resolveSolverSettings :: ProjectConfig -> SolverSettings
resolveSolverSettings ProjectConfig{
                        projectConfigShared,
                        projectConfigLocalPackages,
                        projectConfigSpecificPackage
                      } =
    SolverSettings {..}
  where
    --TODO: [required eventually] some of these settings need validation, e.g.
    -- the flag assignments need checking.
    solverSettingRemoteRepos       = fromNubList projectConfigRemoteRepos
    solverSettingLocalRepos        = fromNubList projectConfigLocalRepos
    solverSettingConstraints       = projectConfigConstraints
    solverSettingPreferences       = projectConfigPreferences
    solverSettingFlagAssignment    = packageConfigFlagAssignment projectConfigLocalPackages
    solverSettingFlagAssignments   = fmap packageConfigFlagAssignment
                                          (getMapMappend projectConfigSpecificPackage)
    solverSettingCabalVersion      = flagToMaybe projectConfigCabalVersion
    solverSettingSolver            = fromFlag projectConfigSolver
    solverSettingAllowOlder        = fromMaybe mempty projectConfigAllowOlder
    solverSettingAllowNewer        = fromMaybe mempty projectConfigAllowNewer
    solverSettingMaxBackjumps      = case fromFlag projectConfigMaxBackjumps of
                                       n | n < 0     -> Nothing
                                         | otherwise -> Just n
    solverSettingReorderGoals      = fromFlag projectConfigReorderGoals
    solverSettingCountConflicts    = fromFlag projectConfigCountConflicts
    solverSettingStrongFlags       = fromFlag projectConfigStrongFlags
    solverSettingAllowBootLibInstalls = fromFlag projectConfigAllowBootLibInstalls
    solverSettingIndexState        = flagToMaybe projectConfigIndexState
    solverSettingIndependentGoals  = fromFlag projectConfigIndependentGoals
  --solverSettingShadowPkgs        = fromFlag projectConfigShadowPkgs
  --solverSettingReinstall         = fromFlag projectConfigReinstall
  --solverSettingAvoidReinstalls   = fromFlag projectConfigAvoidReinstalls
  --solverSettingOverrideReinstall = fromFlag projectConfigOverrideReinstall
  --solverSettingUpgradeDeps       = fromFlag projectConfigUpgradeDeps

    ProjectConfigShared {..} = defaults <> projectConfigShared

    defaults = mempty {
       projectConfigSolver            = Flag defaultSolver,
       projectConfigAllowOlder        = Just (AllowOlder mempty),
       projectConfigAllowNewer        = Just (AllowNewer mempty),
       projectConfigMaxBackjumps      = Flag defaultMaxBackjumps,
       projectConfigReorderGoals      = Flag (ReorderGoals False),
       projectConfigCountConflicts    = Flag (CountConflicts True),
       projectConfigStrongFlags       = Flag (StrongFlags False),
       projectConfigAllowBootLibInstalls = Flag (AllowBootLibInstalls False),
       projectConfigIndependentGoals  = Flag (IndependentGoals False)
     --projectConfigShadowPkgs        = Flag False,
     --projectConfigReinstall         = Flag False,
     --projectConfigAvoidReinstalls   = Flag False,
     --projectConfigOverrideReinstall = Flag False,
     --projectConfigUpgradeDeps       = Flag False
    }


-- | Resolve the project configuration, with all its optional fields, into
-- 'BuildTimeSettings' with no optional fields (by applying defaults).
--
resolveBuildTimeSettings :: Verbosity
                         -> CabalDirLayout
                         -> ProjectConfig
                         -> BuildTimeSettings
resolveBuildTimeSettings verbosity
                         CabalDirLayout {
                           cabalLogsDirectory
                         }
                         ProjectConfig {
                           projectConfigShared = ProjectConfigShared {
                             projectConfigRemoteRepos,
                             projectConfigLocalRepos
                           },
                           projectConfigBuildOnly
                         } =
    BuildTimeSettings {..}
  where
    buildSettingDryRun        = fromFlag    projectConfigDryRun
    buildSettingOnlyDeps      = fromFlag    projectConfigOnlyDeps
    buildSettingSummaryFile   = fromNubList projectConfigSummaryFile
    --buildSettingLogFile       -- defined below, more complicated
    --buildSettingLogVerbosity  -- defined below, more complicated
    buildSettingBuildReports  = fromFlag    projectConfigBuildReports
    buildSettingSymlinkBinDir = flagToList  projectConfigSymlinkBinDir
    buildSettingOneShot       = fromFlag    projectConfigOneShot
    buildSettingNumJobs       = determineNumJobs projectConfigNumJobs
    buildSettingKeepGoing     = fromFlag    projectConfigKeepGoing
    buildSettingOfflineMode   = fromFlag    projectConfigOfflineMode
    buildSettingKeepTempFiles = fromFlag    projectConfigKeepTempFiles
    buildSettingRemoteRepos   = fromNubList projectConfigRemoteRepos
    buildSettingLocalRepos    = fromNubList projectConfigLocalRepos
    buildSettingCacheDir      = fromFlag    projectConfigCacheDir
    buildSettingHttpTransport = flagToMaybe projectConfigHttpTransport
    buildSettingIgnoreExpiry  = fromFlag    projectConfigIgnoreExpiry
    buildSettingReportPlanningFailure
                              = fromFlag projectConfigReportPlanningFailure

    ProjectConfigBuildOnly{..} = defaults
                              <> projectConfigBuildOnly

    defaults = mempty {
      projectConfigDryRun                = toFlag False,
      projectConfigOnlyDeps              = toFlag False,
      projectConfigBuildReports          = toFlag NoReports,
      projectConfigReportPlanningFailure = toFlag False,
      projectConfigKeepGoing             = toFlag False,
      projectConfigOneShot               = toFlag False,
      projectConfigOfflineMode           = toFlag False,
      projectConfigKeepTempFiles         = toFlag False,
      projectConfigIgnoreExpiry          = toFlag False
    }

    -- The logging logic: what log file to use and what verbosity.
    --
    -- If the user has specified --remote-build-reporting=detailed, use the
    -- default log file location. If the --build-log option is set, use the
    -- provided location. Otherwise don't use logging, unless building in
    -- parallel (in which case the default location is used).
    --
    buildSettingLogFile :: Maybe (Compiler -> Platform
                               -> PackageId -> UnitId -> FilePath)
    buildSettingLogFile
      | useDefaultTemplate = Just (substLogFileName defaultTemplate)
      | otherwise          = fmap  substLogFileName givenTemplate

    defaultTemplate = toPathTemplate $
                        cabalLogsDirectory </>
                        "$compiler" </> "$libname" <.> "log"
    givenTemplate   = flagToMaybe projectConfigLogFile

    useDefaultTemplate
      | buildSettingBuildReports == DetailedReports = True
      | isJust givenTemplate                        = False
      | isParallelBuild                             = True
      | otherwise                                   = False

    isParallelBuild = buildSettingNumJobs >= 2

    substLogFileName :: PathTemplate
                     -> Compiler -> Platform
                     -> PackageId -> UnitId -> FilePath
    substLogFileName template compiler platform pkgid uid =
        fromPathTemplate (substPathTemplate env template)
      where
        env = initialPathTemplateEnv
                pkgid uid (compilerInfo compiler) platform

    -- If the user has specified --remote-build-reporting=detailed or
    -- --build-log, use more verbose logging.
    --
    buildSettingLogVerbosity
      | overrideVerbosity = modifyVerbosity (max verbose) verbosity
      | otherwise         = verbosity

    overrideVerbosity
      | buildSettingBuildReports == DetailedReports = True
      | isJust givenTemplate                        = True
      | isParallelBuild                             = False
      | otherwise                                   = False


---------------------------------------------
-- Reading and writing project config files
--

-- | Find the root of this project.
--
-- Searches for an explicit @cabal.project@ file, in the current directory or
-- parent directories. If no project file is found then the current dir is the
-- project root (and the project will use an implicit config).
--
findProjectRoot :: Maybe FilePath -- ^ starting directory, or current directory
                -> Maybe FilePath -- ^ @cabal.project@ file name override
                -> IO (Either BadProjectRoot ProjectRoot)
findProjectRoot _ (Just projectFile)
  | isAbsolute projectFile = do
    exists <- doesFileExist projectFile
    if exists
      then do projectFile' <- canonicalizePath projectFile
              let projectRoot = ProjectRootExplicit (takeDirectory projectFile')
                                                    (takeFileName projectFile')
              return (Right projectRoot)
      else return (Left (BadProjectRootExplicitFile projectFile))

findProjectRoot mstartdir mprojectFile = do
    startdir <- maybe getCurrentDirectory canonicalizePath mstartdir
    homedir  <- getHomeDirectory
    probe startdir homedir
  where
    projectFileName = fromMaybe "cabal.project" mprojectFile

    -- Search upwards. If we get to the users home dir or the filesystem root,
    -- then use the current dir
    probe startdir homedir = go startdir
      where
        go dir | isDrive dir || dir == homedir =
          case mprojectFile of
            Nothing   -> return (Right (ProjectRootImplicit startdir))
            Just file -> return (Left (BadProjectRootExplicitFile file))
        go dir = do
          exists <- doesFileExist (dir </> projectFileName)
          if exists
            then return (Right (ProjectRootExplicit dir projectFileName))
            else go (takeDirectory dir)

   --TODO: [nice to have] add compat support for old style sandboxes


-- | Errors returned by 'findProjectRoot'.
--
data BadProjectRoot = BadProjectRootExplicitFile FilePath
#if MIN_VERSION_base(4,8,0)
  deriving (Show, Typeable)
#else
  deriving (Typeable)

instance Show BadProjectRoot where
  show = renderBadProjectRoot
#endif

instance Exception BadProjectRoot where
#if MIN_VERSION_base(4,8,0)
  displayException = renderBadProjectRoot
#endif

renderBadProjectRoot :: BadProjectRoot -> String
renderBadProjectRoot (BadProjectRootExplicitFile projectFile) =
    "The given project file '" ++ projectFile ++ "' does not exist."


-- | Read all the config relevant for a project. This includes the project
-- file if any, plus other global config.
--
readProjectConfig :: Verbosity -> Flag FilePath -> DistDirLayout -> Rebuild ProjectConfig
readProjectConfig verbosity configFileFlag distDirLayout = do
    global <- readGlobalConfig             verbosity configFileFlag
    local  <- readProjectLocalConfig       verbosity distDirLayout
    freeze <- readProjectLocalFreezeConfig verbosity distDirLayout
    extra  <- readProjectLocalExtraConfig  verbosity distDirLayout
    return (global <> local <> freeze <> extra)


-- | Reads an explicit @cabal.project@ file in the given project root dir,
-- or returns the default project config for an implicitly defined project.
--
readProjectLocalConfig :: Verbosity -> DistDirLayout -> Rebuild ProjectConfig
readProjectLocalConfig verbosity DistDirLayout{distProjectFile} = do
  usesExplicitProjectRoot <- liftIO $ doesFileExist projectFile
  if usesExplicitProjectRoot
    then do
      monitorFiles [monitorFileHashed projectFile]
      addProjectFileProvenance <$> liftIO readProjectFile
    else do
      monitorFiles [monitorNonExistentFile projectFile]
      return defaultImplicitProjectConfig

  where
    projectFile = distProjectFile ""
    readProjectFile =
          reportParseResult verbosity "project file" projectFile
        . parseProjectConfig
      =<< readFile projectFile

    addProjectFileProvenance config =
      config {
        projectConfigProvenance =
          Set.insert (Explicit projectFile) (projectConfigProvenance config)
      }

    defaultImplicitProjectConfig :: ProjectConfig
    defaultImplicitProjectConfig =
      mempty {
        -- We expect a package in the current directory.
        projectPackages         = [ "./*.cabal" ],

        -- This is to automatically pick up deps that we unpack locally.
        projectPackagesOptional = [ "./*/*.cabal" ],

        projectConfigProvenance = Set.singleton Implicit
      }

-- | Reads a @cabal.project.local@ file in the given project root dir,
-- or returns empty. This file gets written by @cabal configure@, or in
-- principle can be edited manually or by other tools.
--
readProjectLocalExtraConfig :: Verbosity -> DistDirLayout
                            -> Rebuild ProjectConfig
readProjectLocalExtraConfig verbosity distDirLayout =
    readProjectExtensionFile verbosity distDirLayout "local"
                             "project local configuration file"

-- | Reads a @cabal.project.freeze@ file in the given project root dir,
-- or returns empty. This file gets written by @cabal freeze@, or in
-- principle can be edited manually or by other tools.
--
readProjectLocalFreezeConfig :: Verbosity -> DistDirLayout
                             -> Rebuild ProjectConfig
readProjectLocalFreezeConfig verbosity distDirLayout =
    readProjectExtensionFile verbosity distDirLayout "freeze"
                             "project freeze file"

-- | Reads a named config file in the given project root dir, or returns empty.
--
readProjectExtensionFile :: Verbosity -> DistDirLayout -> String -> FilePath
                         -> Rebuild ProjectConfig
readProjectExtensionFile verbosity DistDirLayout{distProjectFile}
                         extensionName extensionDescription = do
    exists <- liftIO $ doesFileExist extensionFile
    if exists
      then do monitorFiles [monitorFileHashed extensionFile]
              liftIO readExtensionFile
      else do monitorFiles [monitorNonExistentFile extensionFile]
              return mempty
  where
    extensionFile = distProjectFile extensionName

    readExtensionFile =
          reportParseResult verbosity extensionDescription extensionFile
        . parseProjectConfig
      =<< readFile extensionFile


-- | Parse the 'ProjectConfig' format.
--
-- For the moment this is implemented in terms of parsers for legacy
-- configuration types, plus a conversion.
--
parseProjectConfig :: String -> ParseResult ProjectConfig
parseProjectConfig content =
    convertLegacyProjectConfig <$>
      parseLegacyProjectConfig content


-- | Render the 'ProjectConfig' format.
--
-- For the moment this is implemented in terms of a pretty printer for the
-- legacy configuration types, plus a conversion.
--
showProjectConfig :: ProjectConfig -> String
showProjectConfig =
    showLegacyProjectConfig . convertToLegacyProjectConfig


-- | Write a @cabal.project.local@ file in the given project root dir.
--
writeProjectLocalExtraConfig :: DistDirLayout -> ProjectConfig -> IO ()
writeProjectLocalExtraConfig DistDirLayout{distProjectFile} =
    writeProjectConfigFile (distProjectFile "local")


-- | Write a @cabal.project.freeze@ file in the given project root dir.
--
writeProjectLocalFreezeConfig :: DistDirLayout -> ProjectConfig -> IO ()
writeProjectLocalFreezeConfig DistDirLayout{distProjectFile} =
    writeProjectConfigFile (distProjectFile "freeze")


-- | Write in the @cabal.project@ format to the given file.
--
writeProjectConfigFile :: FilePath -> ProjectConfig -> IO ()
writeProjectConfigFile file =
    writeFile file . showProjectConfig


-- | Read the user's @~/.cabal/config@ file.
--
readGlobalConfig :: Verbosity -> Flag FilePath -> Rebuild ProjectConfig
readGlobalConfig verbosity configFileFlag = do
    config     <- liftIO (loadConfig verbosity configFileFlag)
    configFile <- liftIO (getConfigFilePath configFileFlag)
    monitorFiles [monitorFileHashed configFile]
    return (convertLegacyGlobalConfig config)

reportParseResult :: Verbosity -> String -> FilePath -> ParseResult a -> IO a
reportParseResult verbosity _filetype filename (ParseOk warnings x) = do
    unless (null warnings) $
      let msg = unlines (map (showPWarning filename) warnings)
       in warn verbosity msg
    return x
reportParseResult verbosity filetype filename (ParseFailed err) =
    let (line, msg) = locatedErrorMsg err
     in die' verbosity $ "Error parsing " ++ filetype ++ " " ++ filename
           ++ maybe "" (\n -> ':' : show n) line ++ ":\n" ++ msg


---------------------------------------------
-- Reading packages in the project
--

-- | The location of a package as part of a project. Local file paths are
-- either absolute (if the user specified it as such) or they are relative
-- to the project root.
--
data ProjectPackageLocation =
     ProjectPackageLocalCabalFile FilePath
   | ProjectPackageLocalDirectory FilePath FilePath -- dir and .cabal file
   | ProjectPackageLocalTarball   FilePath
   | ProjectPackageRemoteTarball  URI
   | ProjectPackageRemoteRepo     SourceRepo
   | ProjectPackageNamed          Dependency
  deriving Show


-- | Exception thrown by 'findProjectPackages'.
--
data BadPackageLocations
   = BadPackageLocations (Set ProjectConfigProvenance) [BadPackageLocation]
#if MIN_VERSION_base(4,8,0)
  deriving (Show, Typeable)
#else
  deriving (Typeable)

instance Show BadPackageLocations where
  show = renderBadPackageLocations
#endif

instance Exception BadPackageLocations where
#if MIN_VERSION_base(4,8,0)
  displayException = renderBadPackageLocations
#endif
--TODO: [nice to have] custom exception subclass for Doc rendering, colour etc

data BadPackageLocation
   = BadPackageLocationFile    BadPackageLocationMatch
   | BadLocGlobEmptyMatch      String
   | BadLocGlobBadMatches      String [BadPackageLocationMatch]
   | BadLocUnexpectedUriScheme String
   | BadLocUnrecognisedUri     String
   | BadLocUnrecognised        String
  deriving Show

data BadPackageLocationMatch
   = BadLocUnexpectedFile      String
   | BadLocNonexistantFile     String
   | BadLocDirNoCabalFile      String
   | BadLocDirManyCabalFiles   String
  deriving Show

renderBadPackageLocations :: BadPackageLocations -> String
renderBadPackageLocations (BadPackageLocations provenance bpls)
      -- There is no provenance information,
      -- render standard bad package error information.
    | Set.null provenance = renderErrors renderBadPackageLocation

      -- The configuration is implicit, render bad package locations
      -- using possibly specialized error messages.
    | Set.singleton Implicit == provenance =
        renderErrors renderImplicitBadPackageLocation

      -- The configuration contains both implicit and explicit provenance.
      -- This should not occur, and a message is output to assist debugging.
    | Implicit `Set.member` provenance =
           "Warning: both implicit and explicit configuration is present."
        ++ renderExplicit

      -- The configuration was read from one or more explicit path(s),
      -- list the locations and render the bad package error information.
      -- The intent is to supersede this with the relevant location information
      -- per package error.
    | otherwise = renderExplicit
  where
    renderErrors f = unlines (map f bpls)

    renderExplicit =
           "When using configuration(s) from "
        ++ intercalate ", " (mapMaybe getExplicit (Set.toList provenance))
        ++ ", the following errors occurred:\n"
        ++ renderErrors renderBadPackageLocation

    getExplicit (Explicit path) = Just path
    getExplicit Implicit        = Nothing

--TODO: [nice to have] keep track of the config file (and src loc) packages
-- were listed, to use in error messages

-- | Render bad package location error information for the implicit
-- @cabal.project@ configuration.
--
-- TODO: This is currently not fully realized, with only one of the implicit
-- cases handled. More cases should be added with informative help text
-- about the issues related specifically when having no project configuration
-- is present.
renderImplicitBadPackageLocation :: BadPackageLocation -> String
renderImplicitBadPackageLocation bpl = case bpl of
    BadLocGlobEmptyMatch pkglocstr ->
        "No cabal.project file or cabal file matching the default glob '"
     ++ pkglocstr ++ "' was found.\n"
     ++ "Please create a package description file <pkgname>.cabal "
     ++ "or a cabal.project file referencing the packages you "
     ++ "want to build."
    _ -> renderBadPackageLocation bpl

renderBadPackageLocation :: BadPackageLocation -> String
renderBadPackageLocation bpl = case bpl of
    BadPackageLocationFile badmatch ->
        renderBadPackageLocationMatch badmatch
    BadLocGlobEmptyMatch pkglocstr ->
        "The package location glob '" ++ pkglocstr
     ++ "' does not match any files or directories."
    BadLocGlobBadMatches pkglocstr failures ->
        "The package location glob '" ++ pkglocstr ++ "' does not match any "
     ++ "recognised forms of package. "
     ++ concatMap ((' ':) . renderBadPackageLocationMatch) failures
    BadLocUnexpectedUriScheme pkglocstr ->
        "The package location URI '" ++ pkglocstr ++ "' does not use a "
     ++ "supported URI scheme. The supported URI schemes are http, https and "
     ++ "file."
    BadLocUnrecognisedUri pkglocstr ->
        "The package location URI '" ++ pkglocstr ++ "' does not appear to "
     ++ "be a valid absolute URI."
    BadLocUnrecognised pkglocstr ->
        "The package location syntax '" ++ pkglocstr ++ "' is not recognised."

renderBadPackageLocationMatch :: BadPackageLocationMatch -> String
renderBadPackageLocationMatch bplm = case bplm of
    BadLocUnexpectedFile pkglocstr ->
        "The package location '" ++ pkglocstr ++ "' is not recognised. The "
     ++ "supported file targets are .cabal files, .tar.gz tarballs or package "
     ++ "directories (i.e. directories containing a .cabal file)."
    BadLocNonexistantFile pkglocstr ->
        "The package location '" ++ pkglocstr ++ "' does not exist."
    BadLocDirNoCabalFile pkglocstr ->
        "The package directory '" ++ pkglocstr ++ "' does not contain any "
     ++ ".cabal file."
    BadLocDirManyCabalFiles pkglocstr ->
        "The package directory '" ++ pkglocstr ++ "' contains multiple "
     ++ ".cabal files (which is not currently supported)."

-- | Given the project config,
--
-- Throws 'BadPackageLocations'.
--
findProjectPackages :: DistDirLayout -> ProjectConfig
                    -> Rebuild [ProjectPackageLocation]
findProjectPackages DistDirLayout{distProjectRootDirectory}
                    ProjectConfig{..} = do

    requiredPkgs <- findPackageLocations True    projectPackages
    optionalPkgs <- findPackageLocations False   projectPackagesOptional
    let repoPkgs  = map ProjectPackageRemoteRepo projectPackagesRepo
        namedPkgs = map ProjectPackageNamed      projectPackagesNamed

    return (concat [requiredPkgs, optionalPkgs, repoPkgs, namedPkgs])
  where
    findPackageLocations required pkglocstr = do
      (problems, pkglocs) <-
        partitionEithers <$> mapM (findPackageLocation required) pkglocstr
      unless (null problems) $
        liftIO $ throwIO $ BadPackageLocations projectConfigProvenance problems
      return (concat pkglocs)


    findPackageLocation :: Bool -> String
                        -> Rebuild (Either BadPackageLocation
                                          [ProjectPackageLocation])
    findPackageLocation _required@True pkglocstr =
      -- strategy: try first as a file:// or http(s):// URL.
      -- then as a file glob (usually encompassing single file)
      -- finally as a single file, for files that fail to parse as globs
                    checkIsUriPackage pkglocstr
      `mplusMaybeT` checkIsFileGlobPackage pkglocstr
      `mplusMaybeT` checkIsSingleFilePackage pkglocstr
      >>= maybe (return (Left (BadLocUnrecognised pkglocstr))) return


    findPackageLocation _required@False pkglocstr = do
      -- just globs for optional case
      res <- checkIsFileGlobPackage pkglocstr
      case res of
        Nothing              -> return (Left (BadLocUnrecognised pkglocstr))
        Just (Left _)        -> return (Right []) -- it's optional
        Just (Right pkglocs) -> return (Right pkglocs)


    checkIsUriPackage, checkIsFileGlobPackage, checkIsSingleFilePackage
      :: String -> Rebuild (Maybe (Either BadPackageLocation
                                         [ProjectPackageLocation]))
    checkIsUriPackage pkglocstr =
      case parseAbsoluteURI pkglocstr of
        Just uri@URI {
            uriScheme    = scheme,
            uriAuthority = Just URIAuth { uriRegName = host },
            uriPath      = path,
            uriQuery     = query,
            uriFragment  = frag
          }
          | recognisedScheme && not (null host) ->
            return (Just (Right [ProjectPackageRemoteTarball uri]))

          | scheme == "file:" && null host && null query && null frag ->
            checkIsSingleFilePackage path

          | not recognisedScheme && not (null host) ->
            return (Just (Left (BadLocUnexpectedUriScheme pkglocstr)))

          | recognisedScheme && null host ->
            return (Just (Left (BadLocUnrecognisedUri pkglocstr)))
          where
            recognisedScheme = scheme == "http:" || scheme == "https:"
                            || scheme == "file:"

        _ -> return Nothing


    checkIsFileGlobPackage pkglocstr =
      case simpleParse pkglocstr of
        Nothing   -> return Nothing
        Just glob -> liftM Just $ do
          matches <- matchFileGlob glob
          case matches of
            [] | isJust (isTrivialFilePathGlob glob)
               -> return (Left (BadPackageLocationFile
                                  (BadLocNonexistantFile pkglocstr)))

            [] -> return (Left (BadLocGlobEmptyMatch pkglocstr))

            _  -> do
              (failures, pkglocs) <- partitionEithers <$>
                                     mapM checkFilePackageMatch matches
              return $! case (failures, pkglocs) of
                ([failure], []) | isJust (isTrivialFilePathGlob glob)
                        -> Left (BadPackageLocationFile failure)
                (_, []) -> Left (BadLocGlobBadMatches pkglocstr failures)
                _       -> Right pkglocs


    checkIsSingleFilePackage pkglocstr = do
      let filename = distProjectRootDirectory </> pkglocstr
      isFile <- liftIO $ doesFileExist filename
      isDir  <- liftIO $ doesDirectoryExist filename
      if isFile || isDir
        then checkFilePackageMatch pkglocstr
         >>= either (return . Just . Left  . BadPackageLocationFile)
                    (return . Just . Right . (\x->[x]))
        else return Nothing


    checkFilePackageMatch :: String -> Rebuild (Either BadPackageLocationMatch
                                                       ProjectPackageLocation)
    checkFilePackageMatch pkglocstr = do
      -- The pkglocstr may be absolute or may be relative to the project root.
      -- Either way, </> does the right thing here. We return relative paths if
      -- they were relative in the first place.
      let abspath = distProjectRootDirectory </> pkglocstr
      isFile <- liftIO $ doesFileExist abspath
      isDir  <- liftIO $ doesDirectoryExist abspath
      parentDirExists <- case takeDirectory abspath of
                           []  -> return False
                           dir -> liftIO $ doesDirectoryExist dir
      case () of
        _ | isDir
         -> do matches <- matchFileGlob (globStarDotCabal pkglocstr)
               case matches of
                 [cabalFile]
                     -> return (Right (ProjectPackageLocalDirectory
                                         pkglocstr cabalFile))
                 []  -> return (Left (BadLocDirNoCabalFile pkglocstr))
                 _   -> return (Left (BadLocDirManyCabalFiles pkglocstr))

          | extensionIsTarGz pkglocstr
         -> return (Right (ProjectPackageLocalTarball pkglocstr))

          | takeExtension pkglocstr == ".cabal"
         -> return (Right (ProjectPackageLocalCabalFile pkglocstr))

          | isFile
         -> return (Left (BadLocUnexpectedFile pkglocstr))

          | parentDirExists
         -> return (Left (BadLocNonexistantFile pkglocstr))

          | otherwise
         -> return (Left (BadLocUnexpectedFile pkglocstr))


    extensionIsTarGz f = takeExtension f                 == ".gz"
                      && takeExtension (dropExtension f) == ".tar"


-- | A glob to find all the cabal files in a directory.
--
-- For a directory @some/dir/@, this is a glob of the form @some/dir/\*.cabal@.
-- The directory part can be either absolute or relative.
--
globStarDotCabal :: FilePath -> FilePathGlob
globStarDotCabal dir =
    FilePathGlob
      (if isAbsolute dir then FilePathRoot root else FilePathRelative)
      (foldr (\d -> GlobDir [Literal d])
             (GlobFile [WildCard, Literal ".cabal"]) dirComponents)
  where
    (root, dirComponents) = fmap splitDirectories (splitDrive dir)


--TODO: [code cleanup] use sufficiently recent transformers package
mplusMaybeT :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
mplusMaybeT ma mb = do
  mx <- ma
  case mx of
    Nothing -> mb
    Just x  -> return (Just x)


-- | Read the @.cabal@ file of the given package.
--
-- Note here is where we convert from project-root relative paths to absolute
-- paths.
--
readSourcePackage :: Verbosity -> ProjectPackageLocation
                  -> Rebuild (PackageSpecifier UnresolvedSourcePackage)
readSourcePackage verbosity (ProjectPackageLocalCabalFile cabalFile) =
    readSourcePackage verbosity (ProjectPackageLocalDirectory dir cabalFile)
  where
    dir = takeDirectory cabalFile

readSourcePackage verbosity (ProjectPackageLocalDirectory dir cabalFile) = do
    monitorFiles [monitorFileHashed cabalFile]
    root <- askRoot
    pkgdesc <- liftIO $ readGenericPackageDescription verbosity (root </> cabalFile)
    return $ SpecificSourcePackage SourcePackage {
      packageInfoId        = packageId pkgdesc,
      packageDescription   = pkgdesc,
      packageSource        = LocalUnpackedPackage (root </> dir),
      packageDescrOverride = Nothing
    }

readSourcePackage _ (ProjectPackageNamed (Dependency pkgname verrange)) =
    return $ NamedPackage pkgname [PackagePropertyVersion verrange]

readSourcePackage _verbosity _ =
    fail $ "TODO: add support for fetching and reading local tarballs, remote "
        ++ "tarballs, remote repos and passing named packages through"


-- TODO: add something like this, here or in the project planning
-- Based on the package location, which packages will be built inplace in the
-- build tree vs placed in the store. This has various implications on what we
-- can do with the package, e.g. can we run tests, ghci etc.
--
-- packageIsLocalToProject :: ProjectPackageLocation -> Bool


---------------------------------------------
-- Checking configuration sanity
--

data BadPerPackageCompilerPaths
   = BadPerPackageCompilerPaths [(PackageName, String)]
#if MIN_VERSION_base(4,8,0)
  deriving (Show, Typeable)
#else
  deriving (Typeable)

instance Show BadPerPackageCompilerPaths where
  show = renderBadPerPackageCompilerPaths
#endif

instance Exception BadPerPackageCompilerPaths where
#if MIN_VERSION_base(4,8,0)
  displayException = renderBadPerPackageCompilerPaths
#endif
--TODO: [nice to have] custom exception subclass for Doc rendering, colour etc

renderBadPerPackageCompilerPaths :: BadPerPackageCompilerPaths -> String
renderBadPerPackageCompilerPaths
  (BadPerPackageCompilerPaths ((pkgname, progname) : _)) =
    "The path to the compiler program (or programs used by the compiler) "
 ++ "cannot be specified on a per-package basis in the cabal.project file "
 ++ "(i.e. setting the '" ++ progname ++ "-location' for package '"
 ++ display pkgname ++ "'). All packages have to use the same compiler, so "
 ++ "specify the path in a global 'program-locations' section."
 --TODO: [nice to have] better format control so we can pretty-print the
 -- offending part of the project file. Currently the line wrapping breaks any
 -- formatting.
renderBadPerPackageCompilerPaths _ = error "renderBadPerPackageCompilerPaths"

-- | The project configuration is not allowed to specify program locations for
-- programs used by the compiler as these have to be the same for each set of
-- packages.
--
-- We cannot check this until we know which programs the compiler uses, which
-- in principle is not until we've configured the compiler.
--
-- Throws 'BadPerPackageCompilerPaths'
--
checkBadPerPackageCompilerPaths :: [ConfiguredProgram]
                                -> Map PackageName PackageConfig
                                -> IO ()
checkBadPerPackageCompilerPaths compilerPrograms packagesConfig =
    case [ (pkgname, progname)
         | let compProgNames = Set.fromList (map programId compilerPrograms)
         ,  (pkgname, pkgconf) <- Map.toList packagesConfig
         , progname <- Map.keys (getMapLast (packageConfigProgramPaths pkgconf))
         , progname `Set.member` compProgNames ] of
      [] -> return ()
      ps -> throwIO (BadPerPackageCompilerPaths ps)

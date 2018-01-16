{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox.PackageEnvironment
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for working with the package environment file. Patterned after
-- Distribution.Client.Config.
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox.PackageEnvironment (
    PackageEnvironment(..)
  , PackageEnvironmentType(..)
  , classifyPackageEnvironment
  , createPackageEnvironmentFile
  , tryLoadSandboxPackageEnvironmentFile
  , readPackageEnvironmentFile
  , showPackageEnvironment
  , showPackageEnvironmentWithComments
  , setPackageDB
  , sandboxPackageDBPath
  , loadUserConfig

  , basePackageEnvironment
  , initialPackageEnvironment
  , commentPackageEnvironment
  , sandboxPackageEnvironmentFile
  , userPackageEnvironmentFile
  ) where

import Distribution.Client.Config      ( SavedConfig(..), commentSavedConfig
                                       , loadConfig, configFieldDescriptions
                                       , haddockFlagsFields
                                       , installDirsFields, withProgramsFields
                                       , withProgramOptionsFields
                                       , defaultCompiler )
import Distribution.Client.ParseUtils  ( parseFields, ppFields, ppSection )
import Distribution.Client.Setup       ( GlobalFlags(..), ConfigExFlags(..)
                                       , InstallFlags(..)
                                       , defaultSandboxLocation )
import Distribution.Client.Targets     ( userConstraintPackageName )
import Distribution.Utils.NubList      ( toNubList )
import Distribution.Simple.Compiler    ( Compiler, PackageDB(..)
                                       , compilerFlavor, showCompilerIdWithAbi )
import Distribution.Simple.InstallDirs ( InstallDirs(..), PathTemplate
                                       , defaultInstallDirs, combineInstallDirs
                                       , fromPathTemplate, toPathTemplate )
import Distribution.Simple.Setup       ( Flag(..)
                                       , ConfigFlags(..), HaddockFlags(..)
                                       , fromFlagOrDefault, toFlag, flagToMaybe )
import Distribution.Simple.Utils       ( die', info, notice, warn, debug )
import Distribution.Solver.Types.ConstraintSource
import Distribution.ParseUtils         ( FieldDescr(..), ParseResult(..)
                                       , commaListField, commaNewLineListField
                                       , liftField, lineNo, locatedErrorMsg
                                       , parseFilePathQ, readFields
                                       , showPWarning, simpleField
                                       , syntaxError, warning )
import Distribution.System             ( Platform )
import Distribution.Verbosity          ( Verbosity, normal )
import Control.Monad                   ( foldM, liftM2, unless )
import Data.List                       ( partition, sortBy )
import Data.Maybe                      ( isJust )
import Data.Ord                        ( comparing )
import Distribution.Compat.Exception   ( catchIO )
import Distribution.Compat.Semigroup
import System.Directory                ( doesDirectoryExist, doesFileExist
                                       , renameFile )
import System.FilePath                 ( (<.>), (</>), takeDirectory )
import System.IO.Error                 ( isDoesNotExistError )
import Text.PrettyPrint                ( ($+$) )

import qualified Text.PrettyPrint          as Disp
import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.ParseUtils   as ParseUtils ( Field(..) )
import qualified Distribution.Text         as Text
import GHC.Generics ( Generic )


--
-- * Configuration saved in the package environment file
--

-- TODO: would be nice to remove duplication between
-- D.C.Sandbox.PackageEnvironment and D.C.Config.
data PackageEnvironment = PackageEnvironment {
  -- The 'inherit' feature is not used ATM, but could be useful in the future
  -- for constructing nested sandboxes (see discussion in #1196).
  pkgEnvInherit       :: Flag FilePath,
  pkgEnvSavedConfig   :: SavedConfig
} deriving Generic

instance Monoid PackageEnvironment where
  mempty = gmempty
  mappend = (<>)

instance Semigroup PackageEnvironment where
  (<>) = gmappend

-- | The automatically-created package environment file that should not be
-- touched by the user.
sandboxPackageEnvironmentFile :: FilePath
sandboxPackageEnvironmentFile = "cabal.sandbox.config"

-- | Optional package environment file that can be used to customize the default
-- settings. Created by the user.
userPackageEnvironmentFile :: FilePath
userPackageEnvironmentFile = "cabal.config"

-- | Type of the current package environment.
data PackageEnvironmentType =
  SandboxPackageEnvironment   -- ^ './cabal.sandbox.config'
  | UserPackageEnvironment    -- ^ './cabal.config'
  | AmbientPackageEnvironment -- ^ '~/.cabal/config'

-- | Is there a 'cabal.sandbox.config' or 'cabal.config' in this
-- directory?
classifyPackageEnvironment :: FilePath -> Flag FilePath -> Flag Bool
                              -> IO PackageEnvironmentType
classifyPackageEnvironment pkgEnvDir sandboxConfigFileFlag ignoreSandboxFlag =
  do isSandbox <- liftM2 (||) (return forceSandboxConfig)
                  (configExists sandboxPackageEnvironmentFile)
     isUser    <- configExists userPackageEnvironmentFile
     return (classify isSandbox isUser)
  where
    configExists fname   = doesFileExist (pkgEnvDir </> fname)
    ignoreSandbox        = fromFlagOrDefault False ignoreSandboxFlag
    forceSandboxConfig   = isJust . flagToMaybe $ sandboxConfigFileFlag

    classify :: Bool -> Bool -> PackageEnvironmentType
    classify True _
      | not ignoreSandbox = SandboxPackageEnvironment
    classify _    True    = UserPackageEnvironment
    classify _    False   = AmbientPackageEnvironment

-- | Defaults common to 'initialPackageEnvironment' and
-- 'commentPackageEnvironment'.
commonPackageEnvironmentConfig :: FilePath -> SavedConfig
commonPackageEnvironmentConfig sandboxDir =
  mempty {
    savedConfigureFlags = mempty {
       -- TODO: Currently, we follow cabal-dev and set 'user-install: False' in
       -- the config file. In the future we may want to distinguish between
       -- global, sandbox and user install types.
       configUserInstall = toFlag False,
       configInstallDirs = installDirs
       },
    savedUserInstallDirs   = installDirs,
    savedGlobalInstallDirs = installDirs,
    savedGlobalFlags = mempty {
      globalLogsDir = toFlag $ sandboxDir </> "logs",
      -- Is this right? cabal-dev uses the global world file.
      globalWorldFile = toFlag $ sandboxDir </> "world"
      }
    }
  where
    installDirs = sandboxInstallDirs sandboxDir

-- | 'commonPackageEnvironmentConfig' wrapped inside a 'PackageEnvironment'.
commonPackageEnvironment :: FilePath -> PackageEnvironment
commonPackageEnvironment sandboxDir = mempty {
  pkgEnvSavedConfig = commonPackageEnvironmentConfig sandboxDir
  }

-- | Given a path to a sandbox, return the corresponding InstallDirs record.
sandboxInstallDirs :: FilePath -> InstallDirs (Flag PathTemplate)
sandboxInstallDirs sandboxDir = mempty {
  prefix = toFlag (toPathTemplate sandboxDir)
  }

-- | These are the absolute basic defaults, the fields that must be
-- initialised. When we load the package environment from the file we layer the
-- loaded values over these ones.
basePackageEnvironment :: PackageEnvironment
basePackageEnvironment =
    mempty {
      pkgEnvSavedConfig = mempty {
         savedConfigureFlags = mempty {
            configHcFlavor    = toFlag defaultCompiler,
            configVerbosity   = toFlag normal
            }
         }
      }

-- | Initial configuration that we write out to the package environment file if
-- it does not exist. When the package environment gets loaded this
-- configuration gets layered on top of 'basePackageEnvironment'.
initialPackageEnvironment :: FilePath -> Compiler -> Platform
                             -> IO PackageEnvironment
initialPackageEnvironment sandboxDir compiler platform = do
  defInstallDirs <- defaultInstallDirs (compilerFlavor compiler)
                    {- userInstall= -} False {- _hasLibs= -} False
  let initialConfig = commonPackageEnvironmentConfig sandboxDir
      installDirs   = combineInstallDirs (\d f -> Flag $ fromFlagOrDefault d f)
                      defInstallDirs (savedUserInstallDirs initialConfig)
  return $ mempty {
    pkgEnvSavedConfig = initialConfig {
       savedUserInstallDirs   = installDirs,
       savedGlobalInstallDirs = installDirs,
       savedGlobalFlags = (savedGlobalFlags initialConfig) {
          globalLocalRepos = toNubList [sandboxDir </> "packages"]
          },
       savedConfigureFlags = setPackageDB sandboxDir compiler platform
                             (savedConfigureFlags initialConfig),
       savedInstallFlags = (savedInstallFlags initialConfig) {
         installSummaryFile = toNubList [toPathTemplate (sandboxDir </>
                                               "logs" </> "build.log")]
         }
       }
    }

-- | Return the path to the sandbox package database.
sandboxPackageDBPath :: FilePath -> Compiler -> Platform -> String
sandboxPackageDBPath sandboxDir compiler platform =
    sandboxDir
         </> (Text.display platform ++ "-"
             ++ showCompilerIdWithAbi compiler
             ++ "-packages.conf.d")
-- The path in sandboxPackageDBPath should be kept in sync with the
-- path in the bootstrap.sh which is used to bootstrap cabal-install
-- into a sandbox.

-- | Use the package DB location specific for this compiler.
setPackageDB :: FilePath -> Compiler -> Platform -> ConfigFlags -> ConfigFlags
setPackageDB sandboxDir compiler platform configFlags =
  configFlags {
    configPackageDBs = [Just (SpecificPackageDB $ sandboxPackageDBPath
                                                      sandboxDir
                                                      compiler
                                                      platform)]
    }

-- | Almost the same as 'savedConf `mappend` pkgEnv', but some settings are
-- overridden instead of mappend'ed.
overrideSandboxSettings :: PackageEnvironment -> PackageEnvironment ->
                           PackageEnvironment
overrideSandboxSettings pkgEnv0 pkgEnv =
  pkgEnv {
    pkgEnvSavedConfig = mappendedConf {
         savedConfigureFlags = (savedConfigureFlags mappendedConf) {
          configPackageDBs = configPackageDBs pkgEnvConfigureFlags
          }
       , savedInstallFlags = (savedInstallFlags mappendedConf) {
          installSummaryFile = installSummaryFile pkgEnvInstallFlags
          }
       },
    pkgEnvInherit = pkgEnvInherit pkgEnv0
    }
  where
    pkgEnvConf           = pkgEnvSavedConfig pkgEnv
    mappendedConf        = (pkgEnvSavedConfig pkgEnv0) `mappend` pkgEnvConf
    pkgEnvConfigureFlags = savedConfigureFlags pkgEnvConf
    pkgEnvInstallFlags   = savedInstallFlags pkgEnvConf

-- | Default values that get used if no value is given. Used here to include in
-- comments when we write out the initial package environment.
commentPackageEnvironment :: FilePath -> IO PackageEnvironment
commentPackageEnvironment sandboxDir = do
  commentConf  <- commentSavedConfig
  let baseConf =  commonPackageEnvironmentConfig sandboxDir
  return $ mempty {
    pkgEnvSavedConfig = commentConf `mappend` baseConf
    }

-- | If this package environment inherits from some other package environment,
-- return that package environment; otherwise return mempty.
inheritedPackageEnvironment :: Verbosity -> PackageEnvironment
                               -> IO PackageEnvironment
inheritedPackageEnvironment verbosity pkgEnv = do
  case (pkgEnvInherit pkgEnv) of
    NoFlag                -> return mempty
    confPathFlag@(Flag _) -> do
      conf <- loadConfig verbosity confPathFlag
      return $ mempty { pkgEnvSavedConfig = conf }

-- | Load the user package environment if it exists (the optional "cabal.config"
-- file). If it does not exist locally, attempt to load an optional global one.
userPackageEnvironment :: Verbosity -> FilePath -> Maybe FilePath
                       -> IO PackageEnvironment
userPackageEnvironment verbosity pkgEnvDir globalConfigLocation = do
    let path = pkgEnvDir </> userPackageEnvironmentFile
    minp <- readPackageEnvironmentFile (ConstraintSourceUserConfig path)
            mempty path
    case (minp, globalConfigLocation) of
      (Just parseRes, _)  -> processConfigParse path parseRes
      (_, Just globalLoc) -> do
        minp' <- readPackageEnvironmentFile (ConstraintSourceUserConfig globalLoc)
                 mempty globalLoc
        maybe (warn verbosity ("no constraints file found at " ++ globalLoc)
               >> return mempty)
          (processConfigParse globalLoc)
          minp'
      _ -> do
        debug verbosity ("no user package environment file found at " ++ pkgEnvDir)
        return mempty
  where
    processConfigParse path (ParseOk warns parseResult) = do
      unless (null warns) $ warn verbosity $
        unlines (map (showPWarning path) warns)
      return parseResult
    processConfigParse path (ParseFailed err) = do
      let (line, msg) = locatedErrorMsg err
      warn verbosity $ "Error parsing package environment file " ++ path
        ++ maybe "" (\n -> ":" ++ show n) line ++ ":\n" ++ msg
      return mempty

-- | Same as @userPackageEnvironmentFile@, but returns a SavedConfig.
loadUserConfig :: Verbosity -> FilePath -> Maybe FilePath -> IO SavedConfig
loadUserConfig verbosity pkgEnvDir globalConfigLocation =
    fmap pkgEnvSavedConfig $
    userPackageEnvironment verbosity pkgEnvDir globalConfigLocation

-- | Common error handling code used by 'tryLoadSandboxPackageEnvironment' and
-- 'updatePackageEnvironment'.
handleParseResult :: Verbosity -> FilePath
                     -> Maybe (ParseResult PackageEnvironment)
                     -> IO PackageEnvironment
handleParseResult verbosity path minp =
  case minp of
    Nothing -> die' verbosity $
      "The package environment file '" ++ path ++ "' doesn't exist"
    Just (ParseOk warns parseResult) -> do
      unless (null warns) $ warn verbosity $
        unlines (map (showPWarning path) warns)
      return parseResult
    Just (ParseFailed err) -> do
      let (line, msg) = locatedErrorMsg err
      die' verbosity $ "Error parsing package environment file " ++ path
        ++ maybe "" (\n -> ":" ++ show n) line ++ ":\n" ++ msg

-- | Try to load the given package environment file, exiting with error if it
-- doesn't exist. Also returns the path to the sandbox directory. The path
-- parameter should refer to an existing file.
tryLoadSandboxPackageEnvironmentFile :: Verbosity -> FilePath -> (Flag FilePath)
                                        -> IO (FilePath, PackageEnvironment)
tryLoadSandboxPackageEnvironmentFile verbosity pkgEnvFile configFileFlag = do
  let pkgEnvDir = takeDirectory pkgEnvFile
  minp   <- readPackageEnvironmentFile
            (ConstraintSourceSandboxConfig pkgEnvFile) mempty pkgEnvFile
  pkgEnv <- handleParseResult verbosity pkgEnvFile minp

  -- Get the saved sandbox directory.
  -- TODO: Use substPathTemplate with
  -- compilerTemplateEnv ++ platformTemplateEnv ++ abiTemplateEnv.
  let sandboxDir = fromFlagOrDefault defaultSandboxLocation
                   . fmap fromPathTemplate . prefix . savedUserInstallDirs
                   . pkgEnvSavedConfig $ pkgEnv

  -- Do some sanity checks
  dirExists            <- doesDirectoryExist sandboxDir
  -- TODO: Also check for an initialised package DB?
  unless dirExists $
    die' verbosity ("No sandbox exists at " ++ sandboxDir)
  info verbosity $ "Using a sandbox located at " ++ sandboxDir

  let base   = basePackageEnvironment
  let common = commonPackageEnvironment sandboxDir
  user      <- userPackageEnvironment verbosity pkgEnvDir Nothing --TODO
  inherited <- inheritedPackageEnvironment verbosity user

  -- Layer the package environment settings over settings from ~/.cabal/config.
  cabalConfig <- fmap unsetSymlinkBinDir $ loadConfig verbosity configFileFlag
  return (sandboxDir,
          updateInstallDirs $
          (base `mappend` (toPkgEnv cabalConfig) `mappend`
           common `mappend` inherited `mappend` user)
          `overrideSandboxSettings` pkgEnv)
    where
      toPkgEnv config = mempty { pkgEnvSavedConfig = config }

      updateInstallDirs pkgEnv =
        let config         = pkgEnvSavedConfig    pkgEnv
            configureFlags = savedConfigureFlags  config
            installDirs    = savedUserInstallDirs config
        in pkgEnv {
          pkgEnvSavedConfig = config {
             savedConfigureFlags = configureFlags {
                configInstallDirs = installDirs
                }
             }
          }

      -- We don't want to inherit the value of 'symlink-bindir' from
      -- '~/.cabal/config'. See #1514.
      unsetSymlinkBinDir config =
        let installFlags = savedInstallFlags config
        in config {
          savedInstallFlags = installFlags {
             installSymlinkBinDir = NoFlag
             }
          }

-- | Create a new package environment file, replacing the existing one if it
-- exists. Note that the path parameters should point to existing directories.
createPackageEnvironmentFile :: Verbosity -> FilePath -> FilePath
                                -> Compiler
                                -> Platform
                                -> IO ()
createPackageEnvironmentFile verbosity sandboxDir pkgEnvFile compiler platform = do
  notice verbosity $ "Writing a default package environment file to " ++ pkgEnvFile
  initialPkgEnv <- initialPackageEnvironment sandboxDir compiler platform
  writePackageEnvironmentFile pkgEnvFile initialPkgEnv

-- | Descriptions of all fields in the package environment file.
pkgEnvFieldDescrs :: ConstraintSource -> [FieldDescr PackageEnvironment]
pkgEnvFieldDescrs src = [
  simpleField "inherit"
    (fromFlagOrDefault Disp.empty . fmap Disp.text) (optional parseFilePathQ)
    pkgEnvInherit (\v pkgEnv -> pkgEnv { pkgEnvInherit = v })

  , commaNewLineListField "constraints"
    (Text.disp . fst) ((\pc -> (pc, src)) `fmap` Text.parse)
    (sortConstraints . configExConstraints
     . savedConfigureExFlags . pkgEnvSavedConfig)
    (\v pkgEnv -> updateConfigureExFlags pkgEnv
                  (\flags -> flags { configExConstraints = v }))

  , commaListField "preferences"
    Text.disp Text.parse
    (configPreferences . savedConfigureExFlags . pkgEnvSavedConfig)
    (\v pkgEnv -> updateConfigureExFlags pkgEnv
                  (\flags -> flags { configPreferences = v }))
  ]
  ++ map toPkgEnv configFieldDescriptions'
  where
    optional = Parse.option mempty . fmap toFlag

    configFieldDescriptions' :: [FieldDescr SavedConfig]
    configFieldDescriptions' = filter
      (\(FieldDescr name _ _) -> name /= "preference" && name /= "constraint")
      (configFieldDescriptions src)

    toPkgEnv :: FieldDescr SavedConfig -> FieldDescr PackageEnvironment
    toPkgEnv fieldDescr =
      liftField pkgEnvSavedConfig
      (\savedConfig pkgEnv -> pkgEnv { pkgEnvSavedConfig = savedConfig})
      fieldDescr

    updateConfigureExFlags :: PackageEnvironment
                              -> (ConfigExFlags -> ConfigExFlags)
                              -> PackageEnvironment
    updateConfigureExFlags pkgEnv f = pkgEnv {
      pkgEnvSavedConfig = (pkgEnvSavedConfig pkgEnv) {
         savedConfigureExFlags = f . savedConfigureExFlags . pkgEnvSavedConfig
                                 $ pkgEnv
         }
      }

    sortConstraints = sortBy (comparing $ userConstraintPackageName . fst)

-- | Read the package environment file.
readPackageEnvironmentFile :: ConstraintSource -> PackageEnvironment -> FilePath
                              -> IO (Maybe (ParseResult PackageEnvironment))
readPackageEnvironmentFile src initial file =
  handleNotExists $
  fmap (Just . parsePackageEnvironment src initial) (readFile file)
  where
    handleNotExists action = catchIO action $ \ioe ->
      if isDoesNotExistError ioe
        then return Nothing
        else ioError ioe

-- | Parse the package environment file.
parsePackageEnvironment :: ConstraintSource -> PackageEnvironment -> String
                           -> ParseResult PackageEnvironment
parsePackageEnvironment src initial str = do
  fields <- readFields str
  let (knownSections, others) = partition isKnownSection fields
  pkgEnv <- parse others
  let config       = pkgEnvSavedConfig pkgEnv
      installDirs0 = savedUserInstallDirs config
  (haddockFlags, installDirs, paths, args) <-
    foldM parseSections
    (savedHaddockFlags config, installDirs0, [], [])
    knownSections
  return pkgEnv {
    pkgEnvSavedConfig = config {
       savedConfigureFlags    = (savedConfigureFlags config) {
          configProgramPaths  = paths,
          configProgramArgs   = args
          },
       savedHaddockFlags      = haddockFlags,
       savedUserInstallDirs   = installDirs,
       savedGlobalInstallDirs = installDirs
       }
    }

  where
    isKnownSection :: ParseUtils.Field -> Bool
    isKnownSection (ParseUtils.Section _ "haddock" _ _)                 = True
    isKnownSection (ParseUtils.Section _ "install-dirs" _ _)            = True
    isKnownSection (ParseUtils.Section _ "program-locations" _ _)       = True
    isKnownSection (ParseUtils.Section _ "program-default-options" _ _) = True
    isKnownSection _                                                    = False

    parse :: [ParseUtils.Field] -> ParseResult PackageEnvironment
    parse = parseFields (pkgEnvFieldDescrs src) initial

    parseSections :: SectionsAccum -> ParseUtils.Field
                     -> ParseResult SectionsAccum
    parseSections accum@(h,d,p,a)
                 (ParseUtils.Section _ "haddock" name fs)
      | name == "" = do h' <- parseFields haddockFlagsFields h fs
                        return (h', d, p, a)
      | otherwise  = do
          warning "The 'haddock' section should be unnamed"
          return accum
    parseSections (h,d,p,a)
                  (ParseUtils.Section line "install-dirs" name fs)
      | name == "" = do d' <- parseFields installDirsFields d fs
                        return (h, d',p,a)
      | otherwise  =
        syntaxError line $
        "Named 'install-dirs' section: '" ++ name
        ++ "'. Note that named 'install-dirs' sections are not allowed in the '"
        ++ userPackageEnvironmentFile ++ "' file."
    parseSections accum@(h, d,p,a)
                  (ParseUtils.Section _ "program-locations" name fs)
      | name == "" = do p' <- parseFields withProgramsFields p fs
                        return (h, d, p', a)
      | otherwise  = do
          warning "The 'program-locations' section should be unnamed"
          return accum
    parseSections accum@(h, d, p, a)
                  (ParseUtils.Section _ "program-default-options" name fs)
      | name == "" = do a' <- parseFields withProgramOptionsFields a fs
                        return (h, d, p, a')
      | otherwise  = do
          warning "The 'program-default-options' section should be unnamed"
          return accum
    parseSections accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum

-- | Accumulator type for 'parseSections'.
type SectionsAccum = (HaddockFlags, InstallDirs (Flag PathTemplate)
                     , [(String, FilePath)], [(String, [String])])

-- | Write out the package environment file.
writePackageEnvironmentFile :: FilePath -> PackageEnvironment -> IO ()
writePackageEnvironmentFile path pkgEnv = do
  let tmpPath = (path <.> "tmp")
  writeFile tmpPath $ explanation ++ pkgEnvStr ++ "\n"
  renameFile tmpPath path
  where
    pkgEnvStr = showPackageEnvironment pkgEnv
    explanation = unlines
      ["-- This is a Cabal package environment file."
      ,"-- THIS FILE IS AUTO-GENERATED. DO NOT EDIT DIRECTLY."
      ,"-- Please create a 'cabal.config' file in the same directory"
      ,"-- if you want to change the default settings for this sandbox."
      ,"",""
      ]

-- | Pretty-print the package environment.
showPackageEnvironment :: PackageEnvironment -> String
showPackageEnvironment pkgEnv = showPackageEnvironmentWithComments Nothing pkgEnv

-- | Pretty-print the package environment with default values for empty fields
-- commented out (just like the default ~/.cabal/config).
showPackageEnvironmentWithComments :: (Maybe PackageEnvironment)
                                      -> PackageEnvironment
                                      -> String
showPackageEnvironmentWithComments mdefPkgEnv pkgEnv = Disp.render $
      ppFields (pkgEnvFieldDescrs ConstraintSourceUnknown)
               mdefPkgEnv pkgEnv
  $+$ Disp.text ""
  $+$ ppSection "install-dirs" "" installDirsFields
                (fmap installDirsSection mdefPkgEnv) (installDirsSection pkgEnv)
  where
    installDirsSection = savedUserInstallDirs . pkgEnvSavedConfig

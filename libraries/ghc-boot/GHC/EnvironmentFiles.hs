-- | Logic for finding and parsing environemnt files
module GHC.EnvironmentFiles where

import GHC.Platform.ArchOS
import GHC.AppDir
import Prelude
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad
import System.Directory
import System.FilePath
import Control.Exception
import Data.Maybe
import System.IO.Error
import System.Environment

data PackageEnvironment = PackageEnvironment [PackageFlag] deriving Show


-- -----------------------------------------------------------------------------
-- | Find the package environment (if one exists)
--
-- We interpret the package environment as a set of package flags; to be
-- specific, if we find a package environment file like
--
-- > clear-package-db
-- > global-package-db
-- > package-db blah/package.conf.d
-- > package-id id1
-- > package-id id2
--
-- we interpret this as
--
-- > [ -hide-all-packages
-- > , -clear-package-db
-- > , -global-package-db
-- > , -package-db blah/package.conf.d
-- > , -package-id id1
-- > , -package-id id2
-- > ]
--
-- There's also an older syntax alias for package-id, which is just an
-- unadorned package id
--
-- > id1
-- > id2
--
findPackageEnv :: (String -> IO ()) -> Maybe String -> Bool -> String -> ArchOS -> IO (Maybe PackageEnvironment)
findPackageEnv logger packageEnv hide_all_packages prog_name archOS = do
    mPkgEnv <- runMaybeT $ msum $ [
                   getCmdLineArg >>= \env -> msum [
                       probeNullEnv env
                     , probeEnvFile env
                     , probeEnvName env
                     , cmdLineError env
                     ]
                 , getEnvVar >>= \env -> msum [
                       probeNullEnv env
                     , probeEnvFile env
                     , probeEnvName env
                     , envError     env
                     ]
                 , notIfHideAllPackages >> msum [
                       findLocalEnvFile >>= probeEnvFile
                     , probeEnvName defaultEnvName
                     ]
                 ]
    case mPkgEnv of
      Nothing ->
        -- No environment found. Leave DynFlags unchanged.
        return Nothing
      Just "-" -> do
        -- Explicitly disabled environment file. Leave DynFlags unchanged.
        return Nothing
      Just envfile -> do
        content <- readFile envfile
        logger ("Loaded package environment from " ++ envfile)
        let res = parseEnvFile envfile content
        case res of
          Left err -> error err
          Right p_e -> return (Just p_e)
--        let (_, dflags') = runCmdLine (runEwM (setFlagsFromEnvFile envfile content)) dflags

 --       return dflags'
  where
    -- Loading environments (by name or by location)

--    archOS = platformArchOS (targetPlatform dflags)

    namedEnvPath :: String -> MaybeT IO FilePath
    namedEnvPath name = do
     appdir <- versionedAppDir prog_name archOS
     return $ appdir </> "environments" </> name

    probeEnvName :: String -> MaybeT IO FilePath
    probeEnvName name = probeEnvFile =<< namedEnvPath name

    probeEnvFile :: FilePath -> MaybeT IO FilePath
    probeEnvFile path = do
      guard =<< liftIO (doesFileExist path)
      return path

    probeNullEnv :: FilePath -> MaybeT IO FilePath
    probeNullEnv "-" = return "-"
    probeNullEnv _   = mzero
    -- Various ways to define which environment to use

    getCmdLineArg :: MaybeT IO String
    getCmdLineArg = MaybeT $ return packageEnv

    getEnvVar :: MaybeT IO String
    getEnvVar = do
      mvar <- liftIO $ try $ getEnv "GHC_ENVIRONMENT"
      case mvar of
        Right var -> return var
        Left err  -> if isDoesNotExistError err then mzero
                                                else liftIO $ throwIO err

    notIfHideAllPackages :: MaybeT IO ()
    notIfHideAllPackages =
      guard (not hide_all_packages)

    defaultEnvName :: String
    defaultEnvName = "default"

    -- e.g. .ghc.environment.x86_64-linux-7.6.3
    localEnvFileName :: FilePath
    localEnvFileName = ".ghc.environment" <.> versionedFilePath archOS

    -- Search for an env file, starting in the current dir and looking upwards.
    -- Fail if we get to the users home dir or the filesystem root. That is,
    -- we don't look for an env file in the user's home dir. The user-wide
    -- env lives in ghc's versionedAppDir/environments/default
    findLocalEnvFile :: MaybeT IO FilePath
    findLocalEnvFile = do
        curdir  <- liftIO getCurrentDirectory
        homedir <- tryMaybeT getHomeDirectory
        let probe dir | isDrive dir || dir == homedir
                      = mzero
            probe dir = do
              let file = dir </> localEnvFileName
              exists <- liftIO (doesFileExist file)
              if exists
                then return file
                else probe (takeDirectory dir)
        probe curdir

    -- Error reporting

    cmdLineError :: String -> MaybeT IO a
    cmdLineError env = liftIO . ioError $
      mkIOError doesNotExistErrorType
        ("Package environment " ++ show env ++ " not found")
        Nothing
        (Just env)

    envError :: String -> MaybeT IO a
    envError env = liftIO . ioError $
      mkIOError doesNotExistErrorType
        ( ("Package environment "
        ++ show env
        ++ " (specified in GHC_ENVIRONMENT) not found") )
        Nothing
        (Just env)

data PackageFlag = PackageDb String
                 | ClearPackageDb
                 | HidePackage String
                 | GlobalPackageDb
                 | UserPackageDb
                 | PackageId String
                 deriving Show

parseEnvFile :: FilePath -> String -> Either String PackageEnvironment
parseEnvFile envfile = fmap (PackageEnvironment . catMaybes) . mapM parseEntry . lines
  where
    parseEntry str = case words str of
      ("package-db": _)     -> return (Just (PackageDb (envdir </> db)))
        -- relative package dbs are interpreted relative to the env file
        where envdir = takeDirectory envfile
              db     = drop 11 str
      ["clear-package-db"]  -> return (Just ClearPackageDb)
      ["hide-package", pkg] -> return (Just (HidePackage pkg))
      ["global-package-db"] -> return (Just GlobalPackageDb)
      ["user-package-db"]   -> return (Just UserPackageDb)
      ["package-id", pkgid] -> return (Just (PackageId pkgid))
      (('-':'-':_):_)       -> return Nothing -- comments
      -- and the original syntax introduced in 7.10:
      [pkgid]               -> return (Just (PackageId pkgid))
      []                    -> return Nothing
      _                     -> Left $
                                    "Can't parse environment file entry: "
                                 ++ envfile ++ ": " ++ str

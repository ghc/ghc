{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.GlobalFlags (
    GlobalFlags(..)
  , defaultGlobalFlags
  , RepoContext(..)
  , withRepoContext
  , withRepoContext'
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.Types
         ( Repo(..), RemoteRepo(..) )
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, flagToMaybe )
import Distribution.Utils.NubList
         ( NubList, fromNubList )
import Distribution.Client.HttpUtils
         ( HttpTransport, configureTransport )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Simple.Utils
         ( info )

import Control.Concurrent
         ( MVar, newMVar, modifyMVar )
import Control.Exception
         ( throwIO )
import System.FilePath
         ( (</>) )
import Network.URI
         ( URI, uriScheme, uriPath )
import qualified Data.Map as Map

import qualified Hackage.Security.Client                    as Sec
import qualified Hackage.Security.Util.Path                 as Sec
import qualified Hackage.Security.Util.Pretty               as Sec
import qualified Hackage.Security.Client.Repository.Cache   as Sec
import qualified Hackage.Security.Client.Repository.Local   as Sec.Local
import qualified Hackage.Security.Client.Repository.Remote  as Sec.Remote
import qualified Distribution.Client.Security.HTTP          as Sec.HTTP
import qualified Distribution.Client.Security.DNS           as Sec.DNS

-- ------------------------------------------------------------
-- * Global flags
-- ------------------------------------------------------------

-- | Flags that apply at the top level, not to any sub-command.
data GlobalFlags = GlobalFlags {
    globalVersion           :: Flag Bool,
    globalNumericVersion    :: Flag Bool,
    globalConfigFile        :: Flag FilePath,
    globalSandboxConfigFile :: Flag FilePath,
    globalConstraintsFile   :: Flag FilePath,
    globalRemoteRepos       :: NubList RemoteRepo,     -- ^ Available Hackage servers.
    globalCacheDir          :: Flag FilePath,
    globalLocalRepos        :: NubList FilePath,
    globalLogsDir           :: Flag FilePath,
    globalWorldFile         :: Flag FilePath,
    globalRequireSandbox    :: Flag Bool,
    globalIgnoreSandbox     :: Flag Bool,
    globalIgnoreExpiry      :: Flag Bool,    -- ^ Ignore security expiry dates
    globalHttpTransport     :: Flag String,
    globalNix               :: Flag Bool,  -- ^ Integrate with Nix
    globalStoreDir          :: Flag FilePath
  } deriving Generic

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags  = GlobalFlags {
    globalVersion           = Flag False,
    globalNumericVersion    = Flag False,
    globalConfigFile        = mempty,
    globalSandboxConfigFile = mempty,
    globalConstraintsFile   = mempty,
    globalRemoteRepos       = mempty,
    globalCacheDir          = mempty,
    globalLocalRepos        = mempty,
    globalLogsDir           = mempty,
    globalWorldFile         = mempty,
    globalRequireSandbox    = Flag False,
    globalIgnoreSandbox     = Flag False,
    globalIgnoreExpiry      = Flag False,
    globalHttpTransport     = mempty,
    globalNix               = Flag False,
    globalStoreDir          = mempty
  }

instance Monoid GlobalFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup GlobalFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * Repo context
-- ------------------------------------------------------------

-- | Access to repositories
data RepoContext = RepoContext {
    -- | All user-specified repositories
    repoContextRepos :: [Repo]

    -- | Get the HTTP transport
    --
    -- The transport will be initialized on the first call to this function.
    --
    -- NOTE: It is important that we don't eagerly initialize the transport.
    -- Initializing the transport is not free, and especially in contexts where
    -- we don't know a-priori whether or not we need the transport (for instance
    -- when using cabal in "nix mode") incurring the overhead of transport
    -- initialization on _every_ invocation (eg @cabal build@) is undesirable.
  , repoContextGetTransport :: IO HttpTransport

    -- | Get the (initialized) secure repo
    --
    -- (the 'Repo' type itself is stateless and must remain so, because it
    -- must be serializable)
  , repoContextWithSecureRepo :: forall a.
                                 Repo
                              -> (forall down. Sec.Repository down -> IO a)
                              -> IO a

    -- | Should we ignore expiry times (when checking security)?
  , repoContextIgnoreExpiry :: Bool
  }

-- | Wrapper around 'Repository', hiding the type argument
data SecureRepo = forall down. SecureRepo (Sec.Repository down)

withRepoContext :: Verbosity -> GlobalFlags -> (RepoContext -> IO a) -> IO a
withRepoContext verbosity globalFlags =
    withRepoContext'
      verbosity
      (fromNubList (globalRemoteRepos   globalFlags))
      (fromNubList (globalLocalRepos    globalFlags))
      (fromFlag    (globalCacheDir      globalFlags))
      (flagToMaybe (globalHttpTransport globalFlags))
      (flagToMaybe (globalIgnoreExpiry  globalFlags))

withRepoContext' :: Verbosity -> [RemoteRepo] -> [FilePath]
                 -> FilePath  -> Maybe String -> Maybe Bool
                 -> (RepoContext -> IO a)
                 -> IO a
withRepoContext' verbosity remoteRepos localRepos
                 sharedCacheDir httpTransport ignoreExpiry = \callback -> do
    transportRef <- newMVar Nothing
    let httpLib = Sec.HTTP.transportAdapter
                    verbosity
                    (getTransport transportRef)
    initSecureRepos verbosity httpLib secureRemoteRepos $ \secureRepos' ->
      callback RepoContext {
          repoContextRepos          = allRemoteRepos
                                   ++ map RepoLocal localRepos
        , repoContextGetTransport   = getTransport transportRef
        , repoContextWithSecureRepo = withSecureRepo secureRepos'
        , repoContextIgnoreExpiry   = fromMaybe False ignoreExpiry
        }
  where
    secureRemoteRepos =
      [ (remote, cacheDir) | RepoSecure remote cacheDir <- allRemoteRepos ]
    allRemoteRepos =
      [ (if isSecure then RepoSecure else RepoRemote) remote cacheDir
      | remote <- remoteRepos
      , let cacheDir = sharedCacheDir </> remoteRepoName remote
            isSecure = remoteRepoSecure remote == Just True
      ]

    getTransport :: MVar (Maybe HttpTransport) -> IO HttpTransport
    getTransport transportRef =
      modifyMVar transportRef $ \mTransport -> do
        transport <- case mTransport of
          Just tr -> return tr
          Nothing -> configureTransport verbosity httpTransport
        return (Just transport, transport)

    withSecureRepo :: Map Repo SecureRepo
                   -> Repo
                   -> (forall down. Sec.Repository down -> IO a)
                   -> IO a
    withSecureRepo secureRepos repo callback =
      case Map.lookup repo secureRepos of
        Just (SecureRepo secureRepo) -> callback secureRepo
        Nothing -> throwIO $ userError "repoContextWithSecureRepo: unknown repo"

-- | Initialize the provided secure repositories
--
-- Assumed invariant: `remoteRepoSecure` should be set for all these repos.
initSecureRepos :: forall a. Verbosity
                -> Sec.HTTP.HttpLib
                -> [(RemoteRepo, FilePath)]
                -> (Map Repo SecureRepo -> IO a)
                -> IO a
initSecureRepos verbosity httpLib repos callback = go Map.empty repos
  where
    go :: Map Repo SecureRepo -> [(RemoteRepo, FilePath)] -> IO a
    go !acc [] = callback acc
    go !acc ((r,cacheDir):rs) = do
      cachePath <- Sec.makeAbsolute $ Sec.fromFilePath cacheDir
      initSecureRepo verbosity httpLib r cachePath $ \r' ->
        go (Map.insert (RepoSecure r cacheDir) r' acc) rs

-- | Initialize the given secure repo
--
-- The security library has its own concept of a "local" repository, distinct
-- from @cabal-install@'s; these are secure repositories, but live in the local
-- file system. We use the convention that these repositories are identified by
-- URLs of the form @file:/path/to/local/repo@.
initSecureRepo :: Verbosity
               -> Sec.HTTP.HttpLib
               -> RemoteRepo  -- ^ Secure repo ('remoteRepoSecure' assumed)
               -> Sec.Path Sec.Absolute -- ^ Cache dir
               -> (SecureRepo -> IO a)  -- ^ Callback
               -> IO a
initSecureRepo verbosity httpLib RemoteRepo{..} cachePath = \callback -> do
    requiresBootstrap <- withRepo [] Sec.requiresBootstrap

    mirrors <- if requiresBootstrap
               then do
                   info verbosity $ "Trying to locate mirrors via DNS for " ++
                                    "initial bootstrap of secure " ++
                                    "repository '" ++ show remoteRepoURI ++
                                    "' ..."

                   Sec.DNS.queryBootstrapMirrors verbosity remoteRepoURI
               else pure []

    withRepo mirrors $ \r -> do
      when requiresBootstrap $ Sec.uncheckClientErrors $
        Sec.bootstrap r
          (map Sec.KeyId    remoteRepoRootKeys)
          (Sec.KeyThreshold (fromIntegral remoteRepoKeyThreshold))
      callback $ SecureRepo r
  where
    -- Initialize local or remote repo depending on the URI
    withRepo :: [URI] -> (forall down. Sec.Repository down -> IO a) -> IO a
    withRepo _ callback | uriScheme remoteRepoURI == "file:" = do
      dir <- Sec.makeAbsolute $ Sec.fromFilePath (uriPath remoteRepoURI)
      Sec.Local.withRepository dir
                               cache
                               Sec.hackageRepoLayout
                               Sec.hackageIndexLayout
                               logTUF
                               callback
    withRepo mirrors callback =
      Sec.Remote.withRepository httpLib
                                (remoteRepoURI:mirrors)
                                Sec.Remote.defaultRepoOpts
                                cache
                                Sec.hackageRepoLayout
                                Sec.hackageIndexLayout
                                logTUF
                                callback

    cache :: Sec.Cache
    cache = Sec.Cache {
        cacheRoot   = cachePath
      , cacheLayout = Sec.cabalCacheLayout {
            Sec.cacheLayoutIndexTar   = cacheFn "01-index.tar"
          , Sec.cacheLayoutIndexIdx   = cacheFn "01-index.tar.idx"
          , Sec.cacheLayoutIndexTarGz = cacheFn "01-index.tar.gz"
          }
      }

    cacheFn :: FilePath -> Sec.CachePath
    cacheFn = Sec.rootPath . Sec.fragment

    -- We display any TUF progress only in verbose mode, including any transient
    -- verification errors. If verification fails, then the final exception that
    -- is thrown will of course be shown.
    logTUF :: Sec.LogMessage -> IO ()
    logTUF = info verbosity . Sec.pretty

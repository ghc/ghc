-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Update
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Update
    ( update
    ) where

import Distribution.Simple.Setup
         ( fromFlag )
import Distribution.Client.Types
         ( Repo(..), RemoteRepo(..), maybeRepoRemote )
import Distribution.Client.HttpUtils
         ( DownloadResult(..) )
import Distribution.Client.FetchUtils
         ( downloadIndex )
import Distribution.Client.IndexUtils.Timestamp
import Distribution.Client.IndexUtils
         ( updateRepoIndexCache, Index(..), writeIndexTimestamp
         , currentIndexTimestamp )
import Distribution.Client.JobControl
         ( newParallelJobControl, spawnJob, collectJob )
import Distribution.Client.Setup
         ( RepoContext(..), UpdateFlags(..) )
import Distribution.Text
         ( display )
import Distribution.Verbosity

import Distribution.Simple.Utils
         ( writeFileAtomic, warn, notice, noticeNoWrap )

import qualified Data.ByteString.Lazy       as BS
import Distribution.Client.GZipUtils (maybeDecompress)
import System.FilePath (dropExtension)
import Data.Maybe (mapMaybe)
import Data.Time (getCurrentTime)
import Control.Monad

import qualified Hackage.Security.Client as Sec

-- | 'update' downloads the package list from all known servers
update :: Verbosity -> UpdateFlags -> RepoContext -> IO ()
update verbosity _ repoCtxt | null (repoContextRepos repoCtxt) = do
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
update verbosity updateFlags repoCtxt = do
  let repos       = repoContextRepos repoCtxt
      remoteRepos = mapMaybe maybeRepoRemote repos
  case remoteRepos of
    [] -> return ()
    [remoteRepo] ->
        notice verbosity $ "Downloading the latest package list from "
                        ++ remoteRepoName remoteRepo
    _ -> notice verbosity . unlines
            $ "Downloading the latest package lists from: "
            : map (("- " ++) . remoteRepoName) remoteRepos
  jobCtrl <- newParallelJobControl (length repos)
  mapM_ (spawnJob jobCtrl . updateRepo verbosity updateFlags repoCtxt) repos
  mapM_ (\_ -> collectJob jobCtrl) repos

updateRepo :: Verbosity -> UpdateFlags -> RepoContext -> Repo -> IO ()
updateRepo verbosity updateFlags repoCtxt repo = do
  transport <- repoContextGetTransport repoCtxt
  case repo of
    RepoLocal{..} -> return ()
    RepoRemote{..} -> do
      downloadResult <- downloadIndex transport verbosity repoRemote repoLocalDir
      case downloadResult of
        FileAlreadyInCache -> return ()
        FileDownloaded indexPath -> do
          writeFileAtomic (dropExtension indexPath) . maybeDecompress
                                                  =<< BS.readFile indexPath
          updateRepoIndexCache verbosity (RepoIndex repoCtxt repo)
    RepoSecure{} -> repoContextWithSecureRepo repoCtxt repo $ \repoSecure -> do
      let index = RepoIndex repoCtxt repo
      -- NB: This may be a nullTimestamp if we've never updated before
      current_ts <- currentIndexTimestamp (lessVerbose verbosity) repoCtxt repo
      -- NB: always update the timestamp, even if we didn't actually
      -- download anything
      writeIndexTimestamp index (fromFlag (updateIndexState updateFlags))
      ce <- if repoContextIgnoreExpiry repoCtxt
              then Just `fmap` getCurrentTime
              else return Nothing
      updated <- Sec.uncheckClientErrors $ Sec.checkForUpdates repoSecure ce
      -- Update cabal's internal index as well so that it's not out of sync
      -- (If all access to the cache goes through hackage-security this can go)
      case updated of
        Sec.NoUpdates  ->
          return ()
        Sec.HasUpdates ->
          updateRepoIndexCache verbosity index
      -- TODO: This will print multiple times if there are multiple
      -- repositories: main problem is we don't have a way of updating
      -- a specific repo.  Once we implement that, update this.
      when (current_ts /= nullTimestamp) $
        noticeNoWrap verbosity $
          "To revert to previous state run:\n" ++
          "    cabal update --index-state='" ++ display current_ts ++ "'\n"

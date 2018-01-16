{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}


-- | Management for the installed package store.
--
module Distribution.Client.Store (

    -- * The store layout
    StoreDirLayout(..),
    defaultStoreDirLayout,

    -- * Reading store entries
    getStoreEntries,
    doesStoreEntryExist,

    -- * Creating store entries
    newStoreEntry,
    NewStoreEntryOutcome(..),

    -- * Concurrency strategy
    -- $concurrency
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude
import Distribution.Client.Compat.FileLock

import           Distribution.Client.DistDirLayout
import           Distribution.Client.RebuildMonad

import           Distribution.Package (UnitId, mkUnitId)
import           Distribution.Compiler (CompilerId)

import           Distribution.Simple.Utils
                   ( withTempDirectory, debug, info )
import           Distribution.Verbosity
import           Distribution.Text

import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Exception
import           Control.Monad (forM_)
import           System.FilePath
import           System.Directory
import           System.IO


-- $concurrency
--
-- We access and update the store concurrently. Our strategy to do that safely
-- is as follows.
--
-- The store entries once created are immutable. This alone simplifies matters
-- considerably.
--
-- Additionally, the way 'UnitId' hashes are constructed means that if a store
-- entry exists already then we can assume its content is ok to reuse, rather
-- than having to re-recreate. This is the nix-style input hashing concept.
--
-- A consequence of this is that with a little care it is /safe/ to race
-- updates against each other. Consider two independent concurrent builds that
-- both want to build a particular 'UnitId', where that entry does not yet
-- exist in the store. It is safe for both to build and try to install this
-- entry into the store provided that:
--
-- * only one succeeds
-- * the looser discovers that they lost, they abandon their own build and
--   re-use the store entry installed by the winner.
--
-- Note that because builds are not reproducible in general (nor even
-- necessarily ABI compatible) then it is essential that the loser abandon
-- their build and use the one installed by the winner, so that subsequent
-- packages are built against the exact package from the store rather than some
-- morally equivalent package that may not be ABI compatible.
--
-- Our overriding goal is that store reads be simple, cheap and not require
-- locking. We will derive our write-side protocol to make this possible.
--
-- The read-side protocol is simply:
--
-- * check for the existence of a directory entry named after the 'UnitId' in
--   question. That is, if the dir entry @$root/foo-1.0-fe56a...@ exists then
--   the store entry can be assumed to be complete and immutable.
--
-- Given our read-side protocol, the final step on the write side must be to
-- atomically rename a fully-formed store entry directory into its final
-- location. While this will indeed be the final step, the preparatory steps
-- are more complicated. The tricky aspect is that the store also contains a
-- number of shared package databases (one per compiler version). Our read
-- strategy means that by the time we install the store dir entry the package
-- db must already have been updated. We cannot do the package db update
-- as part of atomically renaming the store entry directory however. Furthermore
-- it is not safe to allow either package db update because the db entry
-- contains the ABI hash and this is not guaranteed to be deterministic. So we
-- must register the new package prior to the atomic dir rename. Since this
-- combination of steps are not atomic then we need locking.
--
-- The write-side protocol is:
--
-- * Create a unique temp dir and write all store entry files into it.
--
-- * Take a lock named after the 'UnitId' in question.
--
-- * Once holding the lock, check again for the existence of the final store
--   entry directory. If the entry exists then the process lost the race and it
--   must abandon, unlock and re-use the existing store entry. If the entry
--   does not exist then the process won the race and it can proceed.
--
-- * Register the package into the package db. Note that the files are not in
--   their final location at this stage so registration file checks may need
--   to be disabled.
--
-- * Atomically rename the temp dir to the final store entry location.
--
-- * Release the previously-acquired lock.
--
-- Obviously this means it is possible to fail after registering but before
-- installing the store entry, leaving a dangling package db entry. This is not
-- much of a problem because this entry does not determine package existence
-- for cabal. It does mean however that the package db update should be insert
-- or replace, i.e. not failing if the db entry already exists.


-- | Check if a particular 'UnitId' exists in the store.
--
doesStoreEntryExist :: StoreDirLayout -> CompilerId -> UnitId -> IO Bool
doesStoreEntryExist StoreDirLayout{storePackageDirectory} compid unitid =
    doesDirectoryExist (storePackageDirectory compid unitid)


-- | Return the 'UnitId's of all packages\/components already installed in the
-- store.
--
getStoreEntries :: StoreDirLayout -> CompilerId -> Rebuild (Set UnitId)
getStoreEntries StoreDirLayout{storeDirectory} compid = do
    paths <- getDirectoryContentsMonitored (storeDirectory compid)
    return $! mkEntries paths
  where
    mkEntries     = Set.delete (mkUnitId "package.db")
                  . Set.delete (mkUnitId "incoming")
                  . Set.fromList
                  . map mkUnitId
                  . filter valid
    valid ('.':_) = False
    valid _       = True


-- | The outcome of 'newStoreEntry': either the store entry was newly created
-- or it existed already. The latter case happens if there was a race between
-- two builds of the same store entry.
--
data NewStoreEntryOutcome = UseNewStoreEntry
                          | UseExistingStoreEntry
  deriving (Eq, Show)

-- | Place a new entry into the store. See the concurrency strategy description
-- for full details.
--
-- In particular, it takes two actions: one to place files into a temporary
-- location, and a second to perform any necessary registration. The first
-- action is executed without any locks held (the temp dir is unique). The
-- second action holds a lock that guarantees that only one cabal process is
-- able to install this store entry. This means it is safe to register into
-- the compiler package DB or do other similar actions.
--
-- Note that if you need to use the registration information later then you
-- /must/ check the 'NewStoreEntryOutcome' and if it's'UseExistingStoreEntry'
-- then you must read the existing registration information (unless your
-- registration information is constructed fully deterministically).
--
newStoreEntry :: Verbosity
              -> StoreDirLayout
              -> CompilerId
              -> UnitId
              -> (FilePath -> IO (FilePath, [FilePath])) -- ^ Action to place files.
              -> IO ()                     -- ^ Register action, if necessary.
              -> IO NewStoreEntryOutcome
newStoreEntry verbosity storeDirLayout@StoreDirLayout{..}
              compid unitid
              copyFiles register =
    -- See $concurrency above for an explanation of the concurrency protocol

    withTempIncomingDir storeDirLayout compid $ \incomingTmpDir -> do

      -- Write all store entry files within the temp dir and return the prefix.
      (incomingEntryDir, otherFiles) <- copyFiles incomingTmpDir

      -- Take a lock named after the 'UnitId' in question.
      withIncomingUnitIdLock verbosity storeDirLayout compid unitid $ do

        -- Check for the existence of the final store entry directory.
        exists <- doesStoreEntryExist storeDirLayout compid unitid

        if exists
          -- If the entry exists then we lost the race and we must abandon,
          -- unlock and re-use the existing store entry.
          then do
            info verbosity $
                "Concurrent build race: abandoning build in favour of existing "
             ++ "store entry " ++ display compid </> display unitid
            return UseExistingStoreEntry

          -- If the entry does not exist then we won the race and can proceed.
          else do

            -- Register the package into the package db (if appropriate).
            register

            -- Atomically rename the temp dir to the final store entry location.
            renameDirectory incomingEntryDir finalEntryDir
            forM_ otherFiles $ \file -> do
              let finalStoreFile = storeDirectory compid </> makeRelative (incomingTmpDir </> (dropDrive (storeDirectory compid))) file
              createDirectoryIfMissing True (takeDirectory finalStoreFile)
              renameFile file finalStoreFile

            debug verbosity $
              "Installed store entry " ++ display compid </> display unitid
            return UseNewStoreEntry
  where
    finalEntryDir = storePackageDirectory compid unitid


withTempIncomingDir :: StoreDirLayout -> CompilerId
                    -> (FilePath -> IO a) -> IO a
withTempIncomingDir StoreDirLayout{storeIncomingDirectory} compid action = do
    createDirectoryIfMissing True incomingDir
    withTempDirectory silent incomingDir "new" action
  where
    incomingDir = storeIncomingDirectory compid


withIncomingUnitIdLock :: Verbosity -> StoreDirLayout
                       -> CompilerId -> UnitId
                       -> IO a -> IO a
withIncomingUnitIdLock verbosity StoreDirLayout{storeIncomingLock}
                       compid unitid action =
    bracket takeLock releaseLock (\_hnd -> action)
  where
    takeLock = do
      h <- openFile (storeIncomingLock compid unitid) ReadWriteMode
      -- First try non-blocking, but if we would have to wait then
      -- log an explanation and do it again in blocking mode.
      gotlock <- hTryLock h ExclusiveLock
      unless gotlock $ do
        info verbosity $ "Waiting for file lock on store entry "
                      ++ display compid </> display unitid
        hLock h ExclusiveLock
      return h

    releaseLock = hClose


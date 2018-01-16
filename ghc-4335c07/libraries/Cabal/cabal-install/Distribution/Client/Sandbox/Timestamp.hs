-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox.Timestamp
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Timestamp file handling (for add-source dependencies).
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox.Timestamp (
  AddSourceTimestamp,
  withAddTimestamps,
  withUpdateTimestamps,
  maybeAddCompilerTimestampRecord,
  listModifiedDeps,
  removeTimestamps,

  -- * For testing
  TimestampFileRecord,
  readTimestampFile,
  writeTimestampFile
  ) where

import Control.Monad                                 (filterM, forM, when)
import Data.Char                                     (isSpace)
import Data.List                                     (partition)
import System.Directory                              (renameFile)
import System.FilePath                               ((<.>), (</>))
import qualified Data.Map as M

import Distribution.Compiler                         (CompilerId)
import Distribution.Simple.Utils                     (debug, die', warn)
import Distribution.System                           (Platform)
import Distribution.Text                             (display)
import Distribution.Verbosity                        (Verbosity)

import Distribution.Client.SrcDist (allPackageSourceFiles)
import Distribution.Client.Sandbox.Index
  (ListIgnoredBuildTreeRefs (ListIgnored), RefTypesToList(OnlyLinks)
  ,listBuildTreeRefs)
import Distribution.Client.SetupWrapper

import Distribution.Compat.Exception                 (catchIO)
import Distribution.Compat.Time               (ModTime, getCurTime,
                                                      getModTime,
                                                      posixSecondsToModTime)


-- | Timestamp of an add-source dependency.
type AddSourceTimestamp  = (FilePath, ModTime)
-- | Timestamp file record - a string identifying the compiler & platform plus a
-- list of add-source timestamps.
type TimestampFileRecord = (String, [AddSourceTimestamp])

timestampRecordKey :: CompilerId -> Platform -> String
timestampRecordKey compId platform = display platform ++ "-" ++ display compId

-- | The 'add-source-timestamps' file keeps the timestamps of all add-source
-- dependencies. It is initially populated by 'sandbox add-source' and kept
-- current by 'reinstallAddSourceDeps' and 'configure -w'. The user can install
-- add-source deps manually with 'cabal install' after having edited them, so we
-- can err on the side of caution sometimes.
-- FIXME: We should keep this info in the index file, together with build tree
-- refs.
timestampFileName :: FilePath
timestampFileName = "add-source-timestamps"

-- | Read the timestamp file. Exits with error if the timestamp file is
-- corrupted. Returns an empty list if the file doesn't exist.
readTimestampFile :: Verbosity -> FilePath -> IO [TimestampFileRecord]
readTimestampFile verbosity timestampFile = do
  timestampString <- readFile timestampFile `catchIO` \_ -> return "[]"
  case reads timestampString of
    [(version, s)]
      | version == (2::Int) ->
        case reads s of
          [(timestamps, s')] | all isSpace s' -> return timestamps
          _                                   -> dieCorrupted
      | otherwise   -> dieWrongFormat

    -- Old format (timestamps are POSIX seconds). Convert to new format.
    [] ->
      case reads timestampString of
        [(timestamps, s)] | all isSpace s -> do
          let timestamps' = map (\(i, ts) ->
                                  (i, map (\(p, t) ->
                                            (p, posixSecondsToModTime t)) ts))
                            timestamps
          writeTimestampFile timestampFile timestamps'
          return timestamps'
        _ -> dieCorrupted
    _ -> dieCorrupted
  where
    dieWrongFormat    = die' verbosity $ wrongFormat ++ deleteAndRecreate
    dieCorrupted      = die' verbosity $ corrupted ++ deleteAndRecreate
    wrongFormat       = "The timestamps file is in the wrong format."
    corrupted         = "The timestamps file is corrupted."
    deleteAndRecreate = " Please delete and recreate the sandbox."

-- | Write the timestamp file, atomically.
writeTimestampFile :: FilePath -> [TimestampFileRecord] -> IO ()
writeTimestampFile timestampFile timestamps = do
  writeFile  timestampTmpFile "2\n" -- version
  appendFile timestampTmpFile (show timestamps ++ "\n")
  renameFile timestampTmpFile timestampFile
  where
    timestampTmpFile = timestampFile <.> "tmp"

-- | Read, process and write the timestamp file in one go.
withTimestampFile :: Verbosity -> FilePath
                     -> ([TimestampFileRecord] -> IO [TimestampFileRecord])
                     -> IO ()
withTimestampFile verbosity sandboxDir process = do
  let timestampFile = sandboxDir </> timestampFileName
  timestampRecords <- readTimestampFile verbosity timestampFile >>= process
  writeTimestampFile timestampFile timestampRecords

-- | Given a list of 'AddSourceTimestamp's, a list of paths to add-source deps
-- we've added and an initial timestamp, add an 'AddSourceTimestamp' to the list
-- for each path. If a timestamp for a given path already exists in the list,
-- update it.
addTimestamps :: ModTime -> [AddSourceTimestamp] -> [FilePath]
                 -> [AddSourceTimestamp]
addTimestamps initial timestamps newPaths =
  [ (p, initial) | p <- newPaths ] ++ oldTimestamps
  where
    (oldTimestamps, _toBeUpdated) =
      partition (\(path, _) -> path `notElem` newPaths) timestamps

-- | Given a list of 'AddSourceTimestamp's, a list of paths to add-source deps
-- we've reinstalled and a new timestamp value, update the timestamp value for
-- the deps in the list. If there are new paths in the list, ignore them.
updateTimestamps :: [AddSourceTimestamp] -> [FilePath] -> ModTime
                    -> [AddSourceTimestamp]
updateTimestamps timestamps pathsToUpdate newTimestamp =
  foldr updateTimestamp [] timestamps
  where
    updateTimestamp t@(path, _oldTimestamp) rest
      | path `elem` pathsToUpdate = (path, newTimestamp) : rest
      | otherwise                 = t : rest

-- | Given a list of 'TimestampFileRecord's and a list of paths to add-source
-- deps we've removed, remove those deps from the list.
removeTimestamps' :: [AddSourceTimestamp] -> [FilePath] -> [AddSourceTimestamp]
removeTimestamps' l pathsToRemove = foldr removeTimestamp [] l
  where
    removeTimestamp t@(path, _oldTimestamp) rest =
      if path `elem` pathsToRemove
      then rest
      else t : rest

-- | If a timestamp record for this compiler doesn't exist, add a new one.
maybeAddCompilerTimestampRecord :: Verbosity -> FilePath -> FilePath
                                   -> CompilerId -> Platform
                                   -> IO ()
maybeAddCompilerTimestampRecord verbosity sandboxDir indexFile
                                compId platform = do
  let key = timestampRecordKey compId platform
  withTimestampFile verbosity sandboxDir $ \timestampRecords -> do
    case lookup key timestampRecords of
      Just _  -> return timestampRecords
      Nothing -> do
        buildTreeRefs <- listBuildTreeRefs verbosity ListIgnored OnlyLinks
                         indexFile
        now <- getCurTime
        let timestamps = map (\p -> (p, now)) buildTreeRefs
        return $ (key, timestamps):timestampRecords

-- | Given an IO action that returns a list of build tree refs, add those
-- build tree refs to the timestamps file (for all compilers).
withAddTimestamps :: Verbosity -> FilePath -> IO [FilePath] -> IO ()
withAddTimestamps verbosity sandboxDir act = do
  let initialTimestamp = minBound
  withActionOnAllTimestamps (addTimestamps initialTimestamp) verbosity sandboxDir act

-- | Given a list of build tree refs, remove those
-- build tree refs from the timestamps file (for all compilers).
removeTimestamps :: Verbosity -> FilePath -> [FilePath] -> IO ()
removeTimestamps verbosity idxFile =
  withActionOnAllTimestamps removeTimestamps' verbosity idxFile . return

-- | Given an IO action that returns a list of build tree refs, update the
-- timestamps of the returned build tree refs to the current time (only for the
-- given compiler & platform).
withUpdateTimestamps :: Verbosity -> FilePath -> CompilerId -> Platform
                        ->([AddSourceTimestamp] -> IO [FilePath])
                        -> IO ()
withUpdateTimestamps =
  withActionOnCompilerTimestamps updateTimestamps

-- | Helper for implementing 'withAddTimestamps' and
-- 'withRemoveTimestamps'. Runs a given action on the list of
-- 'AddSourceTimestamp's for all compilers, applies 'f' to the result and then
-- updates the timestamp file. The IO action is run only once.
withActionOnAllTimestamps :: ([AddSourceTimestamp] -> [FilePath]
                              -> [AddSourceTimestamp])
                             -> Verbosity
                             -> FilePath
                             -> IO [FilePath]
                             -> IO ()
withActionOnAllTimestamps f verbosity sandboxDir act =
  withTimestampFile verbosity sandboxDir $ \timestampRecords -> do
    paths <- act
    return [(key, f timestamps paths) | (key, timestamps) <- timestampRecords]

-- | Helper for implementing 'withUpdateTimestamps'. Runs a given action on the
-- list of 'AddSourceTimestamp's for this compiler, applies 'f' to the result
-- and then updates the timestamp file record. The IO action is run only once.
withActionOnCompilerTimestamps :: ([AddSourceTimestamp]
                                   -> [FilePath] -> ModTime
                                   -> [AddSourceTimestamp])
                                  -> Verbosity
                                  -> FilePath
                                  -> CompilerId
                                  -> Platform
                                  -> ([AddSourceTimestamp] -> IO [FilePath])
                                  -> IO ()
withActionOnCompilerTimestamps f verbosity sandboxDir compId platform act = do
  let needle = timestampRecordKey compId platform
  withTimestampFile verbosity sandboxDir $ \timestampRecords -> do
    timestampRecords' <- forM timestampRecords $ \r@(key, timestamps) ->
      if key == needle
      then do paths <- act timestamps
              now   <- getCurTime
              return (key, f timestamps paths now)
      else return r
    return timestampRecords'

-- | Has this dependency been modified since we have last looked at it?
isDepModified :: Verbosity -> ModTime -> AddSourceTimestamp -> IO Bool
isDepModified verbosity now (packageDir, timestamp) = do
  debug verbosity ("Checking whether the dependency is modified: " ++ packageDir)
  -- TODO: we should properly plumb the correct options through
  -- instead of using defaultSetupScriptOptions
  depSources <- allPackageSourceFiles verbosity defaultSetupScriptOptions packageDir
  go depSources

  where
    go []         = return False
    go (dep0:rest) = do
      -- FIXME: What if the clock jumps backwards at any point? For now we only
      -- print a warning.
      let dep = packageDir </> dep0
      modTime <- getModTime dep
      when (modTime > now) $
        warn verbosity $ "File '" ++ dep
                         ++ "' has a modification time that is in the future."
      if modTime >= timestamp
        then do
          debug verbosity ("Dependency has a modified source file: " ++ dep)
          return True
        else go rest

-- | List all modified dependencies.
listModifiedDeps :: Verbosity -> FilePath -> CompilerId -> Platform
                    -> M.Map FilePath a
                       -- ^ The set of all installed add-source deps.
                    -> IO [FilePath]
listModifiedDeps verbosity sandboxDir compId platform installedDepsMap = do
  timestampRecords <- readTimestampFile verbosity (sandboxDir </> timestampFileName)
  let needle        = timestampRecordKey compId platform
  timestamps       <- maybe noTimestampRecord return
                      (lookup needle timestampRecords)
  now <- getCurTime
  fmap (map fst) . filterM (isDepModified verbosity now)
    . filter (\ts -> fst ts `M.member` installedDepsMap)
    $ timestamps

  where
    noTimestampRecord = die' verbosity $ "Сouldn't find a timestamp record for the given "
                        ++ "compiler/platform pair. "
                        ++ "Please report this on the Cabal bug tracker: "
                        ++ "https://github.com/haskell/cabal/issues/new ."

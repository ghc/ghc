{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | An abstraction for re-running actions if values or files have changed.
--
-- This is not a full-blown make-style incremental build system, it's a bit
-- more ad-hoc than that, but it's easier to integrate with existing code.
--
-- It's a convenient interface to the "Distribution.Client.FileMonitor"
-- functions.
--
module Distribution.Client.RebuildMonad (
    -- * Rebuild monad
    Rebuild,
    runRebuild,
    execRebuild,
    askRoot,

    -- * Setting up file monitoring
    monitorFiles,
    MonitorFilePath,
    monitorFile,
    monitorFileHashed,
    monitorNonExistentFile,
    monitorDirectory,
    monitorNonExistentDirectory,
    monitorDirectoryExistence,
    monitorFileOrDirectory,
    monitorFileSearchPath,
    monitorFileHashedSearchPath,
    -- ** Monitoring file globs
    monitorFileGlob,
    monitorFileGlobExistence,
    FilePathGlob(..),
    FilePathRoot(..),
    FilePathGlobRel(..),
    GlobPiece(..),

    -- * Using a file monitor
    FileMonitor(..),
    newFileMonitor,
    rerunIfChanged,

    -- * Utils
    matchFileGlob,
    getDirectoryContentsMonitored,
    createDirectoryMonitored,
    monitorDirectoryStatus,
    doesFileExistMonitored,
    need,
    needIfExists,
    findFileWithExtensionMonitored,
    findFirstFileMonitored,
    findFileMonitored,
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.FileMonitor
import Distribution.Client.Glob hiding (matchFileGlob)
import qualified Distribution.Client.Glob as Glob (matchFileGlob)

import Distribution.Simple.Utils (debug)
import Distribution.Verbosity    (Verbosity)

import Control.Monad.State as State
import Control.Monad.Reader as Reader
import System.FilePath
import System.Directory


-- | A monad layered on top of 'IO' to help with re-running actions when the
-- input files and values they depend on change. The crucial operations are
-- 'rerunIfChanged' and 'monitorFiles'.
--
newtype Rebuild a = Rebuild (ReaderT FilePath (StateT [MonitorFilePath] IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Use this wihin the body action of 'rerunIfChanged' to declare that the
-- action depends on the given files. This can be based on what the action
-- actually did. It is these files that will be checked for changes next
-- time 'rerunIfChanged' is called for that 'FileMonitor'.
--
-- Relative paths are interpreted as relative to an implicit root, ultimately
-- passed in to 'runRebuild'.
--
monitorFiles :: [MonitorFilePath] -> Rebuild ()
monitorFiles filespecs = Rebuild (State.modify (filespecs++))

-- | Run a 'Rebuild' IO action.
unRebuild :: FilePath -> Rebuild a -> IO (a, [MonitorFilePath])
unRebuild rootDir (Rebuild action) = runStateT (runReaderT action rootDir) []

-- | Run a 'Rebuild' IO action.
runRebuild :: FilePath -> Rebuild a -> IO a
runRebuild rootDir (Rebuild action) = evalStateT (runReaderT action rootDir) []

-- | Run a 'Rebuild' IO action.
execRebuild :: FilePath -> Rebuild a -> IO [MonitorFilePath]
execRebuild rootDir (Rebuild action) = execStateT (runReaderT action rootDir) []

-- | The root that relative paths are interpreted as being relative to.
askRoot :: Rebuild FilePath
askRoot = Rebuild Reader.ask

-- | This captures the standard use pattern for a 'FileMonitor': given a
-- monitor, an action and the input value the action depends on, either
-- re-run the action to get its output, or if the value and files the action
-- depends on have not changed then return a previously cached action result.
--
-- The result is still in the 'Rebuild' monad, so these can be nested.
--
-- Do not share 'FileMonitor's between different uses of 'rerunIfChanged'.
--
rerunIfChanged :: (Binary a, Binary b)
               => Verbosity
               -> FileMonitor a b
               -> a
               -> Rebuild b
               -> Rebuild b
rerunIfChanged verbosity monitor key action = do
    rootDir <- askRoot
    changed <- liftIO $ checkFileMonitorChanged monitor rootDir key
    case changed of
      MonitorUnchanged result files -> do
        liftIO $ debug verbosity $ "File monitor '" ++ monitorName
                                                    ++ "' unchanged."
        monitorFiles files
        return result

      MonitorChanged reason -> do
        liftIO $ debug verbosity $ "File monitor '" ++ monitorName
                                ++ "' changed: " ++ showReason reason
        startTime <- liftIO $ beginUpdateFileMonitor
        (result, files) <- liftIO $ unRebuild rootDir action
        liftIO $ updateFileMonitor monitor rootDir
                                   (Just startTime) files key result
        monitorFiles files
        return result
  where
    monitorName = takeFileName (fileMonitorCacheFile monitor)

    showReason (MonitoredFileChanged file) = "file " ++ file
    showReason (MonitoredValueChanged _)   = "monitor value changed"
    showReason  MonitorFirstRun            = "first run"
    showReason  MonitorCorruptCache        = "invalid cache file"


-- | Utility to match a file glob against the file system, starting from a
-- given root directory. The results are all relative to the given root.
--
-- Since this operates in the 'Rebuild' monad, it also monitors the given glob
-- for changes.
--
matchFileGlob :: FilePathGlob -> Rebuild [FilePath]
matchFileGlob glob = do
    root <- askRoot
    monitorFiles [monitorFileGlobExistence glob]
    liftIO $ Glob.matchFileGlob root glob

getDirectoryContentsMonitored :: FilePath -> Rebuild [FilePath]
getDirectoryContentsMonitored dir = do
    exists <- monitorDirectoryStatus dir
    if exists
      then liftIO $ getDirectoryContents dir
      else return []

createDirectoryMonitored :: Bool -> FilePath -> Rebuild ()
createDirectoryMonitored createParents dir = do
    monitorFiles [monitorDirectoryExistence dir]
    liftIO $ createDirectoryIfMissing createParents dir

-- | Monitor a directory as in 'monitorDirectory' if it currently exists or
-- as 'monitorNonExistentDirectory' if it does not.
monitorDirectoryStatus :: FilePath -> Rebuild Bool
monitorDirectoryStatus dir = do
    exists <- liftIO $ doesDirectoryExist dir
    monitorFiles [if exists
                    then monitorDirectory dir
                    else monitorNonExistentDirectory dir]
    return exists

-- | Like 'doesFileExist', but in the 'Rebuild' monad.  This does
-- NOT track the contents of 'FilePath'; use 'need' in that case.
doesFileExistMonitored :: FilePath -> Rebuild Bool
doesFileExistMonitored f = do
    root <- askRoot
    exists <- liftIO $ doesFileExist (root </> f)
    monitorFiles [if exists
                    then monitorFileExistence f
                    else monitorNonExistentFile f]
    return exists

-- | Monitor a single file
need :: FilePath -> Rebuild ()
need f = monitorFiles [monitorFileHashed f]

-- | Monitor a file if it exists; otherwise check for when it
-- gets created.  This is a bit better for recompilation avoidance
-- because sometimes users give bad package metadata, and we don't
-- want to repeatedly rebuild in this case (which we would if we
-- need'ed a non-existent file).
needIfExists :: FilePath -> Rebuild ()
needIfExists f = do
    root <- askRoot
    exists <- liftIO $ doesFileExist (root </> f)
    monitorFiles [if exists
                    then monitorFileHashed f
                    else monitorNonExistentFile f]

-- | Like 'findFileWithExtension', but in the 'Rebuild' monad.
findFileWithExtensionMonitored
    :: [String]
    -> [FilePath]
    -> FilePath
    -> Rebuild (Maybe FilePath)
findFileWithExtensionMonitored extensions searchPath baseName =
  findFirstFileMonitored id
    [ path </> baseName <.> ext
    | path <- nub searchPath
    , ext <- nub extensions ]

-- | Like 'findFirstFile', but in the 'Rebuild' monad.
findFirstFileMonitored :: (a -> FilePath) -> [a] -> Rebuild (Maybe a)
findFirstFileMonitored file = findFirst
  where findFirst []     = return Nothing
        findFirst (x:xs) = do exists <- doesFileExistMonitored (file x)
                              if exists
                                then return (Just x)
                                else findFirst xs

-- | Like 'findFile', but in the 'Rebuild' monad.
findFileMonitored :: [FilePath] -> FilePath -> Rebuild (Maybe FilePath)
findFileMonitored searchPath fileName =
  findFirstFileMonitored id
    [ path </> fileName
    | path <- nub searchPath]

{-# LANGUAGE CPP #-}

-- | Temporary file-system management
module GHC.Utils.TmpFs
    ( TmpFs
    , initTmpFs
    , forkTmpFsFrom
    , mergeTmpFsInto
    , PathsToClean(..)
    , emptyPathsToClean
    , TempFileLifetime(..)
    , TempDir (..)
    , cleanTempDirs
    , cleanTempFiles
    , cleanCurrentModuleTempFiles
    , keepCurrentModuleTempFiles
    , addFilesToClean
    , changeTempFilesLifetime
    , newTempName
    , newTempLibName
    , newTempSubDir
    , withSystemTempDirectory
    , withTempDirectory
    )
where

import GHC.Prelude

import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Exception as Exception
import GHC.Driver.Phases

import Data.List (partition)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.IORef
import System.Directory
import System.FilePath
import System.IO.Error

#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Internals
#endif

-- | Temporary file-system
data TmpFs = TmpFs
  { tmp_dirs_to_clean :: IORef (Map FilePath FilePath)
      -- ^ Maps system temporary directory (passed via settings or DynFlags) to
      -- an actual temporary directory for this process.
      --
      -- It's a Map probably to support changing the system temporary directory
      -- over time.
      --
      -- Shared with forked TmpFs.

  , tmp_next_suffix :: IORef Int
      -- ^ The next available suffix to uniquely name a temp file, updated
      -- atomically.
      --
      -- Shared with forked TmpFs.

  , tmp_dir_prefix :: String

  , tmp_files_to_clean :: IORef PathsToClean
      -- ^ Files to clean (per session or per module)
      --
      -- Not shared with forked TmpFs.
  , tmp_subdirs_to_clean :: IORef PathsToClean
      -- ^ Subdirs to clean (per session or per module)
      --
      -- Not shared with forked TmpFs.
  }

-- | A collection of paths that must be deleted before ghc exits.
data PathsToClean = PathsToClean
    { ptcGhcSession :: !(Set FilePath)
        -- ^ Paths that will be deleted at the end of runGhc(T)

    , ptcCurrentModule :: !(Set FilePath)
        -- ^ Paths that will be deleted the next time
        -- 'cleanCurrentModuleTempFiles' is called, or otherwise at the end of
        -- the session.
    }

-- | Used when a temp file is created. This determines which component Set of
-- PathsToClean will get the temp file
data TempFileLifetime
  = TFL_CurrentModule
  -- ^ A file with lifetime TFL_CurrentModule will be cleaned up at the
  -- end of upweep_mod
  | TFL_GhcSession
  -- ^ A file with lifetime TFL_GhcSession will be cleaned up at the end of
  -- runGhc(T)
  deriving (Show)

newtype TempDir = TempDir FilePath

-- | An empty PathsToClean
emptyPathsToClean :: PathsToClean
emptyPathsToClean = PathsToClean Set.empty Set.empty

-- | Merge two PathsToClean
mergePathsToClean :: PathsToClean -> PathsToClean -> PathsToClean
mergePathsToClean x y = PathsToClean
    { ptcGhcSession    = Set.union (ptcGhcSession x) (ptcGhcSession y)
    , ptcCurrentModule = Set.union (ptcCurrentModule x) (ptcCurrentModule y)
    }

-- | Initialise an empty TmpFs
initTmpFs :: IO TmpFs
initTmpFs = do
    files   <- newIORef emptyPathsToClean
    subdirs <- newIORef emptyPathsToClean
    dirs    <- newIORef Map.empty
    next    <- newIORef 0
    return $ TmpFs
        { tmp_files_to_clean   = files
        , tmp_subdirs_to_clean = subdirs
        , tmp_dirs_to_clean    = dirs
        , tmp_next_suffix      = next
        , tmp_dir_prefix       = "tmp"
        }

-- | Initialise an empty TmpFs sharing unique numbers and per-process temporary
-- directories with the given TmpFs
--
-- It's not safe to use the subdirs created by the original TmpFs with the
-- forked one. Use @newTempSubDir@ to create new subdirs instead.
forkTmpFsFrom :: TmpFs -> IO TmpFs
forkTmpFsFrom old = do
    files <- newIORef emptyPathsToClean
    subdirs <- newIORef emptyPathsToClean
    counter <- newIORef 0
    prefix  <- newTempSuffix old


    return $ TmpFs
        { tmp_files_to_clean   = files
        , tmp_subdirs_to_clean = subdirs
        , tmp_dirs_to_clean    = tmp_dirs_to_clean old
        , tmp_next_suffix      = counter
        , tmp_dir_prefix       = prefix
        }

-- | Merge the first TmpFs into the second.
--
-- The first TmpFs is returned emptied.
mergeTmpFsInto :: TmpFs -> TmpFs -> IO ()
mergeTmpFsInto src dst = do
    src_files <- atomicModifyIORef' (tmp_files_to_clean src) (\s -> (emptyPathsToClean, s))
    src_subdirs <- atomicModifyIORef' (tmp_subdirs_to_clean src) (\s -> (emptyPathsToClean, s))
    atomicModifyIORef' (tmp_files_to_clean dst) (\s -> (mergePathsToClean src_files s, ()))
    atomicModifyIORef' (tmp_subdirs_to_clean dst) (\s -> (mergePathsToClean src_subdirs s, ()))


cleanTempDirs :: Logger -> TmpFs -> IO ()
cleanTempDirs logger tmpfs
   = mask_
   $ do let ref = tmp_dirs_to_clean tmpfs
        ds <- atomicModifyIORef' ref $ \ds -> (Map.empty, ds)
        removeTmpDirs logger (Map.elems ds)

-- | Delete all paths in @tmp_files_to_clean@ and @tmp_subdirs_to_clean@.
cleanTempFiles :: Logger -> TmpFs -> IO ()
cleanTempFiles logger tmpfs
   = mask_
   $ do removeWith (removeTmpFiles logger) (tmp_files_to_clean tmpfs)
        removeWith (removeTmpSubdirs logger) (tmp_subdirs_to_clean tmpfs)
  where
    removeWith remove ref = do
      to_delete <- atomicModifyIORef' ref $
        \PathsToClean
            { ptcCurrentModule = cm_paths
            , ptcGhcSession = gs_paths
            } -> ( emptyPathsToClean
                  , Set.toList cm_paths ++ Set.toList gs_paths)
      remove to_delete

-- | Keep all the paths in @tmp_files_to_clean@ and @tmp_subdirs_to_clean@
-- that have lifetime TFL_CurrentModule. This function is used when `-keep-tmp-files` is
-- used in an OPTIONS_GHC pragma.
-- This function removes the temporary file from the TmpFs so we no longer remove
-- it at the env when cleanTempFiles is called.
keepCurrentModuleTempFiles :: HasCallStack => Logger -> TmpFs -> IO ()
keepCurrentModuleTempFiles logger tmpfs
   = mask_
   $ do to_keep_files <- keep  (tmp_files_to_clean tmpfs)
        to_keep_subdirs <- keep  (tmp_subdirs_to_clean tmpfs)
        -- Remove any folders which contain any files we want to keep from the
        -- directories we are tracking. A new temporary directory will be created
        -- the next time a temporary file is needed (by perhaps another module).
        keepDirs (to_keep_files ++ to_keep_subdirs) (tmp_dirs_to_clean tmpfs)
  where
    keepDirs keeps ref = do
      let keep_dirs = Set.fromList (map takeDirectory keeps)
      atomicModifyIORef' ref  $ \m -> (Map.filter (\fp -> fp `Set.notMember` keep_dirs) m, ())

    keep ref = do
        to_keep <- atomicModifyIORef' ref $
            \ptc@PathsToClean{ptcCurrentModule = cm_paths} ->
                (ptc {ptcCurrentModule = Set.empty}, Set.toList cm_paths)
        debugTraceMsg logger 2 (text "Keeping:" <+> hsep (map text to_keep))
        return to_keep

-- | Delete all paths in @tmp_files_to_clean@ and @tmp_subdirs_to_clean@
-- That have lifetime TFL_CurrentModule.
-- If a file must be cleaned eventually, but must survive a
-- cleanCurrentModuleTempFiles, ensure it has lifetime TFL_GhcSession.
cleanCurrentModuleTempFiles :: Logger -> TmpFs -> IO ()
cleanCurrentModuleTempFiles logger tmpfs
   = mask_
   $ do removeWith (removeTmpFiles logger) (tmp_files_to_clean tmpfs)
        removeWith (removeTmpSubdirs logger) (tmp_subdirs_to_clean tmpfs)
  where
    removeWith remove ref = do
        to_delete <- atomicModifyIORef' ref $
            \ptc@PathsToClean{ptcCurrentModule = cm_paths} ->
                (ptc {ptcCurrentModule = Set.empty}, Set.toList cm_paths)
        remove to_delete

-- | Ensure that new_files are cleaned on the next call of
-- 'cleanTempFiles' or 'cleanCurrentModuleTempFiles', depending on lifetime.
-- If any of new_files are already tracked, they will have their lifetime
-- updated.
addFilesToClean :: TmpFs -> TempFileLifetime -> [FilePath] -> IO ()
addFilesToClean tmpfs lifetime new_files =
  addToClean (tmp_files_to_clean tmpfs) lifetime new_files

addSubdirsToClean :: TmpFs -> TempFileLifetime -> [FilePath] -> IO ()
addSubdirsToClean tmpfs lifetime new_subdirs =
  addToClean (tmp_subdirs_to_clean tmpfs) lifetime new_subdirs

addToClean :: IORef PathsToClean -> TempFileLifetime -> [FilePath] -> IO ()
addToClean ref lifetime new_filepaths = modifyIORef' ref $
  \PathsToClean
    { ptcCurrentModule = cm_paths
    , ptcGhcSession = gs_paths
    } -> case lifetime of
      TFL_CurrentModule -> PathsToClean
        { ptcCurrentModule = cm_paths `Set.union` new_filepaths_set
        , ptcGhcSession = gs_paths `Set.difference` new_filepaths_set
        }
      TFL_GhcSession -> PathsToClean
        { ptcCurrentModule = cm_paths `Set.difference` new_filepaths_set
        , ptcGhcSession = gs_paths `Set.union` new_filepaths_set
        }
  where
    new_filepaths_set = Set.fromList new_filepaths

-- | Update the lifetime of files already being tracked. If any files are
-- not being tracked they will be discarded.
changeTempFilesLifetime :: TmpFs -> TempFileLifetime -> [FilePath] -> IO ()
changeTempFilesLifetime tmpfs lifetime files = do
  PathsToClean
    { ptcCurrentModule = cm_paths
    , ptcGhcSession = gs_paths
    } <- readIORef (tmp_files_to_clean tmpfs)
  let old_set = case lifetime of
        TFL_CurrentModule -> gs_paths
        TFL_GhcSession -> cm_paths
      existing_files = [f | f <- files, f `Set.member` old_set]
  addFilesToClean tmpfs lifetime existing_files

-- Return a unique numeric temp file suffix
newTempSuffix :: TmpFs -> IO String
newTempSuffix tmpfs = do
  n <- atomicModifyIORef' (tmp_next_suffix tmpfs) $ \n -> (n+1,n)
  return $ tmp_dir_prefix tmpfs ++ "_" ++ show n


-- Find a temporary name that doesn't already exist.
newTempName :: Logger -> TmpFs -> TempDir -> TempFileLifetime -> Suffix -> IO FilePath
newTempName logger tmpfs tmp_dir lifetime extn
  = do d <- getTempDir logger tmpfs tmp_dir
       findTempName (d </> "ghc_") -- See Note [Deterministic base name]
  where
    findTempName :: FilePath -> IO FilePath
    findTempName prefix
      = do suffix <- newTempSuffix tmpfs
           let filename = prefix ++ suffix <.> extn
           b <- doesFileExist filename
           if b then findTempName prefix
                else do -- clean it up later
                        addFilesToClean tmpfs lifetime [filename]
                        return filename

-- | Create a new temporary subdirectory that doesn't already exist
-- The temporary subdirectory is automatically removed at the end of the
-- GHC session, but its contents aren't. Make sure to leave the directory
-- empty before the end of the session, either by removing content
-- directly or by using @addFilesToClean@.
--
-- If the created subdirectory is not empty, it will not be removed (along
-- with its parent temporary directory) and a warning message will be
-- printed at verbosity 2 and higher.
newTempSubDir :: Logger -> TmpFs -> TempDir -> IO FilePath
newTempSubDir logger tmpfs tmp_dir
  = do d <- getTempDir logger tmpfs tmp_dir
       findTempDir (d </> "ghc_")
  where
    findTempDir :: FilePath -> IO FilePath
    findTempDir prefix
      = do suffix <- newTempSuffix tmpfs
           let name = prefix ++ suffix
           b <- doesDirectoryExist name
           if b then findTempDir prefix
                else (do
                  createDirectory name
                  addSubdirsToClean tmpfs TFL_GhcSession [name]
                  return name)
            `Exception.catchIO` \e -> if isAlreadyExistsError e
                  then findTempDir prefix else ioError e

newTempLibName :: Logger -> TmpFs -> TempDir -> TempFileLifetime -> Suffix
  -> IO (FilePath, FilePath, String)
newTempLibName logger tmpfs tmp_dir lifetime extn
  = do d <- getTempDir logger tmpfs tmp_dir
       findTempName d ("ghc_")
  where
    findTempName :: FilePath -> String -> IO (FilePath, FilePath, String)
    findTempName dir prefix
      = do suffix <- newTempSuffix tmpfs -- See Note [Deterministic base name]
           let libname = prefix ++ suffix
               filename = dir </> "lib" ++ libname <.> extn
           b <- doesFileExist filename
           if b then findTempName dir prefix
                else do -- clean it up later
                        addFilesToClean tmpfs lifetime [filename]
                        return (filename, dir, libname)


-- Return our temporary directory within tmp_dir, creating one if we
-- don't have one yet.
getTempDir :: Logger -> TmpFs -> TempDir -> IO FilePath
getTempDir logger tmpfs (TempDir tmp_dir) = do
    mapping <- readIORef dir_ref
    case Map.lookup tmp_dir mapping of
        Nothing -> do
            pid <- getProcessID
            let prefix = tmp_dir </> "ghc" ++ show pid ++ "_"
            mask_ $ mkTempDir prefix
        Just dir -> return dir
  where
    dir_ref = tmp_dirs_to_clean tmpfs

    mkTempDir :: FilePath -> IO FilePath
    mkTempDir prefix = do
        suffix <- newTempSuffix tmpfs
        let our_dir = prefix ++ suffix

        -- 1. Speculatively create our new directory.
        createDirectory our_dir

        -- 2. Update the tmp_dirs_to_clean mapping unless an entry already exists
        -- (i.e. unless another thread beat us to it).
        their_dir <- atomicModifyIORef' dir_ref $ \mapping ->
            case Map.lookup tmp_dir mapping of
                Just dir -> (mapping, Just dir)
                Nothing  -> (Map.insert tmp_dir our_dir mapping, Nothing)

        -- 3. If there was an existing entry, return it and delete the
        -- directory we created.  Otherwise return the directory we created.
        case their_dir of
            Nothing  -> do
                debugTraceMsg logger 2 $
                    text "Created temporary directory:" <+> text our_dir
                return our_dir
            Just dir -> do
                removeDirectory our_dir
                return dir
      `Exception.catchIO` \e -> if isAlreadyExistsError e
                      then mkTempDir prefix else ioError e

{- Note [Deterministic base name]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The filename of temporary files, especially the basename of C files, can end
up in the output in some form, e.g. as part of linker debug information. In the
interest of bit-wise exactly reproducible compilation (#4012), the basename of
the temporary file no longer contains random information (it used to contain
the process id).

This is ok, as the temporary directory used contains the pid (see getTempDir).

In addition to this, multiple threads can race against each other creating temporary
files. Therefore we supply a prefix when creating temporary files, when a thread is
forked, each thread must be given an TmpFs with a unique prefix. This is achieved
by forkTmpFsFrom creating a fresh prefix from the parent TmpFs.
-}
removeTmpDirs :: Logger -> [FilePath] -> IO ()
removeTmpDirs logger ds
  = traceCmd logger "Deleting temp dirs"
             ("Deleting: " ++ unwords ds)
             (mapM_ (removeWith logger removeDirectory) ds)

removeTmpFiles :: Logger -> [FilePath] -> IO ()
removeTmpFiles logger fs
  = warnNon $
    traceCmd logger "Deleting temp files"
             ("Deleting: " ++ unwords deletees)
             (mapM_ (removeWith logger removeFile) deletees)
  where
     -- Flat out refuse to delete files that are likely to be source input
     -- files (is there a worse bug than having a compiler delete your source
     -- files?)
     --
     -- Deleting source files is a sign of a bug elsewhere, so prominently flag
     -- the condition.
    warnNon act
     | null non_deletees = act
     | otherwise         = do
        putMsg logger (text "WARNING - NOT deleting source files:"
                   <+> hsep (map text non_deletees))
        act

    (non_deletees, deletees) = partition isHaskellUserSrcFilename fs

removeTmpSubdirs :: Logger -> [FilePath] -> IO ()
removeTmpSubdirs logger fs
  = traceCmd logger "Deleting temp subdirs"
             ("Deleting: " ++ unwords fs)
             (mapM_ (removeWith logger removeDirectory) fs)

removeWith :: Logger -> (FilePath -> IO ()) -> FilePath -> IO ()
removeWith logger remover f = remover f `Exception.catchIO`
  (\e ->
   let msg = if isDoesNotExistError e
             then text "Warning: deleting non-existent" <+> text f
             else text "Warning: exception raised when deleting"
                                            <+> text f <> colon
               $$ text (show e)
   in debugTraceMsg logger 2 msg
  )

#if defined(mingw32_HOST_OS)
-- relies on Int == Int32 on Windows
foreign import ccall unsafe "_getpid" getProcessID :: IO Int
#else
getProcessID :: IO Int
getProcessID = System.Posix.Internals.c_getpid >>= return . fromIntegral
#endif

-- The following three functions are from the `temporary` package.

-- | Create and use a temporary directory in the system standard temporary
-- directory.
--
-- Behaves exactly the same as 'withTempDirectory', except that the parent
-- temporary directory will be that returned by 'getTemporaryDirectory'.
withSystemTempDirectory :: String   -- ^ Directory name template. See 'openTempFile'.
                        -> (FilePath -> IO a) -- ^ Callback that can use the directory
                        -> IO a
withSystemTempDirectory template action =
  getTemporaryDirectory >>= \tmpDir -> withTempDirectory tmpDir template action


-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the template. The temp directory is deleted after use. For example:
--
-- > withTempDirectory "src" "sdist." $ \tmpDir -> do ...
--
-- The @tmpDir@ will be a new subdirectory of the given directory, e.g.
-- @src/sdist.342@.
withTempDirectory :: FilePath -- ^ Temp directory to create the directory in
                  -> String   -- ^ Directory name template. See 'openTempFile'.
                  -> (FilePath -> IO a) -- ^ Callback that can use the directory
                  -> IO a
withTempDirectory targetDir template =
  Exception.bracket
    (createTempDirectory targetDir template)
    (ignoringIOErrors . removeDirectoryRecursive)

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `Exception.catchIO` const (return ())


createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir template = do
  pid <- getProcessID
  findTempName pid
  where findTempName x = do
            let path = dir </> template ++ show x
            createDirectory path
            return path
          `Exception.catchIO` \e -> if isAlreadyExistsError e
                          then findTempName (x+1) else ioError e

{-# LANGUAGE CPP #-}

-- | Temporary file-system management
module GHC.Utils.TmpFs
    ( TmpFs
    , initTmpFs
    , forkTmpFsFrom
    , mergeTmpFsInto
    , FilesToClean(..)
    , emptyFilesToClean
    , TempFileLifetime(..)
    , cleanTempDirs
    , cleanTempFiles
    , cleanCurrentModuleTempFiles
    , addFilesToClean
    , changeTempFilesLifetime
    , newTempName
    , newTempLibName
    , newTempDir
    , withSystemTempDirectory
    , withTempDirectory
    )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Exception as Exception
import GHC.Driver.Phases

import Control.Monad
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

  , tmp_files_to_clean :: IORef FilesToClean
      -- ^ Files to clean (per session or per module)
      --
      -- Not shared with forked TmpFs.
  }

-- | A collection of files that must be deleted before ghc exits.
data FilesToClean = FilesToClean
    { ftcGhcSession :: !(Set FilePath)
        -- ^ Files that will be deleted at the end of runGhc(T)

    , ftcCurrentModule :: !(Set FilePath)
        -- ^ Files that will be deleted the next time
        -- 'cleanCurrentModuleTempFiles' is called, or otherwise at the end of
        -- the session.
    }

-- | Used when a temp file is created. This determines which component Set of
-- FilesToClean will get the temp file
data TempFileLifetime
  = TFL_CurrentModule
  -- ^ A file with lifetime TFL_CurrentModule will be cleaned up at the
  -- end of upweep_mod
  | TFL_GhcSession
  -- ^ A file with lifetime TFL_GhcSession will be cleaned up at the end of
  -- runGhc(T)
  deriving (Show)


-- | An empty FilesToClean
emptyFilesToClean :: FilesToClean
emptyFilesToClean = FilesToClean Set.empty Set.empty

-- | Merge two FilesToClean
mergeFilesToClean :: FilesToClean -> FilesToClean -> FilesToClean
mergeFilesToClean x y = FilesToClean
    { ftcGhcSession    = Set.union (ftcGhcSession x) (ftcGhcSession y)
    , ftcCurrentModule = Set.union (ftcCurrentModule x) (ftcCurrentModule y)
    }

-- | Initialise an empty TmpFs
initTmpFs :: IO TmpFs
initTmpFs = do
    files <- newIORef emptyFilesToClean
    dirs  <- newIORef Map.empty
    next  <- newIORef 0
    return $ TmpFs
        { tmp_files_to_clean = files
        , tmp_dirs_to_clean  = dirs
        , tmp_next_suffix    = next
        }

-- | Initialise an empty TmpFs sharing unique numbers and per-process temporary
-- directories with the given TmpFs
forkTmpFsFrom :: TmpFs -> IO TmpFs
forkTmpFsFrom old = do
    files <- newIORef emptyFilesToClean
    return $ TmpFs
        { tmp_files_to_clean = files
        , tmp_dirs_to_clean  = tmp_dirs_to_clean old
        , tmp_next_suffix    = tmp_next_suffix old
        }

-- | Merge the first TmpFs into the second.
--
-- The first TmpFs is returned emptied.
mergeTmpFsInto :: TmpFs -> TmpFs -> IO ()
mergeTmpFsInto src dst = do
    src_files <- atomicModifyIORef' (tmp_files_to_clean src) (\s -> (emptyFilesToClean, s))
    atomicModifyIORef' (tmp_files_to_clean dst) (\s -> (mergeFilesToClean src_files s, ()))

cleanTempDirs :: Logger -> TmpFs -> DynFlags -> IO ()
cleanTempDirs logger tmpfs dflags
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = tmp_dirs_to_clean tmpfs
        ds <- atomicModifyIORef' ref $ \ds -> (Map.empty, ds)
        removeTmpDirs logger dflags (Map.elems ds)

-- | Delete all files in @tmp_files_to_clean@.
cleanTempFiles :: Logger -> TmpFs -> DynFlags -> IO ()
cleanTempFiles logger tmpfs dflags
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = tmp_files_to_clean tmpfs
        to_delete <- atomicModifyIORef' ref $
            \FilesToClean
                { ftcCurrentModule = cm_files
                , ftcGhcSession = gs_files
                } -> ( emptyFilesToClean
                     , Set.toList cm_files ++ Set.toList gs_files)
        removeTmpFiles logger dflags to_delete

-- | Delete all files in @tmp_files_to_clean@. That have lifetime
-- TFL_CurrentModule.
-- If a file must be cleaned eventually, but must survive a
-- cleanCurrentModuleTempFiles, ensure it has lifetime TFL_GhcSession.
cleanCurrentModuleTempFiles :: Logger -> TmpFs -> DynFlags -> IO ()
cleanCurrentModuleTempFiles logger tmpfs dflags
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = tmp_files_to_clean tmpfs
        to_delete <- atomicModifyIORef' ref $
            \ftc@FilesToClean{ftcCurrentModule = cm_files} ->
                (ftc {ftcCurrentModule = Set.empty}, Set.toList cm_files)
        removeTmpFiles logger dflags to_delete

-- | Ensure that new_files are cleaned on the next call of
-- 'cleanTempFiles' or 'cleanCurrentModuleTempFiles', depending on lifetime.
-- If any of new_files are already tracked, they will have their lifetime
-- updated.
addFilesToClean :: TmpFs -> TempFileLifetime -> [FilePath] -> IO ()
addFilesToClean tmpfs lifetime new_files = modifyIORef' (tmp_files_to_clean tmpfs) $
  \FilesToClean
    { ftcCurrentModule = cm_files
    , ftcGhcSession = gs_files
    } -> case lifetime of
      TFL_CurrentModule -> FilesToClean
        { ftcCurrentModule = cm_files `Set.union` new_files_set
        , ftcGhcSession = gs_files `Set.difference` new_files_set
        }
      TFL_GhcSession -> FilesToClean
        { ftcCurrentModule = cm_files `Set.difference` new_files_set
        , ftcGhcSession = gs_files `Set.union` new_files_set
        }
  where
    new_files_set = Set.fromList new_files

-- | Update the lifetime of files already being tracked. If any files are
-- not being tracked they will be discarded.
changeTempFilesLifetime :: TmpFs -> TempFileLifetime -> [FilePath] -> IO ()
changeTempFilesLifetime tmpfs lifetime files = do
  FilesToClean
    { ftcCurrentModule = cm_files
    , ftcGhcSession = gs_files
    } <- readIORef (tmp_files_to_clean tmpfs)
  let old_set = case lifetime of
        TFL_CurrentModule -> gs_files
        TFL_GhcSession -> cm_files
      existing_files = [f | f <- files, f `Set.member` old_set]
  addFilesToClean tmpfs lifetime existing_files

-- Return a unique numeric temp file suffix
newTempSuffix :: TmpFs -> IO Int
newTempSuffix tmpfs =
  atomicModifyIORef' (tmp_next_suffix tmpfs) $ \n -> (n+1,n)

-- Find a temporary name that doesn't already exist.
newTempName :: Logger -> TmpFs -> DynFlags -> TempFileLifetime -> Suffix -> IO FilePath
newTempName logger tmpfs dflags lifetime extn
  = do d <- getTempDir logger tmpfs dflags
       findTempName (d </> "ghc_") -- See Note [Deterministic base name]
  where
    findTempName :: FilePath -> IO FilePath
    findTempName prefix
      = do n <- newTempSuffix tmpfs
           let filename = prefix ++ show n <.> extn
           b <- doesFileExist filename
           if b then findTempName prefix
                else do -- clean it up later
                        addFilesToClean tmpfs lifetime [filename]
                        return filename

newTempDir :: Logger -> TmpFs -> DynFlags -> IO FilePath
newTempDir logger tmpfs dflags
  = do d <- getTempDir logger tmpfs dflags
       findTempDir (d </> "ghc_")
  where
    findTempDir :: FilePath -> IO FilePath
    findTempDir prefix
      = do n <- newTempSuffix tmpfs
           let filename = prefix ++ show n
           b <- doesDirectoryExist filename
           if b then findTempDir prefix
                else do createDirectory filename
                        -- see mkTempDir below; this is wrong: -> consIORef (tmp_dirs_to_clean tmpfs) filename
                        return filename

newTempLibName :: Logger -> TmpFs -> DynFlags -> TempFileLifetime -> Suffix
  -> IO (FilePath, FilePath, String)
newTempLibName logger tmpfs dflags lifetime extn
  = do d <- getTempDir logger tmpfs dflags
       findTempName d ("ghc_")
  where
    findTempName :: FilePath -> String -> IO (FilePath, FilePath, String)
    findTempName dir prefix
      = do n <- newTempSuffix tmpfs -- See Note [Deterministic base name]
           let libname = prefix ++ show n
               filename = dir </> "lib" ++ libname <.> extn
           b <- doesFileExist filename
           if b then findTempName dir prefix
                else do -- clean it up later
                        addFilesToClean tmpfs lifetime [filename]
                        return (filename, dir, libname)


-- Return our temporary directory within tmp_dir, creating one if we
-- don't have one yet.
getTempDir :: Logger -> TmpFs -> DynFlags -> IO FilePath
getTempDir logger tmpfs dflags = do
    mapping <- readIORef dir_ref
    case Map.lookup tmp_dir mapping of
        Nothing -> do
            pid <- getProcessID
            let prefix = tmp_dir </> "ghc" ++ show pid ++ "_"
            mask_ $ mkTempDir prefix
        Just dir -> return dir
  where
    tmp_dir = tmpDir dflags
    dir_ref = tmp_dirs_to_clean tmpfs

    mkTempDir :: FilePath -> IO FilePath
    mkTempDir prefix = do
        n <- newTempSuffix tmpfs
        let our_dir = prefix ++ show n

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
                debugTraceMsg logger dflags 2 $
                    text "Created temporary directory:" <+> text our_dir
                return our_dir
            Just dir -> do
                removeDirectory our_dir
                return dir
      `catchIO` \e -> if isAlreadyExistsError e
                      then mkTempDir prefix else ioError e

{- Note [Deterministic base name]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The filename of temporary files, especially the basename of C files, can end
up in the output in some form, e.g. as part of linker debug information. In the
interest of bit-wise exactly reproducible compilation (#4012), the basename of
the temporary file no longer contains random information (it used to contain
the process id).

This is ok, as the temporary directory used contains the pid (see getTempDir).
-}
removeTmpDirs :: Logger -> DynFlags -> [FilePath] -> IO ()
removeTmpDirs logger dflags ds
  = traceCmd logger dflags "Deleting temp dirs"
             ("Deleting: " ++ unwords ds)
             (mapM_ (removeWith logger dflags removeDirectory) ds)

removeTmpFiles :: Logger -> DynFlags -> [FilePath] -> IO ()
removeTmpFiles logger dflags fs
  = warnNon $
    traceCmd logger dflags "Deleting temp files"
             ("Deleting: " ++ unwords deletees)
             (mapM_ (removeWith logger dflags removeFile) deletees)
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
        putMsg logger dflags (text "WARNING - NOT deleting source files:"
                       <+> hsep (map text non_deletees))
        act

    (non_deletees, deletees) = partition isHaskellUserSrcFilename fs

removeWith :: Logger -> DynFlags -> (FilePath -> IO ()) -> FilePath -> IO ()
removeWith logger dflags remover f = remover f `catchIO`
  (\e ->
   let msg = if isDoesNotExistError e
             then text "Warning: deleting non-existent" <+> text f
             else text "Warning: exception raised when deleting"
                                            <+> text f <> colon
               $$ text (show e)
   in debugTraceMsg logger dflags 2 msg
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
ignoringIOErrors ioe = ioe `catchIO` const (return ())


createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir template = do
  pid <- getProcessID
  findTempName pid
  where findTempName x = do
            let path = dir </> template ++ show x
            createDirectory path
            return path
          `catchIO` \e -> if isAlreadyExistsError e
                          then findTempName (x+1) else ioError e

{-# LANGUAGE CPP #-}
module GHC.SysTools.FileCleanup
  ( TempFileLifetime(..)
  , cleanTempDirs, cleanTempFiles, cleanCurrentModuleTempFiles
  , addFilesToClean, changeTempFilesLifetime
  , newTempName, newTempLibName, newTempDir
  , withSystemTempDirectory, withTempDirectory
  ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Exception as Exception
import GHC.Driver.Phases

import Control.Monad
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IORef
import System.Directory
import System.FilePath
import System.IO.Error

#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Internals
#endif

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

cleanTempDirs :: DynFlags -> IO ()
cleanTempDirs dflags
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = dirsToClean dflags
        ds <- atomicModifyIORef' ref $ \ds -> (Map.empty, ds)
        removeTmpDirs dflags (Map.elems ds)

-- | Delete all files in @filesToClean dflags@.
cleanTempFiles :: DynFlags -> IO ()
cleanTempFiles dflags
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = filesToClean dflags
        to_delete <- atomicModifyIORef' ref $
            \FilesToClean
                { ftcCurrentModule = cm_files
                , ftcGhcSession = gs_files
                } -> ( emptyFilesToClean
                     , Set.toList cm_files ++ Set.toList gs_files)
        removeTmpFiles dflags to_delete

-- | Delete all files in @filesToClean dflags@. That have lifetime
-- TFL_CurrentModule.
-- If a file must be cleaned eventually, but must survive a
-- cleanCurrentModuleTempFiles, ensure it has lifetime TFL_GhcSession.
cleanCurrentModuleTempFiles :: DynFlags -> IO ()
cleanCurrentModuleTempFiles dflags
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = filesToClean dflags
        to_delete <- atomicModifyIORef' ref $
            \ftc@FilesToClean{ftcCurrentModule = cm_files} ->
                (ftc {ftcCurrentModule = Set.empty}, Set.toList cm_files)
        removeTmpFiles dflags to_delete

-- | Ensure that new_files are cleaned on the next call of
-- 'cleanTempFiles' or 'cleanCurrentModuleTempFiles', depending on lifetime.
-- If any of new_files are already tracked, they will have their lifetime
-- updated.
addFilesToClean :: DynFlags -> TempFileLifetime -> [FilePath] -> IO ()
addFilesToClean dflags lifetime new_files = modifyIORef' (filesToClean dflags) $
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
changeTempFilesLifetime :: DynFlags -> TempFileLifetime -> [FilePath] -> IO ()
changeTempFilesLifetime dflags lifetime files = do
  FilesToClean
    { ftcCurrentModule = cm_files
    , ftcGhcSession = gs_files
    } <- readIORef (filesToClean dflags)
  let old_set = case lifetime of
        TFL_CurrentModule -> gs_files
        TFL_GhcSession -> cm_files
      existing_files = [f | f <- files, f `Set.member` old_set]
  addFilesToClean dflags lifetime existing_files

-- Return a unique numeric temp file suffix
newTempSuffix :: DynFlags -> IO Int
newTempSuffix dflags =
  atomicModifyIORef' (nextTempSuffix dflags) $ \n -> (n+1,n)

-- Find a temporary name that doesn't already exist.
newTempName :: DynFlags -> TempFileLifetime -> Suffix -> IO FilePath
newTempName dflags lifetime extn
  = do d <- getTempDir dflags
       findTempName (d </> "ghc_") -- See Note [Deterministic base name]
  where
    findTempName :: FilePath -> IO FilePath
    findTempName prefix
      = do n <- newTempSuffix dflags
           let filename = prefix ++ show n <.> extn
           b <- doesFileExist filename
           if b then findTempName prefix
                else do -- clean it up later
                        addFilesToClean dflags lifetime [filename]
                        return filename

newTempDir :: DynFlags -> IO FilePath
newTempDir dflags
  = do d <- getTempDir dflags
       findTempDir (d </> "ghc_")
  where
    findTempDir :: FilePath -> IO FilePath
    findTempDir prefix
      = do n <- newTempSuffix dflags
           let filename = prefix ++ show n
           b <- doesDirectoryExist filename
           if b then findTempDir prefix
                else do createDirectory filename
                        -- see mkTempDir below; this is wrong: -> consIORef (dirsToClean dflags) filename
                        return filename

newTempLibName :: DynFlags -> TempFileLifetime -> Suffix
  -> IO (FilePath, FilePath, String)
newTempLibName dflags lifetime extn
  = do d <- getTempDir dflags
       findTempName d ("ghc_")
  where
    findTempName :: FilePath -> String -> IO (FilePath, FilePath, String)
    findTempName dir prefix
      = do n <- newTempSuffix dflags -- See Note [Deterministic base name]
           let libname = prefix ++ show n
               filename = dir </> "lib" ++ libname <.> extn
           b <- doesFileExist filename
           if b then findTempName dir prefix
                else do -- clean it up later
                        addFilesToClean dflags lifetime [filename]
                        return (filename, dir, libname)


-- Return our temporary directory within tmp_dir, creating one if we
-- don't have one yet.
getTempDir :: DynFlags -> IO FilePath
getTempDir dflags = do
    mapping <- readIORef dir_ref
    case Map.lookup tmp_dir mapping of
        Nothing -> do
            pid <- getProcessID
            let prefix = tmp_dir </> "ghc" ++ show pid ++ "_"
            mask_ $ mkTempDir prefix
        Just dir -> return dir
  where
    tmp_dir = tmpDir dflags
    dir_ref = dirsToClean dflags

    mkTempDir :: FilePath -> IO FilePath
    mkTempDir prefix = do
        n <- newTempSuffix dflags
        let our_dir = prefix ++ show n

        -- 1. Speculatively create our new directory.
        createDirectory our_dir

        -- 2. Update the dirsToClean mapping unless an entry already exists
        -- (i.e. unless another thread beat us to it).
        their_dir <- atomicModifyIORef' dir_ref $ \mapping ->
            case Map.lookup tmp_dir mapping of
                Just dir -> (mapping, Just dir)
                Nothing  -> (Map.insert tmp_dir our_dir mapping, Nothing)

        -- 3. If there was an existing entry, return it and delete the
        -- directory we created.  Otherwise return the directory we created.
        case their_dir of
            Nothing  -> do
                debugTraceMsg dflags 2 $
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
removeTmpDirs :: DynFlags -> [FilePath] -> IO ()
removeTmpDirs dflags ds
  = traceCmd dflags "Deleting temp dirs"
             ("Deleting: " ++ unwords ds)
             (mapM_ (removeWith dflags removeDirectory) ds)

removeTmpFiles :: DynFlags -> [FilePath] -> IO ()
removeTmpFiles dflags fs
  = warnNon $
    traceCmd dflags "Deleting temp files"
             ("Deleting: " ++ unwords deletees)
             (mapM_ (removeWith dflags removeFile) deletees)
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
        putMsg dflags (text "WARNING - NOT deleting source files:"
                       <+> hsep (map text non_deletees))
        act

    (non_deletees, deletees) = partition isHaskellUserSrcFilename fs

removeWith :: DynFlags -> (FilePath -> IO ()) -> FilePath -> IO ()
removeWith dflags remover f = remover f `catchIO`
  (\e ->
   let msg = if isDoesNotExistError e
             then text "Warning: deleting non-existent" <+> text f
             else text "Warning: exception raised when deleting"
                                            <+> text f <> colon
               $$ text (show e)
   in debugTraceMsg dflags 2 msg
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

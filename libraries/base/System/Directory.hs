-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- System-independent interface to directory manipulation.
--
-----------------------------------------------------------------------------

module System.Directory 
   ( 
    -- $intro

    -- * Actions on directories
      createDirectory		-- :: FilePath -> IO ()
    , createDirectoryIfMissing  -- :: Bool -> FilePath -> IO ()
    , removeDirectory		-- :: FilePath -> IO ()
    , removeDirectoryRecursive  -- :: FilePath -> IO ()
    , renameDirectory		-- :: FilePath -> FilePath -> IO ()

    , getDirectoryContents      -- :: FilePath -> IO [FilePath]
    , getCurrentDirectory       -- :: IO FilePath
    , setCurrentDirectory       -- :: FilePath -> IO ()

    -- * Pre-defined directories
    , getHomeDirectory
    , getAppUserDataDirectory
    , getUserDocumentsDirectory
    , getTemporaryDirectory

    -- * Actions on files
    , removeFile		-- :: FilePath -> IO ()
    , renameFile                -- :: FilePath -> FilePath -> IO ()
    , copyFile                  -- :: FilePath -> FilePath -> IO ()
    
    , canonicalizePath
    , findExecutable

    -- * Existence tests
    , doesFileExist		-- :: FilePath -> IO Bool
    , doesDirectoryExist        -- :: FilePath -> IO Bool

    -- * Permissions

    -- $permissions

    , Permissions(
	Permissions,
	readable,		-- :: Permissions -> Bool
	writable,		-- :: Permissions -> Bool
	executable,		-- :: Permissions -> Bool
	searchable		-- :: Permissions -> Bool
      )

    , getPermissions            -- :: FilePath -> IO Permissions
    , setPermissions	        -- :: FilePath -> Permissions -> IO ()

    -- * Timestamps

    , getModificationTime       -- :: FilePath -> IO ClockTime
   ) where

import System.Directory.Internals
import System.Environment      ( getEnv )
import System.IO.Error
import Control.Monad           ( when, unless )

#ifdef __NHC__
import Directory
#endif /* __NHC__ */

#ifdef __HUGS__
import Hugs.Directory
#endif /* __HUGS__ */

import Foreign
import Foreign.C

{-# CFILES cbits/PrelIOUtils.c #-}

#ifdef __GLASGOW_HASKELL__
import Prelude

import Control.Exception       ( bracket )
import System.Posix.Types
import System.Posix.Internals
import System.Time             ( ClockTime(..) )
import System.IO

import GHC.IOBase	( IOException(..), IOErrorType(..), ioException )

{- $intro
A directory contains a series of entries, each of which is a named
reference to a file system object (file, directory etc.).  Some
entries may be hidden, inaccessible, or have some administrative
function (e.g. `.' or `..' under POSIX
<http://www.opengroup.org/onlinepubs/007904975/toc.htm>), but in 
this standard all such entries are considered to form part of the
directory contents. Entries in sub-directories are not, however,
considered to form part of the directory contents.

Each file system object is referenced by a /path/.  There is
normally at least one absolute path to each file system object.  In
some operating systems, it may also be possible to have paths which
are relative to the current directory.
-}

-----------------------------------------------------------------------------
-- Permissions

{- $permissions

 The 'Permissions' type is used to record whether certain operations are
 permissible on a file\/directory. 'getPermissions' and 'setPermissions'
 get and set these permissions, respectively. Permissions apply both to
 files and directories. For directories, the executable field will be
 'False', and for files the searchable field will be 'False'. Note that
 directories may be searchable without being readable, if permission has
 been given to use them as part of a path, but not to examine the 
 directory contents.

Note that to change some, but not all permissions, a construct on the following lines must be used. 

>  makeReadable f = do
>     p <- getPermissions f
>     setPermissions f (p {readable = True})

-}

data Permissions
 = Permissions {
    readable,   writable, 
    executable, searchable :: Bool 
   } deriving (Eq, Ord, Read, Show)

{- |The 'getPermissions' operation returns the
permissions for the file or directory.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to access
  the permissions; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}

getPermissions :: FilePath -> IO Permissions
getPermissions name = do
  withCString name $ \s -> do
  read  <- c_access s r_OK
  write <- c_access s w_OK
  exec  <- c_access s x_OK
  withFileStatus "getPermissions" name $ \st -> do
  is_dir <- isDirectory st
  return (
    Permissions {
      readable   = read  == 0,
      writable   = write == 0,
      executable = not is_dir && exec == 0,
      searchable = is_dir && exec == 0
    }
   )

{- |The 'setPermissions' operation sets the
permissions for the file or directory.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to set
  the permissions; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions name (Permissions r w e s) = do
  allocaBytes sizeof_stat $ \ p_stat -> do
  withCString name $ \p_name -> do
    throwErrnoIfMinus1_ "setPermissions" $ do
      c_stat p_name p_stat
      mode <- st_mode p_stat
      let mode1 = modifyBit r mode s_IRUSR
      let mode2 = modifyBit w mode1 s_IWUSR
      let mode3 = modifyBit (e || s) mode2 s_IXUSR
      c_chmod p_name mode3

 where
   modifyBit :: Bool -> CMode -> CMode -> CMode
   modifyBit False m b = m .&. (complement b)
   modifyBit True  m b = m .|. b


copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions source dest = do
  allocaBytes sizeof_stat $ \ p_stat -> do
  withCString source $ \p_source -> do
  withCString dest $ \p_dest -> do
    throwErrnoIfMinus1_ "copyPermissions" $ c_stat p_source p_stat
    mode <- st_mode p_stat
    throwErrnoIfMinus1_ "copyPermissions" $ c_chmod p_dest mode

-----------------------------------------------------------------------------
-- Implementation

{- |@'createDirectory' dir@ creates a new directory @dir@ which is
initially empty, or as near to empty as the operating system
allows.

The operation may fail with:

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES]@

* 'isAlreadyExistsError' \/ 'AlreadyExists'
The operand refers to a directory that already exists.  
@ [EEXIST]@

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'NoSuchThing'
There is no path to the directory. 
@[ENOENT, ENOTDIR]@

* 'ResourceExhausted'
Insufficient resources (virtual memory, process file descriptors,
physical disk space, etc.) are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* 'InappropriateType'
The path refers to an existing non-directory object.
@[EEXIST]@

-}

createDirectory :: FilePath -> IO ()
createDirectory path = do
  modifyIOError (`ioeSetFileName` path) $
    withCString path $ \s -> do
      throwErrnoIfMinus1Retry_ "createDirectory" $
	mkdir s 0o777

#else /* !__GLASGOW_HASKELL__ */

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions fromFPath toFPath
  = getPermissions fromFPath >>= setPermissions toFPath

#endif

-- | @'createDirectoryIfMissing' parents dir@ creates a new directory 
-- @dir@ if it doesn\'t exist. If the first argument is 'True'
-- the function will also create all parent directories if they are missing.
createDirectoryIfMissing :: Bool     -- ^ Create its parents too?
		         -> FilePath -- ^ The path to the directory you want to make
		         -> IO ()
createDirectoryIfMissing parents file = do
  b <- doesDirectoryExist file
  case (b,parents, file) of 
    (_,     _, "") -> return ()
    (True,  _,  _) -> return ()
    (_,  True,  _) -> mapM_ (createDirectoryIfMissing False) (tail (pathParents file))
    (_, False,  _) -> createDirectory file

#if __GLASGOW_HASKELL__
{- | @'removeDirectory' dir@ removes an existing directory /dir/.  The
implementation may specify additional constraints which must be
satisfied before a directory can be removed (e.g. the directory has to
be empty, or may not be in use by other processes).  It is not legal
for an implementation to partially remove a directory unless the
entire directory is removed. A conformant implementation need not
support directory removal in all situations (e.g. removal of the root
directory).

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
EIO

* 'InvalidArgument'
The operand is not a valid directory name.
[ENAMETOOLONG, ELOOP]

* 'isDoesNotExistError' \/ 'NoSuchThing'
The directory does not exist. 
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.  
@[EBUSY, ENOTEMPTY, EEXIST]@

* 'UnsupportedOperation'
The implementation does not support removal in this situation.
@[EINVAL]@

* 'InappropriateType'
The operand refers to an existing non-directory object.
@[ENOTDIR]@

-}

removeDirectory :: FilePath -> IO ()
removeDirectory path = do
  modifyIOError (`ioeSetFileName` path) $
    withCString path $ \s ->
       throwErrnoIfMinus1Retry_ "removeDirectory" (c_rmdir s)
#endif

-- | @'removeDirectoryRecursive' dir@  removes an existing directory /dir/
-- together with its content and all subdirectories. Be careful, 
-- if the directory contains symlinks, the function will follow them.
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive startLoc = do
  cont <- getDirectoryContents startLoc
  sequence_ [rm (startLoc `joinFileName` x) | x <- cont, x /= "." && x /= ".."]
  removeDirectory startLoc
  where
    rm :: FilePath -> IO ()
    rm f = do temp <- try (removeFile f)
              case temp of
                Left e  -> do isDir <- doesDirectoryExist f
                              -- If f is not a directory, re-throw the error
                              unless isDir $ ioError e
                              removeDirectoryRecursive f
                Right _ -> return ()

#if __GLASGOW_HASKELL__
{- |'removeFile' /file/ removes the directory entry for an existing file
/file/, where /file/ is not itself a directory. The
implementation may specify additional constraints which must be
satisfied before a file can be removed (e.g. the file may not be in
use by other processes).

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The file does not exist. 
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.  
@[EBUSY]@

* 'InappropriateType'
The operand refers to an existing directory.
@[EPERM, EINVAL]@

-}

removeFile :: FilePath -> IO ()
removeFile path = do
  modifyIOError (`ioeSetFileName` path) $
    withCString path $ \s ->
      throwErrnoIfMinus1Retry_ "removeFile" (c_unlink s)

{- |@'renameDirectory' old new@ changes the name of an existing
directory from /old/ to /new/.  If the /new/ directory
already exists, it is atomically replaced by the /old/ directory.
If the /new/ directory is neither the /old/ directory nor an
alias of the /old/ directory, it is removed as if by
'removeDirectory'.  A conformant implementation need not support
renaming directories in all situations (e.g. renaming to an existing
directory, or across different physical devices), but the constraints
must be documented.

On Win32 platforms, @renameDirectory@ fails if the /new/ directory already
exists.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
Either operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The original directory does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.  
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@

* 'UnsupportedOperation'
The implementation does not support renaming in this situation.
@[EINVAL, EXDEV]@

* 'InappropriateType'
Either path refers to an existing non-directory object.
@[ENOTDIR, EISDIR]@

-}

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory opath npath =
   withFileStatus "renameDirectory" opath $ \st -> do
   is_dir <- isDirectory st
   if (not is_dir)
	then ioException (IOError Nothing InappropriateType "renameDirectory"
			    ("not a directory") (Just opath))
	else do

   withCString opath $ \s1 ->
     withCString npath $ \s2 ->
        throwErrnoIfMinus1Retry_ "renameDirectory" (c_rename s1 s2)

{- |@'renameFile' old new@ changes the name of an existing file system
object from /old/ to /new/.  If the /new/ object already
exists, it is atomically replaced by the /old/ object.  Neither
path may refer to an existing directory.  A conformant implementation
need not support renaming files in all situations (e.g. renaming
across different physical devices), but the constraints must be
documented.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
Either operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The original file does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.  
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.
@[EBUSY]@

* 'UnsupportedOperation'
The implementation does not support renaming in this situation.
@[EXDEV]@

* 'InappropriateType'
Either path refers to an existing directory.
@[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@

-}

renameFile :: FilePath -> FilePath -> IO ()
renameFile opath npath =
   withFileOrSymlinkStatus "renameFile" opath $ \st -> do
   is_dir <- isDirectory st
   if is_dir
	then ioException (IOError Nothing InappropriateType "renameFile"
			   "is a directory" (Just opath))
	else do

    withCString opath $ \s1 ->
      withCString npath $ \s2 ->
         throwErrnoIfMinus1Retry_ "renameFile" (c_rename s1 s2)

#endif /* __GLASGOW_HASKELL__ */

{- |@'copyFile' old new@ copies the existing file from /old/ to /new/.
If the /new/ file already exists, it is atomically replaced by the /old/ file.
Neither path may refer to an existing directory.  The permissions of /old/ are
copied to /new/, if possible.
-}

{- NOTES:

It's tempting to try to remove the target file before opening it for
writing.  This could be useful: for example if the target file is an
executable that is in use, writing will fail, but unlinking first
would succeed.

However, it certainly isn't always what you want.

 * if the target file is hardlinked, removing it would break
   the hard link, but just opening would preserve it.

 * opening and truncating will preserve permissions and
   ACLs on the target.

 * If the destination file is read-only in a writable directory,
   we might want copyFile to fail.  Removing the target first
   would succeed, however.

 * If the destination file is special (eg. /dev/null), removing
   it is probably not the right thing.  Copying to /dev/null
   should leave /dev/null intact, not replace it with a plain
   file.

 * There's a small race condition between removing the target and
   opening it for writing during which time someone might
   create it again.
-}
copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFPath toFPath =
#if (!(defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 600))
	do readFile fromFPath >>= writeFile toFPath
	   try (copyPermissions fromFPath toFPath)
	   return ()
#else
	(bracket (openBinaryFile fromFPath ReadMode) hClose $ \hFrom ->
	 bracket (openBinaryFile toFPath WriteMode) hClose $ \hTo ->
	 allocaBytes bufferSize $ \buffer -> do
		copyContents hFrom hTo buffer
		try (copyPermissions fromFPath toFPath)
		return ()) `catch` (ioError . changeFunName)
	where
		bufferSize = 1024
		
		changeFunName (IOError h iot fun str mb_fp) = IOError h iot "copyFile" str mb_fp
		
		copyContents hFrom hTo buffer = do
			count <- hGetBuf hFrom buffer bufferSize
			when (count > 0) $ do
				hPutBuf hTo buffer count
				copyContents hFrom hTo buffer
#endif

-- | Given path referring to a file or directory, returns a
-- canonicalized path, with the intent that two paths referring
-- to the same file\/directory will map to the same canonicalized
-- path. Note that it is impossible to guarantee that the
-- implication (same file\/dir \<=\> same canonicalizedPath) holds
-- in either direction: this function can make only a best-effort
-- attempt.
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath fpath =
  withCString fpath $ \pInPath ->
  allocaBytes long_path_size $ \pOutPath ->
#if defined(mingw32_HOST_OS)
  alloca $ \ppFilePart ->
    do c_GetFullPathName pInPath (fromIntegral long_path_size) pOutPath ppFilePart
#else
    do c_realpath pInPath pOutPath
#endif
       peekCString pOutPath

#if defined(mingw32_HOST_OS)
foreign import stdcall unsafe "GetFullPathNameA"
            c_GetFullPathName :: CString
                              -> CInt
                              -> CString
                              -> Ptr CString
                              -> IO CInt
#else
foreign import ccall unsafe "realpath"
                   c_realpath :: CString
                              -> CString
                              -> IO CString
#endif

-- | Given an executable file name, searches for such file
-- in the directories listed in system PATH. The returned value 
-- is the path to the found executable or Nothing if there isn't
-- such executable. For example (findExecutable \"ghc\")
-- gives you the path to GHC.
findExecutable :: String -> IO (Maybe FilePath)
findExecutable binary =
#if defined(mingw32_HOST_OS)
  withCString binary $ \c_binary ->
  withCString ('.':exeExtension) $ \c_ext ->
  allocaBytes long_path_size $ \pOutPath ->
  alloca $ \ppFilePart -> do
    res <- c_SearchPath nullPtr c_binary c_ext (fromIntegral long_path_size) pOutPath ppFilePart
    if res > 0 && res < fromIntegral long_path_size
      then do fpath <- peekCString pOutPath
              return (Just fpath)
      else return Nothing

foreign import stdcall unsafe "SearchPathA"
            c_SearchPath :: CString
                         -> CString
                         -> CString
                         -> CInt
                         -> CString
                         -> Ptr CString
                         -> IO CInt
#else
 do
  path <- getEnv "PATH"
  search (parseSearchPath path)
  where
    fileName = binary `joinFileExt` exeExtension

    search :: [FilePath] -> IO (Maybe FilePath)
    search [] = return Nothing
    search (d:ds) = do
	let path = d `joinFileName` fileName
	b <- doesFileExist path
	if b then return (Just path)
             else search ds
#endif


#ifdef __GLASGOW_HASKELL__
{- |@'getDirectoryContents' dir@ returns a list of /all/ entries
in /dir/. 

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The directory does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.
@[EMFILE, ENFILE]@

* 'InappropriateType'
The path refers to an existing non-directory object.
@[ENOTDIR]@

-}

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = do
  modifyIOError (`ioeSetFileName` path) $
   alloca $ \ ptr_dEnt ->
     bracket
	(withCString path $ \s -> 
	   throwErrnoIfNullRetry desc (c_opendir s))
	(\p -> throwErrnoIfMinus1_ desc (c_closedir p))
	(\p -> loop ptr_dEnt p)
  where
    desc = "getDirectoryContents"

    loop :: Ptr (Ptr CDirent) -> Ptr CDir -> IO [String]
    loop ptr_dEnt dir = do
      resetErrno
      r <- readdir dir ptr_dEnt
      if (r == 0)
	 then do
	         dEnt    <- peek ptr_dEnt
		 if (dEnt == nullPtr)
		   then return []
		   else do
	 	    entry   <- (d_name dEnt >>= peekCString)
		    freeDirEnt dEnt
		    entries <- loop ptr_dEnt dir
		    return (entry:entries)
	 else do errno <- getErrno
		 if (errno == eINTR) then loop ptr_dEnt dir else do
		 let (Errno eo) = errno
		 if (eo == end_of_dir)
		    then return []
		    else throwErrno desc



{- |If the operating system has a notion of current directories,
'getCurrentDirectory' returns an absolute path to the
current directory of the calling process.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
There is no path referring to the current directory.
@[EPERM, ENOENT, ESTALE...]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.

* 'UnsupportedOperation'
The operating system has no notion of current directory.

-}

getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
  p <- mallocBytes long_path_size
  go p long_path_size
  where go p bytes = do
    	  p' <- c_getcwd p (fromIntegral bytes)
	  if p' /= nullPtr 
	     then do s <- peekCString p'
		     free p'
		     return s
	     else do errno <- getErrno
		     if errno == eRANGE
		        then do let bytes' = bytes * 2
			        p' <- reallocBytes p bytes'
			        go p' bytes'
		        else throwErrno "getCurrentDirectory"

{- |If the operating system has a notion of current directories,
@'setCurrentDirectory' dir@ changes the current
directory of the calling process to /dir/.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The directory does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'UnsupportedOperation'
The operating system has no notion of current directory, or the
current directory cannot be dynamically changed.

* 'InappropriateType'
The path refers to an existing non-directory object.
@[ENOTDIR]@

-}

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory path = do
  modifyIOError (`ioeSetFileName` path) $
    withCString path $ \s -> 
       throwErrnoIfMinus1Retry_ "setCurrentDirectory" (c_chdir s)
	-- ToDo: add path to error

{- |The operation 'doesDirectoryExist' returns 'True' if the argument file
exists and is a directory, and 'False' otherwise.
-}

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist name = 
 catch
   (withFileStatus "doesDirectoryExist" name $ \st -> isDirectory st)
   (\ _ -> return False)

{- |The operation 'doesFileExist' returns 'True'
if the argument file exists and is not a directory, and 'False' otherwise.
-}

doesFileExist :: FilePath -> IO Bool
doesFileExist name = do 
 catch
   (withFileStatus "doesFileExist" name $ \st -> do b <- isDirectory st; return (not b))
   (\ _ -> return False)

{- |The 'getModificationTime' operation returns the
clock time at which the file or directory was last modified.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to access
  the modification time; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}

getModificationTime :: FilePath -> IO ClockTime
getModificationTime name =
 withFileStatus "getModificationTime" name $ \ st ->
 modificationTime st

withFileStatus :: String -> FilePath -> (Ptr CStat -> IO a) -> IO a
withFileStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes sizeof_stat $ \p ->
      withCString (fileNameEndClean name) $ \s -> do
        throwErrnoIfMinus1Retry_ loc (c_stat s p)
	f p

withFileOrSymlinkStatus :: String -> FilePath -> (Ptr CStat -> IO a) -> IO a
withFileOrSymlinkStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes sizeof_stat $ \p ->
      withCString name $ \s -> do
        throwErrnoIfMinus1Retry_ loc (lstat s p)
	f p

modificationTime :: Ptr CStat -> IO ClockTime
modificationTime stat = do
    mtime <- st_mtime stat
    let realToInteger = round . realToFrac :: Real a => a -> Integer
    return (TOD (realToInteger (mtime :: CTime)) 0)
    
isDirectory :: Ptr CStat -> IO Bool
isDirectory stat = do
  mode <- st_mode stat
  return (s_isdir mode)

fileNameEndClean :: String -> String
fileNameEndClean name = 
  if i > 0 && (ec == '\\' || ec == '/') then 
     fileNameEndClean (take i name)
   else
     name
  where
      i  = (length name) - 1
      ec = name !! i

foreign import ccall unsafe "__hscore_R_OK" r_OK :: CMode
foreign import ccall unsafe "__hscore_W_OK" w_OK :: CMode
foreign import ccall unsafe "__hscore_X_OK" x_OK :: CMode

foreign import ccall unsafe "__hscore_S_IRUSR" s_IRUSR :: CMode
foreign import ccall unsafe "__hscore_S_IWUSR" s_IWUSR :: CMode
foreign import ccall unsafe "__hscore_S_IXUSR" s_IXUSR :: CMode

foreign import ccall unsafe "__hscore_long_path_size"
  long_path_size :: Int

#else
long_path_size :: Int
long_path_size = 2048	/* guess? */

#endif /* __GLASGOW_HASKELL__ */

{- | Returns the current user's home directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getAppUserDataDirectory'
instead.

On Unix, 'getHomeDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be 
@C:/Documents And Settings/user@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of home directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getHomeDirectory :: IO FilePath
getHomeDirectory =
#if defined(mingw32_HOST_OS)
  allocaBytes long_path_size $ \pPath -> do
     r <- c_SHGetFolderPath nullPtr csidl_PROFILE nullPtr 0 pPath
     if (r < 0)
       then do
          r <- c_SHGetFolderPath nullPtr csidl_WINDOWS nullPtr 0 pPath
	  when (r < 0) (raiseUnsupported "System.Directory.getHomeDirectory")
       else return ()
     peekCString pPath
#else
  getEnv "HOME"
#endif

{- | Returns the pathname of a directory in which application-specific
data for the current user can be stored.  The result of
'getAppUserDataDirectory' for a given application is specific to
the current user.

The argument should be the name of the application, which will be used
to construct the pathname (so avoid using unusual characters that
might result in an invalid pathname).

Note: the directory may not actually exist, and may need to be created
first.  It is expected that the parent directory exists and is
writable.

On Unix, this function returns @$HOME\/.appName@.  On Windows, a
typical path might be 

> C:/Documents And Settings/user/Application Data/appName

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of application-specific data directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getAppUserDataDirectory :: String -> IO FilePath
getAppUserDataDirectory appName = do
#if defined(mingw32_HOST_OS)
  allocaBytes long_path_size $ \pPath -> do
     r <- c_SHGetFolderPath nullPtr csidl_APPDATA nullPtr 0 pPath
     when (r<0) (raiseUnsupported "System.Directory.getAppUserDataDirectory")
     s <- peekCString pPath
     return (s++'\\':appName)
#else
  path <- getEnv "HOME"
  return (path++'/':'.':appName)
#endif

{- | Returns the current user's document directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getAppUserDataDirectory'
instead.

On Unix, 'getUserDocumentsDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be 
@C:\/Documents and Settings\/user\/My Documents@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of document directory.

* 'isDoesNotExistError'
The document directory for the current user does not exist, or
cannot be found.
-}
getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = do
#if defined(mingw32_HOST_OS)
  allocaBytes long_path_size $ \pPath -> do
     r <- c_SHGetFolderPath nullPtr csidl_PERSONAL nullPtr 0 pPath
     when (r<0) (raiseUnsupported "System.Directory.getUserDocumentsDirectory")
     peekCString pPath
#else
  getEnv "HOME"
#endif

{- | Returns the current directory for temporary files.

On Unix, 'getTemporaryDirectory' returns the value of the @TMPDIR@
environment variable or \"\/tmp\" if the variable isn\'t defined.
On Windows, the function checks for the existence of environment variables in 
the following order and uses the first path found:

* 
TMP environment variable. 

*
TEMP environment variable. 

*
USERPROFILE environment variable. 

*
The Windows directory

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of temporary directory.

The function doesn\'t verify whether the path exists.
-}
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = do
#if defined(mingw32_HOST_OS)
  allocaBytes long_path_size $ \pPath -> do
     r <- c_GetTempPath (fromIntegral long_path_size) pPath
     peekCString pPath
#else
  catch (getEnv "TMPDIR") (\ex -> return "/tmp")
#endif

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "__hscore_getFolderPath"
            c_SHGetFolderPath :: Ptr () 
                              -> CInt 
                              -> Ptr () 
                              -> CInt 
                              -> CString 
                              -> IO CInt
foreign import ccall unsafe "__hscore_CSIDL_PROFILE"  csidl_PROFILE  :: CInt
foreign import ccall unsafe "__hscore_CSIDL_APPDATA"  csidl_APPDATA  :: CInt
foreign import ccall unsafe "__hscore_CSIDL_WINDOWS"  csidl_WINDOWS  :: CInt
foreign import ccall unsafe "__hscore_CSIDL_PERSONAL" csidl_PERSONAL :: CInt

foreign import stdcall unsafe "GetTempPathA" c_GetTempPath :: CInt -> CString -> IO CInt

raiseUnsupported loc = 
   ioException (IOError Nothing UnsupportedOperation loc "unsupported operation" Nothing)

#endif

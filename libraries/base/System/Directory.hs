-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
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
    , removeDirectory		-- :: FilePath -> IO ()
    , renameDirectory		-- :: FilePath -> FilePath -> IO ()

    , getDirectoryContents      -- :: FilePath -> IO [FilePath]
    , getCurrentDirectory       -- :: IO FilePath
    , setCurrentDirectory       -- :: FilePath -> IO ()

    -- * Actions on files
    , removeFile		-- :: FilePath -> IO ()
    , renameFile                -- :: FilePath -> FilePath -> IO ()

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

#ifdef __NHC__
import Directory
#elif defined(__HUGS__)
import Hugs.Directory
#else

import Prelude

import Control.Exception       ( bracket )
import System.Posix.Types
import System.Time             ( ClockTime(..) )
import System.IO
import System.IO.Error
import Foreign
import Foreign.C

#ifdef __GLASGOW_HASKELL__
import System.Posix.Internals
import GHC.IOBase	( IOException(..), IOErrorType(..), ioException )
#endif

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
    let
     read  = if r      then s_IRUSR else emptyCMode
     write = if w      then s_IWUSR else emptyCMode
     exec  = if e || s then s_IXUSR else emptyCMode

     mode  = read `unionCMode` (write `unionCMode` exec)

    withCString name $ \s ->
      throwErrnoIfMinus1_ "setPermissions" $ c_chmod s mode

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
    withCString path $ \s -> do
      throwErrnoIfMinus1Retry_ "createDirectory" $
	mkdir s 0o777

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
    return (TOD (toInteger (mtime :: CTime)) 0)
    
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

emptyCMode     :: CMode
emptyCMode     = 0

unionCMode     :: CMode -> CMode -> CMode
unionCMode     = (+)


foreign import ccall unsafe "__hscore_long_path_size"
  long_path_size :: Int

foreign import ccall unsafe "__hscore_R_OK" r_OK :: CMode
foreign import ccall unsafe "__hscore_W_OK" w_OK :: CMode
foreign import ccall unsafe "__hscore_X_OK" x_OK :: CMode

foreign import ccall unsafe "__hscore_S_IRUSR" s_IRUSR :: CMode
foreign import ccall unsafe "__hscore_S_IWUSR" s_IWUSR :: CMode
foreign import ccall unsafe "__hscore_S_IXUSR" s_IXUSR :: CMode

#endif

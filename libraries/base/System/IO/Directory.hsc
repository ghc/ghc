-- -----------------------------------------------------------------------------
-- $Id: Directory.hsc,v 1.1 2001/06/28 14:15:04 simonmar Exp $
--
-- (c) The University of Glasgow, 1994-2000
--

-- The Directory Interface

{-
A directory contains a series of entries, each of which is a named
reference to a file system object (file, directory etc.).  Some
entries may be hidden, inaccessible, or have some administrative
function (e.g. "." or ".." under POSIX), but in this standard all such
entries are considered to form part of the directory contents.
Entries in sub-directories are not, however, considered to form part
of the directory contents.

Each file system object is referenced by a {\em path}.  There is
normally at least one absolute path to each file system object.  In
some operating systems, it may also be possible to have paths which
are relative to the current directory.
-}

module System.IO.Directory 
   ( 
      Permissions               -- abstract
      
    , readable                  -- :: Permissions -> Bool
    , writable                  -- :: Permissions -> Bool
    , executable                -- :: Permissions -> Bool
    , searchable                -- :: Permissions -> Bool

    , createDirectory		-- :: FilePath -> IO ()
    , removeDirectory		-- :: FilePath -> IO ()
    , renameDirectory		-- :: FilePath -> FilePath -> IO ()

    , getDirectoryContents      -- :: FilePath -> IO [FilePath]
    , getCurrentDirectory       -- :: IO FilePath
    , setCurrentDirectory       -- :: FilePath -> IO ()

    , removeFile		-- :: FilePath -> IO ()
    , renameFile                -- :: FilePath -> FilePath -> IO ()

    , doesFileExist		-- :: FilePath -> IO Bool
    , doesDirectoryExist        -- :: FilePath -> IO Bool

    , getPermissions            -- :: FilePath -> IO Permissions
    , setPermissions	        -- :: FilePath -> Permissions -> IO ()

    , getModificationTime       -- :: FilePath -> IO ClockTime
   ) where

import Prelude

import System.Time             ( ClockTime(..) )
import System.IO
import Foreign
import Foreign.C

#ifdef __GLASGOW_HASKELL__
import GHC.Posix
import GHC.IOBase	( IOException(..), IOErrorType(..), ioException )
#endif

-- to get config.h
#include "HsCore.h"

#include <sys/stat.h>
#include <dirent.h>
#include <limits.h>
#include <errno.h>
#include <unistd.h>

-----------------------------------------------------------------------------
-- Permissions

-- The Permissions type is used to record whether certain
-- operations are permissible on a file/directory:
-- [to whom? - presumably the "current user"]

data Permissions
 = Permissions {
    readable,   writable, 
    executable, searchable :: Bool 
   } deriving (Eq, Ord, Read, Show)

-----------------------------------------------------------------------------
-- Implementation

-- `createDirectory dir' creates a new directory dir which is
-- initially empty, or as near to empty as the operating system
-- allows.

-- The operation may fail with:

{-
\begin{itemize}
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES]@
\item @isAlreadyExistsError@ / @AlreadyExists@
The operand refers to a directory that already exists.  
@ [EEXIST]@
\item @HardwareFault@
A physical I/O error has occurred.
@ [EIO]@
\item @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@
\item @NoSuchThing@
There is no path to the directory. 
@[ENOENT, ENOTDIR]@
\item @ResourceExhausted@
Insufficient resources (virtual memory, process file descriptors,
physical disk space, etc.) are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
\item @InappropriateType@
The path refers to an existing non-directory object.
@[EEXIST]@
\end{itemize}
-}

createDirectory :: FilePath -> IO ()
createDirectory path = do
    withUnsafeCString path $ \s -> do
      throwErrnoIfMinus1Retry_ "createDirectory" $
#if defined(mingw32_TARGET_OS)
        mkdir s
#else
        mkdir s 0o777
#endif

{-
@removeDirectory dir@ removes an existing directory {\em dir}.  The
implementation may specify additional constraints which must be
satisfied before a directory can be removed (e.g. the directory has to
be empty, or may not be in use by other processes).  It is not legal
for an implementation to partially remove a directory unless the
entire directory is removed. A conformant implementation need not
support directory removal in all situations (e.g. removal of the root
directory).

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
[@EIO@]
\item @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExist@ / @NoSuchThing@
The directory does not exist. 
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@
\item @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.  
@[EBUSY, ENOTEMPTY, EEXIST]@
\item @UnsupportedOperation@
The implementation does not support removal in this situation.
@[EINVAL]@
\item @InappropriateType@
The operand refers to an existing non-directory object.
@[ENOTDIR]@
\end{itemize}
-}

removeDirectory :: FilePath -> IO ()
removeDirectory path = do
    withUnsafeCString path $ \s ->
       throwErrnoIfMinus1Retry_ "removeDirectory" (rmdir s)

{-
@Removefile file@ removes the directory entry for an existing file
{\em file}, where {\em file} is not itself a directory. The
implementation may specify additional constraints which must be
satisfied before a file can be removed (e.g. the file may not be in
use by other processes).

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @InvalidArgument@
The operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExist@ / @NoSuchThing@
The file does not exist. 
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@
\item @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.  
@[EBUSY]@
\item @InappropriateType@
The operand refers to an existing directory.
@[EPERM, EINVAL]@
\end{itemize}
-}

removeFile :: FilePath -> IO ()
removeFile path = do
    withUnsafeCString path $ \s ->
      throwErrnoIfMinus1Retry_ "removeFile" (unlink s)

{-
@renameDirectory@ {\em old} {\em new} changes the name of an existing
directory from {\em old} to {\em new}.  If the {\em new} directory
already exists, it is atomically replaced by the {\em old} directory.
If the {\em new} directory is neither the {\em old} directory nor an
alias of the {\em old} directory, it is removed as if by
$removeDirectory$.  A conformant implementation need not support
renaming directories in all situations (e.g. renaming to an existing
directory, or across different physical devices), but the constraints
must be documented.

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @InvalidArgument@
Either operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExistError@ / @NoSuchThing@
The original directory does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@
\item @ResourceExhausted@
Insufficient resources are available to perform the operation.  
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
\item @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@
\item @UnsupportedOperation@
The implementation does not support renaming in this situation.
@[EINVAL, EXDEV]@
\item @InappropriateType@
Either path refers to an existing non-directory object.
@[ENOTDIR, EISDIR]@
\end{itemize}
-}

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory opath npath =
   withFileStatus opath $ \st -> do
   is_dir <- isDirectory st
   if (not is_dir)
	then ioException (IOError Nothing InappropriateType "renameDirectory"
			    ("not a directory") (Just opath))
	else do

   withUnsafeCString opath $ \s1 ->
     withUnsafeCString npath $ \s2 ->
        throwErrnoIfMinus1Retry_ "renameDirectory" (rename s1 s2)

{-
@renameFile@ {\em old} {\em new} changes the name of an existing file system
object from {\em old} to {\em new}.  If the {\em new} object already
exists, it is atomically replaced by the {\em old} object.  Neither
path may refer to an existing directory.  A conformant implementation
need not support renaming files in all situations (e.g. renaming
across different physical devices), but the constraints must be
documented.

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @InvalidArgument@
Either operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExistError@ / @NoSuchThing@
The original file does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@
\item @ResourceExhausted@
Insufficient resources are available to perform the operation.  
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
\item @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY]@
\item @UnsupportedOperation@
The implementation does not support renaming in this situation.
@[EXDEV]@
\item @InappropriateType@
Either path refers to an existing directory.
@[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@
\end{itemize}
-}

renameFile :: FilePath -> FilePath -> IO ()
renameFile opath npath =
   withFileStatus opath $ \st -> do
   is_dir <- isDirectory st
   if is_dir
	then ioException (IOError Nothing InappropriateType "renameFile"
			   "is a directory" (Just opath))
	else do

    withUnsafeCString opath $ \s1 ->
      withUnsafeCString npath $ \s2 ->
         throwErrnoIfMinus1Retry_ "renameFile" (rename s1 s2)

{-
@getDirectoryContents dir@ returns a list of {\em all} entries
in {\em dir}. 

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExistError@ / @NoSuchThing@
The directory does not exist.
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EACCES]@
\item @ResourceExhausted@
Insufficient resources are available to perform the operation.
@[EMFILE, ENFILE]@
\item @InappropriateType@
The path refers to an existing non-directory object.
@[ENOTDIR]@
\end{itemize}
-}

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = do
   p <- withUnsafeCString path $ \s ->
	  throwErrnoIfNullRetry "getDirectoryContents" (opendir s)
   loop p
  where
    loop :: Ptr CDir -> IO [String]
    loop dir = do
      resetErrno
      p <- readdir dir
      if (p /= nullPtr)
	 then do
#ifdef mingw32_TARGET_OS
                 entryp <- (#peek struct dirent,d_name) p
                 entry <- peekCString entryp -- on mingwin it's a char *, not a char []
#else
                 entry <- peekCString ((#ptr struct dirent,d_name) p)
#endif
		 entries <- loop dir
		 return (entry:entries)
	 else do errno <- getErrno
		 if (errno == eINTR) then loop dir else do
		 throwErrnoIfMinus1_ "getDirectoryContents" $ closedir dir
#ifdef mingw32_TARGET_OS
		 if (errno == eNOENT) -- mingwin (20001111) cunningly sets errno to ENOENT when it runs out of files
#else
		 if (errno == eOK)
#endif
		    then return []
		    else throwErrno "getDirectoryContents"

{-
If the operating system has a notion of current directories,
@getCurrentDirectory@ returns an absolute path to the
current directory of the calling process.

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @isDoesNotExistError@ / @NoSuchThing@
There is no path referring to the current directory.
@[EPERM, ENOENT, ESTALE...]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EACCES]@
\item @ResourceExhausted@
Insufficient resources are available to perform the operation.
\item @UnsupportedOperation@
The operating system has no notion of current directory.
\end{itemize}
-}

getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
  p <- mallocBytes (#const PATH_MAX)
  go p (#const PATH_MAX)
  where go p bytes = do
    	  p' <- getcwd p (fromIntegral bytes)
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

{-
If the operating system has a notion of current directories,
@setCurrentDirectory dir@ changes the current
directory of the calling process to {\em dir}.

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExistError@ / @NoSuchThing@
The directory does not exist.
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EACCES]@
\item @UnsupportedOperation@
The operating system has no notion of current directory, or the
current directory cannot be dynamically changed.
\item @InappropriateType@
The path refers to an existing non-directory object.
@[ENOTDIR]@
\end{itemize}
-}

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory path = do
    withUnsafeCString path $ \s -> 
       throwErrnoIfMinus1Retry_ "setCurrentDirectory" (chdir s)
	-- ToDo: add path to error

{-
To clarify, @doesDirectoryExist@ returns True if a file system object
exist, and it's a directory. @doesFileExist@ returns True if the file
system object exist, but it's not a directory (i.e., for every other 
file system object that is not a directory.) 
-}

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist name = 
 catch
   (withFileStatus name $ \st -> isDirectory st)
   (\ _ -> return False)

doesFileExist :: FilePath -> IO Bool
doesFileExist name = do 
 catch
   (withFileStatus name $ \st -> do b <- isDirectory st; return (not b))
   (\ _ -> return False)

getModificationTime :: FilePath -> IO ClockTime
getModificationTime name =
 withFileStatus name $ \ st ->
 modificationTime st

getPermissions :: FilePath -> IO Permissions
getPermissions name = do
  withUnsafeCString name $ \s -> do
  read  <- access s (#const R_OK)
  write <- access s (#const W_OK)
  exec  <- access s (#const X_OK)
  withFileStatus name $ \st -> do
  is_dir <- isDirectory st
  is_reg <- isRegularFile st
  return (
    Permissions {
      readable   = read  == 0,
      writable   = write == 0,
      executable = not is_dir && exec == 0,
      searchable = not is_reg && exec == 0
    }
   )

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions name (Permissions r w e s) = do
    let
     read  = if r      then (#const S_IRUSR) else emptyCMode
     write = if w      then (#const S_IWUSR) else emptyCMode
     exec  = if e || s then (#const S_IXUSR) else emptyCMode

     mode  = read `unionCMode` (write `unionCMode` exec)

    withUnsafeCString name $ \s ->
      throwErrnoIfMinus1_ "setPermissions" $ chmod s mode

withFileStatus :: FilePath -> (Ptr CStat -> IO a) -> IO a
withFileStatus name f = do
    allocaBytes (#const sizeof(struct stat)) $ \p ->
      withUnsafeCString name $ \s -> do
        throwErrnoIfMinus1Retry_ "withFileStatus" (stat s p)
	f p

modificationTime :: Ptr CStat -> IO ClockTime
modificationTime stat = do
    mtime <- (#peek struct stat, st_mtime) stat
    return (TOD (toInteger (mtime :: CTime)) 0)

isDirectory :: Ptr CStat -> IO Bool
isDirectory stat = do
  mode <- (#peek struct stat, st_mode) stat
  return (s_ISDIR mode /= 0)

isRegularFile :: Ptr CStat -> IO Bool
isRegularFile stat = do
  mode <- (#peek struct stat, st_mode) stat
  return (s_ISREG mode /= 0)

foreign import ccall unsafe s_ISDIR :: CMode -> Int
#def inline HsInt s_ISDIR(m) {return S_ISDIR(m);}

foreign import ccall unsafe s_ISREG :: CMode -> Int
#def inline HsInt s_ISREG(m) {return S_ISREG(m);}

emptyCMode     :: CMode
emptyCMode     = 0

unionCMode     :: CMode -> CMode -> CMode
unionCMode     = (+)

type UCString = UnsafeCString

#if defined(mingw32_TARGET_OS)
foreign import ccall unsafe mkdir    :: UCString -> IO CInt
#else
foreign import ccall unsafe mkdir    :: UCString -> CInt -> IO CInt
#endif

foreign import ccall unsafe chmod    :: UCString -> CMode -> IO CInt
foreign import ccall unsafe access   :: UCString -> CMode -> IO CInt
foreign import ccall unsafe rmdir    :: UCString -> IO CInt
foreign import ccall unsafe chdir    :: UCString -> IO CInt
foreign import ccall unsafe getcwd   :: Ptr CChar -> CInt -> IO (Ptr CChar)
foreign import ccall unsafe unlink   :: UCString -> IO CInt
foreign import ccall unsafe rename   :: UCString -> UCString -> IO CInt
		     
foreign import ccall unsafe opendir  :: UCString  -> IO (Ptr CDir)
foreign import ccall unsafe readdir  :: Ptr CDir -> IO (Ptr CDirent)
foreign import ccall unsafe closedir :: Ptr CDir -> IO CInt

foreign import ccall unsafe stat     :: UCString -> Ptr CStat -> IO CInt

type CDirent = ()

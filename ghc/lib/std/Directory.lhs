% -----------------------------------------------------------------------------
%
% (c) The University of Glasgow, 1994-
%
% The Directory Interface

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

\begin{code}
{-# OPTIONS -#include "dirUtils.h" #-}
module Directory 
   ( 
      Permissions		-- instance of (Eq, Ord, Read, Show)
	( Permissions
        , readable              -- :: Permissions -> Bool
        , writable              -- :: Permissions -> Bool
        , executable            -- :: Permissions -> Bool
        , searchable            -- :: Permissions -> Bool
	)

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

import Prelude		-- Just to get it in the dependencies

import Time             ( ClockTime(..) )

import PrelPosix
import PrelStorable
import PrelCString
import PrelMarshalAlloc
import PrelCTypesISO
import PrelCTypes
import PrelCError
import PrelPtr
import PrelIOBase
import PrelBase
\end{code}

-----------------------------------------------------------------------------
-- Permissions

The @Permissions@ type is used to record whether certain
operations are permissible on a file/directory:
[to whom? - presumably the "current user"]

\begin{code}
data Permissions
 = Permissions {
    readable,   writable, 
    executable, searchable :: Bool 
   } deriving (Eq, Ord, Read, Show)
\end{code}

-----------------------------------------------------------------------------
-- Implementation

@createDirectory dir@ creates a new directory {\em dir} which is
initially empty, or as near to empty as the operating system
allows.

The operation may fail with:

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

\begin{code}
createDirectory :: FilePath -> IO ()
createDirectory path = do
    withCString path $ \s -> do
      throwErrnoIfMinus1Retry_ "createDirectory" $
        mkdir s 0o777
\end{code}

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

\begin{code}
removeDirectory :: FilePath -> IO ()
removeDirectory path = do
    withCString path $ \s ->
       throwErrnoIfMinus1Retry_ "removeDirectory" (rmdir s)

\end{code}

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

\begin{code}
removeFile :: FilePath -> IO ()
removeFile path = do
    withCString path $ \s ->
      throwErrnoIfMinus1Retry_ "removeFile" (unlink s)

\end{code}

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

\begin{code}
renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory opath npath =
   withFileStatus opath $ \st -> do
   is_dir <- isDirectory st
   if (not is_dir)
	then ioException (IOError Nothing InappropriateType "renameDirectory"
			    ("not a directory") (Just opath))
	else do

   withCString opath $ \s1 ->
     withCString npath $ \s2 ->
        throwErrnoIfMinus1Retry_ "renameDirectory" (rename s1 s2)

\end{code}

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

\begin{code}
renameFile :: FilePath -> FilePath -> IO ()
renameFile opath npath =
   withFileOrSymlinkStatus opath $ \st -> do
   is_dir <- isDirectory st
   if is_dir
	then ioException (IOError Nothing InappropriateType "renameFile"
			   "is a directory" (Just opath))
	else do

    withCString opath $ \s1 ->
      withCString npath $ \s2 ->
         throwErrnoIfMinus1Retry_ "renameFile" (rename s1 s2)

\end{code}

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

\begin{code}
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = do
   p <- withCString path $ \s ->
	  throwErrnoIfNullRetry "getDirectoryContents" (opendir s)
   loop p
  where
    loop :: Ptr CDir -> IO [String]
    loop dir = do
      resetErrno
      p <- readdir dir
      if (p /= nullPtr)
	 then do
	 	 entry   <- (d_name p >>= peekCString)
		 entries <- loop dir
		 return (entry:entries)
	 else do errno <- getErrno
		 if (errno == eINTR) then loop dir else do
		 throwErrnoIfMinus1_ "getDirectoryContents" $ closedir dir
		 let (Errno eo) = errno
		 if (eo == end_of_dir)
		    then return []
		    else throwErrno "getDirectoryContents"

foreign import ccall "prel_end_of_dir" unsafe end_of_dir :: CInt
foreign import ccall "prel_d_name" unsafe d_name :: Ptr CDirent -> IO CString

\end{code}

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

\begin{code}
getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
  p <- mallocBytes path_max
  go p path_max
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

foreign import ccall "prel_path_max" unsafe path_max :: Int

\end{code}

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

\begin{code}
setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory path = do
    withCString path $ \s -> 
       throwErrnoIfMinus1Retry_ "setCurrentDirectory" (chdir s)
	-- ToDo: add path to error

\end{code}

To clarify, @doesDirectoryExist@ returns True if a file system object
exist, and it's a directory. @doesFileExist@ returns True if the file
system object exist, but it's not a directory (i.e., for every other 
file system object that is not a directory.) 

\begin{code}
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
  withCString name $ \s -> do
  read  <- access s r_OK
  write <- access s w_OK
  exec  <- access s x_OK
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
   
foreign import ccall "prel_R_OK" unsafe r_OK :: CMode
foreign import ccall "prel_W_OK" unsafe w_OK :: CMode
foreign import ccall "prel_X_OK" unsafe x_OK :: CMode

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions name (Permissions r w e s) = do
    let
     read  = if r      then s_IRUSR else emptyCMode
     write = if w      then s_IWUSR else emptyCMode
     exec  = if e || s then s_IXUSR else emptyCMode

     mode  = read `unionCMode` (write `unionCMode` exec)

    withCString name $ \s ->
      throwErrnoIfMinus1_ "setPermissions" $ chmod s mode

foreign import ccall "prel_S_IRUSR" unsafe s_IRUSR :: CMode
foreign import ccall "prel_S_IWUSR" unsafe s_IWUSR :: CMode
foreign import ccall "prel_S_IXUSR" unsafe s_IXUSR :: CMode

withFileStatus :: FilePath -> (Ptr CStat -> IO a) -> IO a
withFileStatus name f = do
    allocaBytes sizeof_stat $ \p ->
      withCString name $ \s -> do
        throwErrnoIfMinus1Retry_ "withFileStatus" (stat s p)
	f p

withFileOrSymlinkStatus :: FilePath -> (Ptr CStat -> IO a) -> IO a
withFileOrSymlinkStatus name f = do
    allocaBytes sizeof_stat $ \p ->
      withCString name $ \s -> do
        throwErrnoIfMinus1Retry_ "withFileOrSymlinkStatus" (lstat s p)
	f p

foreign import ccall "prel_sz_stat" unsafe sizeof_stat :: Int

modificationTime :: Ptr CStat -> IO ClockTime
modificationTime stat = do
    mtime <- st_mtime stat
    return (TOD (toInteger (mtime :: CTime)) 0)
    
foreign import ccall "prel_st_mtime" unsafe st_mtime :: Ptr CStat -> IO CTime
foreign import ccall "prel_st_mode" unsafe st_mode :: Ptr CStat -> IO CMode

isDirectory :: Ptr CStat -> IO Bool
isDirectory stat = do
  mode <- st_mode stat
  return (s_ISDIR mode /= 0)

isRegularFile :: Ptr CStat -> IO Bool
isRegularFile stat = do
  mode <- st_mode stat
  return (s_ISREG mode /= 0)

foreign import ccall "prel_s_ISDIR" unsafe s_ISDIR :: CMode -> Int
foreign import ccall "prel_s_ISREG" unsafe s_ISREG :: CMode -> Int

emptyCMode     :: CMode
emptyCMode     = 0

unionCMode     :: CMode -> CMode -> CMode
unionCMode     = (+)

foreign import ccall "prel_mkdir" unsafe mkdir    :: CString -> CInt -> IO CInt

foreign import ccall unsafe chmod    :: CString -> CMode -> IO CInt
foreign import ccall unsafe access   :: CString -> CMode -> IO CInt
foreign import ccall unsafe rmdir    :: CString -> IO CInt
foreign import ccall unsafe chdir    :: CString -> IO CInt
foreign import ccall unsafe getcwd   :: Ptr CChar -> CInt -> IO (Ptr CChar)
foreign import ccall unsafe unlink   :: CString -> IO CInt
foreign import ccall unsafe rename   :: CString -> CString -> IO CInt
		     
foreign import ccall unsafe opendir  :: CString  -> IO (Ptr CDir)
foreign import ccall unsafe readdir  :: Ptr CDir -> IO (Ptr CDirent)
foreign import ccall unsafe closedir :: Ptr CDir -> IO CInt

foreign import ccall unsafe stat     :: CString -> Ptr CStat -> IO CInt

foreign import ccall "prel_lstat" unsafe lstat :: CString -> Ptr CStat -> IO CInt

type CDirent = ()

\end{code}

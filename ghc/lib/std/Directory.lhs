%
% (c) The AQUA Project, Glasgow University, 1994-1997
%
\section[Directory]{Directory interface}

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
{-# OPTIONS -#include <sys/stat.h> -#include <dirent.h> -#include "cbits/stgio.h" #-}
module Directory 
   ( 
    Permissions(Permissions),

    createDirectory, 
    removeDirectory, 
    renameDirectory, 
    getDirectoryContents,
    getCurrentDirectory, 
    setCurrentDirectory,

    removeFile, 
    renameFile, 

    doesFileExist,
    doesDirectoryExist,
    getPermissions, 
    setPermissions,
#ifndef __HUGS__
    getModificationTime
#endif
   ) where

#ifdef __HUGS__
import PreludeBuiltin
#else
import PrelBase
import PrelIOBase
import PrelHandle	
import PrelST
import PrelArr
import PrelPack		( unpackNBytesST )
import PrelAddr
import Time             ( ClockTime(..) )
#endif

\end{code}

%*********************************************************
%*							*
\subsection{Signatures}
%*							*
%*********************************************************

\begin{code}
createDirectory 	:: FilePath -> IO ()
removeDirectory 	:: FilePath -> IO ()
removeFile 		:: FilePath -> IO ()
renameDirectory 	:: FilePath -> FilePath -> IO ()
renameFile 		:: FilePath -> FilePath -> IO ()
getDirectoryContents 	:: FilePath -> IO [FilePath]
getCurrentDirectory 	:: IO FilePath
setCurrentDirectory 	:: FilePath -> IO ()
doesFileExist           :: FilePath -> IO Bool
doesDirectoryExist      :: FilePath -> IO Bool
getPermissions          :: FilePath -> IO Permissions
setPermissions          :: FilePath -> Permissions -> IO ()
#ifndef __HUGS__
getModificationTime     :: FilePath -> IO ClockTime
#endif
\end{code}

\begin{code}
#ifdef __HUGS__
foreign import stdcall "libHS_cbits.so" "createDirectory"  	primCreateDirectory     :: CString -> IO Int
foreign import stdcall "libHS_cbits.so" "removeDirectory"  	primRemoveDirectory     :: CString -> IO Int
foreign import stdcall "libHS_cbits.so" "removeFile"       	primRemoveFile          :: CString -> IO Int
foreign import stdcall "libHS_cbits.so" "renameDirectory"  	primRenameDirectory     :: CString -> CString -> IO Int
foreign import stdcall "libHS_cbits.so" "renameFile"       	primRenameFile          :: CString -> CString -> IO Int
foreign import stdcall "libHS_cbits.so" "openDir__"        	primOpenDir     	:: CString -> IO Addr
foreign import stdcall "libHS_cbits.so" "readDir__"        	primReadDir     	:: Addr -> IO Addr
foreign import stdcall "libHS_cbits.so" "get_dirent_d_name"   primGetDirentDName      :: Addr -> IO Addr
foreign import stdcall "libHS_cbits.so" "setCurrentDirectory" primSetCurrentDirectory :: CString -> IO Int
foreign import stdcall "libHS_cbits.so" "getCurrentDirectory" primGetCurrentDirectory :: IO Addr
foreign import stdcall "libc.so.6"        "free"                primFree                :: Addr -> IO ()
foreign import stdcall "libc.so.6"        "malloc"              primMalloc              :: Word -> IO Addr
foreign import stdcall "libc.so.6"        "chmod"               primChmod               :: CString -> Word -> IO Int
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Permissions}
%*							*
%*********************************************************

The @Permissions@ type is used to record whether certain
operations are permissible on a file/directory:
[to whom? - owner/group/world - the Report don't say much]

\begin{code}
data Permissions
 = Permissions {
    readable,   writeable, 
    executable, searchable :: Bool 
   } deriving (Eq, Ord, Read, Show)
\end{code}

%*********************************************************
%*							*
\subsection{Implementation}
%*							*
%*********************************************************

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

createDirectory path = do
#ifdef __HUGS__
    rc <- primCreateDirectory (primPackString path)
#else
    rc <- _ccall_ createDirectory path
#endif
    if rc == 0 then return () else
        constructErrorAndFailWithInfo "createDirectory" path
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
removeDirectory path = do
#ifdef __HUGS__
    rc <- primRemoveDirectory (primPackString path)
#else
    rc <- _ccall_ removeDirectory path
#endif
    if rc == 0 then 
	return ()
     else 
        constructErrorAndFailWithInfo "removeDirectory" path
\end{code}

@removeFile file@ removes the directory entry for an existing file
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
removeFile path = do
#ifdef __HUGS__
    rc <- primRemoveFile (primPackString path)
#else
    rc <- _ccall_ removeFile path
#endif
    if rc == 0 then
        return ()
     else
        constructErrorAndFailWithInfo "removeFile" path
\end{code}

@renameDirectory old@ {\em new} changes the name of an existing
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
renameDirectory opath npath = do
#ifdef __HUGS__
    rc <- primRenameDirectory (primPackString opath) (primPackString npath)
#else
    rc <- _ccall_ renameDirectory opath npath
#endif
    if rc == 0 then
        return ()
     else
        constructErrorAndFailWithInfo "renameDirectory" ("old: " ++ opath ++ ",new: " ++ npath)
\end{code}

@renameFile old@ {\em new} changes the name of an existing file system
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
renameFile opath npath = do
#ifdef __HUGS__
    rc <- primRenameFile (primPackString opath) (primPackString npath)
#else
    rc <- _ccall_ renameFile opath npath
#endif
    if rc == 0 then
        return ()
     else
        constructErrorAndFailWithInfo	"renameFile" opath
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
--getDirectoryContents :: FilePath -> IO [FilePath]
#ifdef __HUGS__
getDirectoryContents path = do
    dir <- primOpenDir (primPackString path)
    if dir == nullAddr
	then constructErrorAndFailWithInfo "getDirectoryContents" path
     	else loop dir
  where
    loop :: Addr -> IO [String]
    loop dir  = do
      dirent_ptr <- primReadDir dir
      if dirent_ptr == nullAddr
       then do
	  -- readDir__ implicitly performs closedir() when the
	  -- end is reached.
	  return [] 
       else do
          str     <- primGetDirentDName dirent_ptr
	  entry   <- primUnpackCString str
	  entries <- loop dir
          return (entry:entries)
#else
getDirectoryContents path = do
    dir <- _ccall_ openDir__ path
    if dir == ``NULL'' 
	then constructErrorAndFailWithInfo "getDirectoryContents" path
     	else loop dir
  where
    loop :: Addr -> IO [String]
    loop dir  = do
      dirent_ptr <- _ccall_ readDir__ dir
      if (dirent_ptr::Addr) == ``NULL'' 
       then do
	  -- readDir__ implicitly performs closedir() when the
	  -- end is reached.
	  return [] 
       else do
          str     <- _casm_ `` %r=(char*)((struct dirent*)%0)->d_name; '' dirent_ptr
	    -- not using the unpackCString function here, since we have to force
	    -- the unmarshalling of the directory entry right here as subsequent
	    -- calls to readdir() may overwrite it.
          len     <- _ccall_ strlen str
	  entry   <- stToIO (unpackNBytesST str len)
	  entries <- loop dir
          return (entry:entries)
#endif
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
getCurrentDirectory = do
#ifdef __HUGS__
    str <- primGetCurrentDirectory
#else
    str <- _ccall_ getCurrentDirectory
#endif
    if str /= nullAddr
	then do
#ifdef __HUGS__
            pwd <- primUnpackCString str
            primFree str
#else
	   	-- don't use unpackCString (see getDirectoryContents above)
            len <- _ccall_ strlen str
	    pwd <- stToIO (unpackNBytesST str len)
	    _ccall_ free str
#endif
            return pwd
    	else
            constructErrorAndFail "getCurrentDirectory"
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
setCurrentDirectory path = do
#ifdef __HUGS__
    rc <- primSetCurrentDirectory (primPackString path)
#else
    rc <- _ccall_ setCurrentDirectory path
#endif
    if rc == 0 
	then return ()
	else constructErrorAndFailWithInfo "setCurrentDirectory" path
\end{code}


\begin{code}
--doesFileExist :: FilePath -> IO Bool
#ifdef __HUGS__
foreign import stdcall "libc.so.6"        "access"     primAccess  :: PrimByteArray -> Int -> IO Int
foreign import stdcall "libHS_cbits.so" "const_F_OK" const_F_OK  :: Int

doesFileExist name = do 
  rc <- primAccess (primPackString name) const_F_OK
  return (rc == 0)
#else
doesFileExist name = do 
  rc <- _ccall_ access name (``F_OK''::Int)
  return (rc == 0)
#endif

--doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist name = 
 (getFileStatus name >>= \ st -> return (isDirectory st))  
   `catch` 
 (\ _ -> return False)

#ifndef __HUGS__
--getModificationTime :: FilePath -> IO ClockTime
getModificationTime name =
 getFileStatus name >>= \ st ->
 modificationTime st
#endif

--getPermissions :: FilePath -> IO Permissions
getPermissions name =
  getFileStatus name >>= \ st ->
  let
   fm = fileMode st
   isect v = intersectFileMode v fm == v
  in
  return (
    Permissions {
      readable   = isect ownerReadMode,
      writeable  = isect ownerWriteMode,
      executable = not (isDirectory st)   && isect ownerExecuteMode,
      searchable = not (isRegularFile st) && isect ownerExecuteMode
    }
  )

--setPermissions :: FilePath -> Permissions -> IO ()
#ifdef __HUGS__
setPermissions name (Permissions r w e s) = do
    let
     read  = if r      then ownerReadMode    else emptyFileMode
     write = if w      then ownerWriteMode   else emptyFileMode
     exec  = if e || s then ownerExecuteMode else emptyFileMode

     mode  = read `unionFileMode` (write `unionFileMode` exec)

    rc <- primChmod (primPackString name) mode
    if rc == 0
	then return ()
	else fail (IOError Nothing SystemError "setPermissions" "insufficient permissions")
#else
setPermissions name (Permissions r w e s) = do
    let
     read#  = case (if r then ownerReadMode else ``0'') of { W# x# -> x# }
     write# = case (if w then ownerWriteMode else ``0'') of { W# x# -> x# }
     exec#  = case (if e || s then ownerExecuteMode else ``0'') of { W# x# -> x# }

     mode  = I# (word2Int# (read# `or#` write# `or#` exec#))

    rc <- _ccall_ chmod name mode
    if rc == 0
	then return ()
	else fail (IOError Nothing SystemError "setPermissions" "insufficient permissions")
#endif
\end{code}


(Sigh)..copied from Posix.Files to avoid dep. on posix library

\begin{code}
#ifdef __HUGS__
foreign import stdcall "libHS_cbits.so" "sizeof_stat" sizeof_stat :: Int
foreign import stdcall "libHS_cbits.so" "prim_stat"   primStat :: PrimByteArray -> PrimMutableByteArray RealWorld -> IO Int

type FileStatus = PrimByteArray

getFileStatus :: FilePath -> IO FileStatus
getFileStatus name = do
    bytes <- primNewByteArray sizeof_stat
    rc <- primStat (primPackString name) bytes
    if rc == 0 
	then primUnsafeFreezeByteArray bytes
     	else fail (IOError Nothing SystemError "getFileStatus" "")
#else
type FileStatus = ByteArray Int

getFileStatus :: FilePath -> IO FileStatus
getFileStatus name = do
    bytes <- stToIO (newCharArray (0,``sizeof(struct stat)''))
    rc <- _casm_ ``%r = stat(%0,(struct stat *)%1);'' name bytes
    if rc == 0 
	then stToIO (unsafeFreezeByteArray bytes)
     	else fail (IOError Nothing SystemError "getFileStatus" "")

modificationTime :: FileStatus -> IO ClockTime
modificationTime stat = do
    i1 <- malloc1
    _casm_ ``((unsigned long *)%1)[0] = ((struct stat *)%0)->st_mtime;'' stat i1
    secs <- cvtUnsigned i1
    return (TOD secs 0)
  where
    malloc1 = IO $ \ s# ->
	case newIntArray# 1# s# of 
          (# s2#, barr# #) -> (# s2#, MutableByteArray bnds barr# #)

    bnds = (0,1)
    -- The C routine fills in an unsigned word.  We don't have `unsigned2Integer#,'
    -- so we freeze the data bits and use them for an MP_INT structure.  Note that
    -- zero is still handled specially, although (J# 1# 1# (ptr to 0#)) is probably
    -- acceptable to gmp.

    cvtUnsigned (MutableByteArray _ arr#) = IO $ \ s# ->
	case readIntArray# arr# 0# s# of 
	  (# s2#, r# #) ->
            if r# ==# 0# then
                (# s2#, 0 #)
            else
                case unsafeFreezeByteArray# arr# s2# of
                  (# s3#, frozen# #) -> 
			(# s3#, J# 1# 1# frozen# #)
#endif

#ifdef __HUGS__
foreign import stdcall "libHS_cbits.so" "get_stat_st_mode" fileMode     :: FileStatus -> FileMode
foreign import stdcall "libHS_cbits.so" "prim_S_ISDIR"     prim_S_ISDIR :: FileMode -> Int
foreign import stdcall "libHS_cbits.so" "prim_S_ISREG"     prim_S_ISREG :: FileMode -> Int

isDirectory :: FileStatus -> Bool
isDirectory stat = prim_S_ISDIR (fileMode stat) /= 0

isRegularFile :: FileStatus -> Bool
isRegularFile stat = prim_S_ISREG (fileMode stat) /= 0
#else
isDirectory :: FileStatus -> Bool
isDirectory stat = unsafePerformIO $ do
    rc <- _casm_ ``%r = S_ISDIR(((struct stat *)%0)->st_mode);'' stat
    return (rc /= 0)

isRegularFile :: FileStatus -> Bool
isRegularFile stat = unsafePerformIO $ do
    rc <- _casm_ ``%r = S_ISREG(((struct stat *)%0)->st_mode);'' stat
    return (rc /= 0)
#endif
\end{code}

\begin{code}
type FileMode = Word

#ifdef __HUGS__
emptyFileMode     :: FileMode
unionFileMode     :: FileMode -> FileMode -> FileMode
intersectFileMode :: FileMode -> FileMode -> FileMode

foreign import stdcall "libHS_cbits.so" "const_S_IRUSR" ownerReadMode    :: FileMode
foreign import stdcall "libHS_cbits.so" "const_S_IWUSR" ownerWriteMode   :: FileMode
foreign import stdcall "libHS_cbits.so" "const_S_IXUSR" ownerExecuteMode :: FileMode

emptyFileMode     = primIntToWord 0
unionFileMode     = primOrWord
intersectFileMode = primAndWord
#else
ownerReadMode    :: FileMode
ownerReadMode    = ``S_IRUSR''

ownerWriteMode   :: FileMode
ownerWriteMode   = ``S_IWUSR''

ownerExecuteMode :: FileMode
ownerExecuteMode = ``S_IXUSR''

intersectFileMode :: FileMode -> FileMode -> FileMode
intersectFileMode (W# m1#) (W# m2#) = W# (m1# `and#` m2#)

fileMode          :: FileStatus -> FileMode
fileMode stat = unsafePerformIO (
	_casm_ ``%r = ((struct stat *)%0)->st_mode;'' stat)
#endif

\end{code}

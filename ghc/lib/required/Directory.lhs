%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Directory]{Module @Directory@}

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
module Directory ( 
    createDirectory, removeDirectory, removeFile, 
    renameDirectory, renameFile, getDirectoryContents,
    getCurrentDirectory, setCurrentDirectory
  ) where

import Prelude
import Foreign
import IOBase
import STBase		( PrimIO )
import PackedString	( packCBytesST, unpackPS )
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
\end{code}


%*********************************************************
%*							*
\subsection{Signatures}
%*							*
%*********************************************************

$createDirectory dir$ creates a new directory
{\em dir} which is initially empty, or as near to empty as the
operating system allows.

The operation may fail with:
\begin{itemize}
\item $AlreadyExists$
The operand refers to a directory that already exists.  
[$EEXIST$]
\item $HardwareFault$
A physical I/O error has occurred.
[$EIO$]
\item $InvalidArgument$
The operand is not a valid directory name.
[$ENAMETOOLONG$, $ELOOP$]
\item $NoSuchThing$
There is no path to the directory. 
[$ENOENT$, $ENOTDIR$]
\item $PermissionDenied$
The process has insufficient privileges to perform the operation.
[$EROFS$, $EACCES$]
\item $ResourceExhausted$
Insufficient resources (virtual memory, process file descriptors,
physical disk space, etc.) are available to perform the operation.
[$EDQUOT$, $ENOSPC$, $ENOMEM$, 
$EMLINK$] 
\item $InappropriateType$
The path refers to an existing non-directory object.
[$EEXIST$]
\end{itemize}

\begin{code}
createDirectory path =
    _ccall_ createDirectory path    `thenIO_Prim` \ rc ->
    if rc == 0 then
        return ()
    else
        constructErrorAndFail "createDirectory"
\end{code}

$removeDirectory dir$ removes an existing directory {\em dir}.  The
implementation may specify additional constraints which must be
satisfied before a directory can be removed (e.g. the directory has to
be empty, or may not be in use by other processes).  It is not legal
for an implementation to partially remove a directory unless the
entire directory is removed. A conformant implementation need not
support directory removal in all situations (e.g. removal of the root
directory).

The operation may fail with:
\begin{itemize}
\item $HardwareFault$
A physical I/O error has occurred.
[$EIO$]
\item $InvalidArgument$
The operand is not a valid directory name.
[$ENAMETOOLONG$, $ELOOP$]
\item $NoSuchThing$
The directory does not exist. 
[$ENOENT$, $ENOTDIR$]
\item $PermissionDenied$
The process has insufficient privileges to perform the operation.
[$EROFS$, $EACCES$, $EPERM$]
\item $UnsatisfiedConstraints$
Implementation-dependent constraints are not satisfied.  
[$EBUSY$, $ENOTEMPTY$, $EEXIST$]
\item $UnsupportedOperation$
The implementation does not support removal in this situation.
[$EINVAL$]
\item $InappropriateType$
The operand refers to an existing non-directory object.
[$ENOTDIR$]
\end{itemize}

\begin{code}
removeDirectory path =
    _ccall_ removeDirectory path    `thenIO_Prim` \ rc ->
    if rc == 0 then
        return ()
    else
        constructErrorAndFail "removeDirectory"
\end{code}

$removeFile file$ removes the directory entry for an existing file
{\em file}, where {\em file} is not itself a directory. The
implementation may specify additional constraints which must be
satisfied before a file can be removed (e.g. the file may not be in
use by other processes).

The operation may fail with:
\begin{itemize}
\item $HardwareFault$
A physical I/O error has occurred.
[$EIO$]
\item $InvalidArgument$
The operand is not a valid file name.
[$ENAMETOOLONG$, $ELOOP$]
\item $NoSuchThing$
The file does not exist. 
[$ENOENT$, $ENOTDIR$]
\item $PermissionDenied$
The process has insufficient privileges to perform the operation.
[$EROFS$, $EACCES$, $EPERM$]
\item $UnsatisfiedConstraints$
Implementation-dependent constraints are not satisfied.  
[$EBUSY$]
\item $InappropriateType$
The operand refers to an existing directory.
[$EPERM$, $EINVAL$]
\end{itemize}

\begin{code}
removeFile path =
    _ccall_ removeFile path `thenIO_Prim` \ rc ->
    if rc == 0 then
        return ()
    else
        constructErrorAndFail "removeFile"
\end{code}

$renameDirectory old$ {\em new} changes the name of an existing
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
\item $HardwareFault$
A physical I/O error has occurred.
[$EIO$]
\item $InvalidArgument$
Either operand is not a valid directory name.
[$ENAMETOOLONG$, $ELOOP$]
\item $NoSuchThing$
The original directory does not exist, or there is no path to the target.
[$ENOENT$, $ENOTDIR$]
\item $PermissionDenied$
The process has insufficient privileges to perform the operation.
[$EROFS$, $EACCES$, $EPERM$]
\item $ResourceExhausted$
Insufficient resources are available to perform the operation.  
[$EDQUOT$, $ENOSPC$, $ENOMEM$, 
$EMLINK$]
\item $UnsatisfiedConstraints$
Implementation-dependent constraints are not satisfied.
[$EBUSY$, $ENOTEMPTY$, $EEXIST$]
\item $UnsupportedOperation$
The implementation does not support renaming in this situation.
[$EINVAL$, $EXDEV$]
\item $InappropriateType$
Either path refers to an existing non-directory object.
[$ENOTDIR$, $EISDIR$]
\end{itemize}

\begin{code}
renameDirectory opath npath =
    _ccall_ renameDirectory opath npath	`thenIO_Prim` \ rc ->
    if rc == 0 then
        return ()
    else
        constructErrorAndFail "renameDirectory"
\end{code}

$renameFile old$ {\em new} changes the name of an existing file system
object from {\em old} to {\em new}.  If the {\em new} object already
exists, it is atomically replaced by the {\em old} object.  Neither
path may refer to an existing directory.  A conformant implementation
need not support renaming files in all situations (e.g. renaming
across different physical devices), but the constraints must be
documented.

The operation may fail with:
\begin{itemize}
\item $HardwareFault$
A physical I/O error has occurred.
[$EIO$]
\item $InvalidArgument$
Either operand is not a valid file name.
[$ENAMETOOLONG$, $ELOOP$]
\item $NoSuchThing$
The original file does not exist, or there is no path to the target.
[$ENOENT$, $ENOTDIR$]
\item $PermissionDenied$
The process has insufficient privileges to perform the operation.
[$EROFS$, $EACCES$, $EPERM$]
\item $ResourceExhausted$
Insufficient resources are available to perform the operation.  
[$EDQUOT$, $ENOSPC$, $ENOMEM$, 
$EMLINK$]
\item $UnsatisfiedConstraints$
Implementation-dependent constraints are not satisfied.
[$EBUSY$]
\item $UnsupportedOperation$
The implementation does not support renaming in this situation.
[$EXDEV$]
\item $InappropriateType$
Either path refers to an existing directory.
[$ENOTDIR$, $EISDIR$, $EINVAL$, 
$EEXIST$, $ENOTEMPTY$]
\end{itemize}

\begin{code}
renameFile opath npath =
    _ccall_ renameFile opath npath  `thenIO_Prim` \ rc ->
    if rc == 0 then
        return ()
    else
        constructErrorAndFail	"renameFile"
\end{code}

$getDirectoryContents dir$ returns a list of
<i>all</i> entries in {\em dir}.

The operation may fail with:
\begin{itemize}
\item $HardwareFault$
A physical I/O error has occurred.
[$EIO$]
\item $InvalidArgument$
The operand is not a valid directory name.
[$ENAMETOOLONG$, $ELOOP$]
\item $NoSuchThing$
The directory does not exist.
[$ENOENT$, $ENOTDIR$]
\item $PermissionDenied$
The process has insufficient privileges to perform the operation.
[$EACCES$]
\item $ResourceExhausted$
Insufficient resources are available to perform the operation.
[$EMFILE$, $ENFILE$]
\item $InappropriateType$
The path refers to an existing non-directory object.
[$ENOTDIR$]
\end{itemize}

\begin{code}
getDirectoryContents path =
    _ccall_ getDirectoryContents path	`thenIO_Prim` \ ptr ->
    if ptr == ``NULL'' then
        constructErrorAndFail "getDirectoryContents"
    else
	stToIO (getEntries ptr 0)	>>= \ entries ->
	_ccall_ free ptr		`thenIO_Prim` \ () ->
	return entries
  where
    getEntries :: Addr -> Int -> PrimIO [FilePath]
    getEntries ptr n =
        _casm_ ``%r = ((char **)%0)[%1];'' ptr n    >>= \ str ->
        if str == ``NULL'' then 
            return []
        else
            _ccall_ strlen str			    >>= \ len ->
	    packCBytesST len str		    >>= \ entry ->
            _ccall_ free str			    >>= \ () ->
            getEntries ptr (n+1)		    >>= \ entries ->
	    return (unpackPS entry : entries)
\end{code}

If the operating system has a notion of current directories,
$getCurrentDirectory$ returns an absolute path to the
current directory of the calling process.

The operation may fail with:
\begin{itemize}
\item $HardwareFault$
A physical I/O error has occurred.
[$EIO$]
\item $NoSuchThing$
There is no path referring to the current directory.
[$EPERM$, $ENOENT$, $ESTALE$...]
\item $PermissionDenied$
The process has insufficient privileges to perform the operation.
[$EACCES$]
\item $ResourceExhausted$
Insufficient resources are available to perform the operation.
\item $UnsupportedOperation$
The operating system has no notion of current directory.
\end{itemize}

\begin{code}
getCurrentDirectory =
    _ccall_ getCurrentDirectory	    `thenIO_Prim` \ str ->
    if str /= ``NULL'' then
        _ccall_ strlen str		`thenIO_Prim` \ len ->
        stToIO (packCBytesST len str)	>>=	    \ pwd ->
        _ccall_ free str		`thenIO_Prim` \ () ->
        return (unpackPS pwd)
    else
        constructErrorAndFail "getCurrentDirectory"
\end{code}

If the operating system has a notion of current directories,
$setCurrentDirectory dir$ changes the current
directory of the calling process to {\em dir}.

The operation may fail with:
\begin{itemize}
\item $HardwareFault$
A physical I/O error has occurred.
[$EIO$]
\item $InvalidArgument$
The operand is not a valid directory name.
[$ENAMETOOLONG$, $ELOOP$]
\item $NoSuchThing$
The directory does not exist.
[$ENOENT$, $ENOTDIR$]
\item $PermissionDenied$
The process has insufficient privileges to perform the operation.
[$EACCES$]
\item $UnsupportedOperation$
The operating system has no notion of current directory, or the
current directory cannot be dynamically changed.
\item $InappropriateType$
The path refers to an existing non-directory object.
[$ENOTDIR$]
\end{itemize}

\begin{code}
setCurrentDirectory path =
    _ccall_ setCurrentDirectory path	`thenIO_Prim` \ rc ->
    if rc == 0 then
        return ()
    else
        constructErrorAndFail "setCurrentDirectory"
\end{code}



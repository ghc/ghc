`%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
% Last Modified: Wed Jul 19 13:12:10 1995
% Darren J Moffat <moffatd@dcs.gla.ac.uk>
%
% Generated from: @(#)errno.h 2.14 90/01/23 SMI; from UCB 4.1 82/12/28
\section[CError]{Interface to C Error Codes}

\begin{code}
module CError (
    CErrorCode(..),

    errorCodeToStr,	-- :: CErrorCode -> String
    getCErrorCode,	-- :: PrimIO CErrorCode
    setCErrorCode	-- :: CErrorCode -> PrimIO ()

) where

import PreludeGlaST
\end{code}

import PreludeGlaMisc
import LibSystem
\begin{code}     
data CErrorCode =
	  NOERROR	-- Added as dummy value since deriving Ix starts at 0
	| EPERM		-- Not owner
	| ENOENT	-- No such file or directory
	| ESRCH		-- No such process
	| EINTR		-- Interrupted system call
	| EIO		-- I/O error
	| ENXIO		-- No such device or address
	| E2BIG		-- Arg list too long
	| ENOEXEC	-- Exec format error
	| EBADF		-- Bad file number
	| ECHILD	-- No children
	| EAGAIN	-- No more processes
	| ENOMEM	-- Not enough core
	| EACCES	-- Permission denied
	| EFAULT	-- Bad address
	| ENOTBLK	-- Block device required
	| EBUSY		-- Mount device busy
	| EEXIST	-- File exists
	| EXDEV		-- Cross-device link
	| ENODEV	-- No such device
	| ENOTDIR	-- Not a directory*/
	| EISDIR	-- Is a directory
	| EINVAL	-- Invalid argument
	| ENFILE	-- File table overflow
	| EMFILE	-- Too many open files
	| ENOTTY	-- Not a typewriter
	| ETXTBSY	-- Text file busy
	| EFBIG		-- File too large
	| ENOSPC	-- No space left on device
	| ESPIPE	-- Illegal seek
	| EROFS		-- Read-only file system
	| EMLINK	-- Too many links
	| EPIPE		-- Broken pipe

-- math software
	| EDOM		-- Argument too large
	| ERANGE	-- Result too large

-- non-blocking and interrupt i/o
	| EWOULDBLOCK	-- Operation would block
	| EINPROGRESS	-- Operation now in progress
	| EALREADY	-- Operation already in progress
-- ipc/network software

-- argument errors
	| ENOTSOCK	-- Socket operation on non-socket
	| EDESTADDRREQ	-- Destination address required
	| EMSGSIZE	-- Message too long
	| EPROTOTYPE	-- Protocol wrong type for socket
	| ENOPROTOOPT	-- Protocol not available
	| EPROTONOSUPPOR -- Protocol not supported
	| ESOCKTNOSUPPORT -- Socket type not supported
	| EOPNOTSUPP	-- Operation not supported on socket
	| EPFNOSUPPORT	-- Protocol family not supported
	| EAFNOSUPPORT	-- Address family not supported by protocol family
	| EADDRINUSE	-- Address already in use
	| EADDRNOTAVAIL	-- Can't assign requested address
-- operational errors
	| ENETDOWN	-- Network is down
	| ENETUNREACH	-- Network is unreachable
	| ENETRESET	-- Network dropped connection on reset
	| ECONNABORTED	-- Software caused connection abort
	| ECONNRESET	-- Connection reset by peer
	| ENOBUFS	-- No buffer space available
	| EISCONN	-- Socket is already connected
	| ENOTCONN	-- Socket is not connected
	| ESHUTDOWN	-- Can't send after socket shutdown
	| ETOOMANYREFS	-- Too many references: can't splice
	| ETIMEDOUT	-- Connection timed out
	| ECONNREFUSED	-- Connection refused

	| ELOOP		-- Too many levels of symbolic links
	| ENAMETOOLONG	-- File name too long

-- should be rearranged
	| EHOSTDOWN	-- Host is down
	| EHOSTUNREACH	-- No route to host
	| ENOTEMPTY	-- Directory not empty

-- quotas & mush
	| EPROCLIM	-- Too many processes
	| EUSERS	-- Too many users
	| EDQUOT	-- Disc quota exceeded

-- Network File System
	| ESTALE	-- Stale NFS file handle
	| EREMOTE	-- Too many levels of remote in path

-- streams
	| ENOSTR	-- Device is not a stream
	| ETIME		-- Timer expired
	| ENOSR		-- Out of streams resources
	| ENOMSG	-- No message of desired type
	| EBADMSG	-- Trying to read unreadable message

-- SystemV IPC
	| EIDRM		-- Identifier removed

-- SystemV Record Locking
	| EDEADLK	-- Deadlock condition.
	| ENOLCK	-- No record locks available.

-- RFS
	| ENONET	-- Machine is not on the network
	| ERREMOTE	-- Object is remote
	| ENOLINK	-- the link has been severed
	| EADV		-- advertise error
	| ESRMNT	-- srmount error
	| ECOMM		-- Communication error on send
	| EPROTO	-- Protocol error
	| EMULTIHOP	-- multihop attempted
	| EDOTDOT	-- Cross mount point (not an error)
	| EREMCHG	-- Remote address changed
-- POSIX
	| ENOSYS	-- function not implemented

	deriving (Eq,Ord,Ix,Text)


errorCodeToStr :: CErrorCode -> String
errorCodeToStr NOERROR	= ""
errorCodeToStr EPERM	= "Not owner"
errorCodeToStr ENOENT	= "No such file or directory"
errorCodeToStr ESRCH	= "No such process"
errorCodeToStr EINTR	= "Interrupted system call"
errorCodeToStr EIO	= "I/O error"
errorCodeToStr ENXIO	= "No such device or address"
errorCodeToStr E2BIG	= "Arg list too long"
errorCodeToStr ENOEXEC	= "Exec format error"
errorCodeToStr EBADF	= "Bad file number"
errorCodeToStr ECHILD	= "No children"
errorCodeToStr EAGAIN	= "No more processes"
errorCodeToStr ENOMEM	= "Not enough core"
errorCodeToStr EACCES	= "Permission denied"
errorCodeToStr EFAULT	= "Bad address"
errorCodeToStr ENOTBLK	= "Block device required"
errorCodeToStr EBUSY	= "Mount device busy"
errorCodeToStr EEXIST	= "File exists"
errorCodeToStr EXDEV	= "Cross-device link"
errorCodeToStr ENODEV	= "No such device"
errorCodeToStr ENOTDIR	= "Not a directory"
errorCodeToStr EISDIR	= "Is a directory"
errorCodeToStr EINVAL	= "Invalid argument"
errorCodeToStr ENFILE	= "File table overflow"
errorCodeToStr EMFILE	= "Too many open files"
errorCodeToStr ENOTTY	= "Not a typewriter"
errorCodeToStr ETXTBSY	= "Text file busy"
errorCodeToStr EFBIG	= "File too large"
errorCodeToStr ENOSPC	= "No space left on device"
errorCodeToStr ESPIPE	= "Illegal seek"
errorCodeToStr EROFS	= "Read-only file system"
errorCodeToStr EMLINK	= "Too many links"
errorCodeToStr EPIPE	= "Broken pipe"

-- math software
errorCodeToStr EDOM	= "Argument too large"
errorCodeToStr ERANGE	= "Result too large"

-- non-blocking and interrupt i/o"
errorCodeToStr EWOULDBLOCK	= "Operation would block"
errorCodeToStr EINPROGRESS	= "Operation now in progress"
errorCodeToStr EALREADY		= "Operation already in progress"
-- ipc/network software

-- argument errors
errorCodeToStr ENOTSOCK		= "Socket operation on non-socket"
errorCodeToStr EDESTADDRREQ	= "Destination address required"
errorCodeToStr EMSGSIZE		= "Message too long"
errorCodeToStr EPROTOTYPE	= "Protocol wrong type for socket"
errorCodeToStr ENOPROTOOPT	= "Protocol not available"
errorCodeToStr EPROTONOSUPPOR 	= "Protocol not supported"
errorCodeToStr ESOCKTNOSUPPORT 	= "Socket type not supported"
errorCodeToStr EOPNOTSUPP	= "Operation not supported on socket"
errorCodeToStr EPFNOSUPPORT	= "Protocol family not supported"
errorCodeToStr EAFNOSUPPORT	= "Address family not supported by protocol family"
errorCodeToStr EADDRINUSE	= "Address already in use"
errorCodeToStr EADDRNOTAVAIL	= "Can't assign requested address"

-- operational errors
errorCodeToStr ENETDOWN		= "Network is down"
errorCodeToStr ENETUNREACH	= "Network is unreachable"
errorCodeToStr ENETRESET	= "Network dropped connection on reset"
errorCodeToStr ECONNABORTED	= "Software caused connection abort"
errorCodeToStr ECONNRESET	= "Connection reset by peer"
errorCodeToStr ENOBUFS		= "No buffer space available"
errorCodeToStr EISCONN		= "Socket is already connected"
errorCodeToStr ENOTCONN		= "Socket is not connected"
errorCodeToStr ESHUTDOWN	= "Can't send after socket shutdown"
errorCodeToStr ETOOMANYREFS	= "Too many references: can't splice"
errorCodeToStr ETIMEDOUT	= "Connection timed out"
errorCodeToStr ECONNREFUSED	= "Connection refused"

errorCodeToStr ELOOP		= "Too many levels of symbolic links"
errorCodeToStr ENAMETOOLONG	= "File name too long"

-- should be rearranged
errorCodeToStr EHOSTDOWN	= "Host is down"
errorCodeToStr EHOSTUNREACH	= "No route to host"
errorCodeToStr ENOTEMPTY	= "Directory not empty"

-- quotas & mush
errorCodeToStr EPROCLIM	= "Too many processes"
errorCodeToStr EUSERS	= "Too many users"
errorCodeToStr EDQUOT	= "Disc quota exceeded"

-- Network File System
errorCodeToStr ESTALE	= "Stale NFS file handle"
errorCodeToStr EREMOTE	= "Too many levels of remote in path"

-- streams
errorCodeToStr ENOSTR	= "Device is not a stream"
errorCodeToStr ETIME	= "Timer expired"
errorCodeToStr ENOSR	= "Out of streams resources"
errorCodeToStr ENOMSG	= "No message of desired type"
errorCodeToStr EBADMSG	= "Trying to read unreadable message"

-- SystemV IPC
errorCodeToStr EIDRM	= "Identifier removed"

-- SystemV Record Locking
errorCodeToStr EDEADLK	= "Deadlock condition."
errorCodeToStr ENOLCK	= "No record locks available."

-- RFS
errorCodeToStr ENONET	= "Machine is not on the network"
errorCodeToStr ERREMOTE	= "Object is remote"
errorCodeToStr ENOLINK	= "the link has been severed"
errorCodeToStr EADV	= "advertise error"
errorCodeToStr ESRMNT	= "srmount error"
errorCodeToStr ECOMM	= "Communication error on send"
errorCodeToStr EPROTO	= "Protocol error"
errorCodeToStr EMULTIHOP = "multihop attempted"
errorCodeToStr EDOTDOT	= "Cross mount point (not an error)"
errorCodeToStr EREMCHG	= "Remote address changed"

-- POSIX
errorCodeToStr ENOSYS	= "function not implemented"

unpackCErrorCode   :: Int -> CErrorCode
unpackCErrorCode   e = (range (NOERROR, ENOSYS))!!e

packCErrorCode :: CErrorCode -> Int
packCErrorCode e = index (NOERROR, ENOSYS) e


getCErrorCode :: PrimIO CErrorCode
getCErrorCode =
    _casm_ ``%r = errno;''			    `thenPrimIO` \ errno ->    
    returnPrimIO (unpackCErrorCode errno)


setCErrorCode :: CErrorCode -> PrimIO ()
setCErrorCode ecode =
    _casm_ ``errno = %0;'' (packCErrorCode ecode)   `thenPrimIO` \ () ->
    returnPrimIO ()


\end{code}


%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[PosixIO]{Haskell 1.3 POSIX Input/Output Primitives}

\begin{code}
{-# OPTIONS -#include "../std/cbits/stgio.h" #-}
module PosixIO (
    FdOption(..),
    FileLock,
    LockRequest(..),

    fdClose,
    createPipe,
    dup,
    dupTo,

    fdRead,
    fdWrite,
    fdSeek,

    queryFdOption,
    setFdOption,

    getLock,  setLock,
    waitToSetLock,

    -- Handle <-> Fd
    handleToFd, fdToHandle,
    ) where

import GlaExts
import PrelIOBase
import PrelHandle (newHandle, getBMode__, getHandleFd, 
		   freeFileObject, freeStdFileObject )
import IO
import Addr
import Foreign
import Weak 	( addForeignFinalizer )
import CString ( freeze, allocChars, packStringIO, unpackNBytesBAIO )

import PosixUtil
import PosixFiles ( stdInput, stdOutput, stdError )


createPipe :: IO (Fd, Fd)
createPipe = do
    bytes <- allocChars ``(2*sizeof(int))''
    rc    <- _casm_ ``%r = pipe((int *)%0);'' bytes
    if rc /= ((-1)::Int)
       then do
	rd <- _casm_ ``%r = ((int *)%0)[0];'' bytes
	wd <- _casm_ ``%r = ((int *)%0)[1];'' bytes
	return (rd, wd)
       else
	syserr "createPipe"

dup :: Fd -> IO Fd
dup fd =
    _ccall_ dup fd	>>= \ fd2@(I# fd2#) ->
    if fd2 /= -1 then
	return (FD# fd2#)
    else
	syserr "dup"

dupTo :: Fd -> Fd -> IO ()
dupTo fd1 fd2 = minusone_error (_ccall_ dup2 fd1 fd2) "dupTo"

fdClose :: Fd -> IO ()
fdClose fd = minusone_error (_ccall_ close fd) "fdClose"

handleToFd :: Handle -> IO Fd
handleToFd h = do
  fd <- getHandleFd h
  let (I# fd#) = fd
  return (FD# fd#)

-- default is no buffering.
fdToHandle :: Fd -> IO Handle
fdToHandle fd@(FD# fd#) = do
     -- first find out what kind of file desc. this is..
    flags <- _ccall_ fcntl fd (``F_GETFL''::Int) (0::Int)
    if flags /= ((-1)::Int)
     then do
      let
       (I# flags#) = flags

       wH  = (int2Word# flags# `and#` (case ``O_WRONLY'' of { W# x -> x}))
			`neWord#` int2Word# 0#
       aH  = (int2Word# flags# `and#` (case ``O_APPEND'' of { W# x -> x}))
			`neWord#` int2Word# 0#
       rwH = (int2Word# flags# `and#` (case ``O_RDWR'' of { W# x -> x }))
			`neWord#` int2Word# 0#

       (handle_t, flush_on_close)
	 | wH && aH  = (AppendHandle, 1)
	 | wH        = (WriteHandle, 1)
	 | rwH       = (ReadWriteHandle, 1)
	 | otherwise = (ReadHandle, 0)
	  
      fo <- _ccall_ openFd fd flags (flush_on_close::Int)
      if fo /= nullAddr then do
	 {-
	   A distinction is made here between std{Input,Output,Error} Fds
	   and all others. The standard descriptors have a finaliser
	   that will not close the underlying fd, the others have one
	   that will. 

	   Delaying the closing of the standard descriptors until the process
	   exits is necessary since the RTS is likely to require these after
	   (or as a result of) program termination.
	 -}
#ifndef __PARALLEL_HASKELL__
	 fo <- mkForeignObj fo
	 if fd == stdInput || fd == stdOutput || fd == stdError then
	      addForeignFinalizer fo (freeStdFileObject fo)
	  else
	      addForeignFinalizer fo (freeFileObject fo)
#endif
	 (bm, bf_size)  <- getBMode__ fo
         mkBuffer__ fo bf_size
	 newHandle (Handle__ fo handle_t bm fd_str)
       else
         syserr "fdToHandle"
     else
       syserr "fdToHandle"
  where
   fd_str = "<file descriptor: " ++ show (I# fd#) ++ ">"

fdRead :: Fd -> ByteCount -> IO (String, ByteCount)
fdRead fd 0 = return ("", 0)
fdRead fd nbytes = do
    bytes <-  allocChars nbytes
    rc    <-  _ccall_ read fd bytes nbytes
    case rc of
      -1 -> syserr "fdRead"
      0  -> ioError (IOError Nothing EOF "fdRead" "EOF")
      n | n == nbytes -> do
	    buf <- freeze bytes
	    s   <- unpackNBytesBAIO buf n
	    return (s, n)
        | otherwise -> do
	    -- Let go of the excessively long ByteArray# by copying to a
	    -- shorter one.  Maybe we need a new primitive, shrinkCharArray#?
	    bytes' <- allocChars n
	    _casm_ ``do {I_ i; for(i = 0; i < %2; i++) ((B_)%0)[i] = ((B_)%1)[i];
                      } while(0);'' bytes' bytes n
            buf <- freeze bytes'
	    s   <- unpackNBytesBAIO buf n
	    return (s, n)

fdWrite :: Fd -> String -> IO ByteCount
fdWrite fd str = do
    buf <- packStringIO str
    rc  <- _ccall_ write fd buf (length str)
    if rc /= ((-1)::Int)
       then return rc
       else syserr "fdWrite"

data FdOption = AppendOnWrite
	      | CloseOnExec
	      | NonBlockingRead

queryFdOption :: Fd -> FdOption -> IO Bool
queryFdOption fd CloseOnExec =
    _ccall_ fcntl fd (``F_GETFD''::Int) (0::Int)    >>= \ (I# flags#) ->
    if flags# /=# -1# then
	return ((int2Word# flags# `and#` fd_cloexec#) `neWord#` int2Word# 0#)
    else
	syserr "queryFdOption"
  where
    fd_cloexec# = case (``FD_CLOEXEC'') of { W# x -> x }
queryFdOption fd other =
    _ccall_ fcntl fd (``F_GETFL''::Int) (0::Int)    >>= \ (I# flags#) ->
    if flags# >=# 0# then
	return ((int2Word# flags# `and#` opt#) `neWord#` int2Word# 0#)
    else
	syserr "queryFdOption"
  where
    opt# = case (
	case other of
	  AppendOnWrite   -> ``O_APPEND''
          NonBlockingRead -> ``O_NONBLOCK'' ) of { W# x -> x }

setFdOption :: Fd -> FdOption -> Bool -> IO ()
setFdOption fd CloseOnExec val = do
    flags <- _ccall_ fcntl fd (``F_GETFD''::Int) (0::Int)
    if flags /= ((-1)::Int) then do
	rc <- (if val then
		 _casm_ ``%r = fcntl(%0, F_SETFD, %1 | FD_CLOEXEC);'' fd flags
	       else do
		 _casm_ ``%r = fcntl(%0, F_SETFD, %1 & ~FD_CLOEXEC);'' fd flags)
	if rc /= ((-1)::Int)
	   then return ()
	   else fail
     else fail
  where
    fail = syserr "setFdOption"

setFdOption fd other val = do
    flags <- _ccall_ fcntl fd (``F_GETFL''::Int) (0::Int)
    if flags >= (0::Int) then do
	rc <- (if val then
	         _casm_ ``%r = fcntl(%0, F_SETFL, %1 | %2);'' fd flags opt
	       else do
	         _casm_ ``%r = fcntl(%0, F_SETFL, %1 & ~(%2));'' fd flags opt)
	if rc /= ((-1)::Int)
	   then return ()
	   else fail
     else fail
  where
    fail = syserr "setFdOption"
    opt =
	case other of
	  AppendOnWrite -> (``O_APPEND''::Word)
          NonBlockingRead -> (``O_NONBLOCK''::Word)

data LockRequest = ReadLock
                 | WriteLock
                 | Unlock

type FileLock = (LockRequest, SeekMode, FileOffset, FileOffset)

getLock :: Fd -> FileLock -> IO (Maybe (ProcessID, FileLock))
getLock fd lock = do
    flock <- lock2Bytes lock
    rc    <- _ccall_ fcntl fd (``F_GETLK''::Int) flock
    if rc /= ((-1)::Int)
       then do
	    result <- bytes2ProcessIDAndLock flock
	    return (maybeResult result)
       else syserr "getLock"
  where
    maybeResult (_, (Unlock, _, _, _)) = Nothing
    maybeResult x = Just x

setLock :: Fd -> FileLock -> IO ()
setLock fd lock = do
    flock <- lock2Bytes lock
    minusone_error (_ccall_ fcntl fd (``F_SETLK''::Int) flock) "setLock"

waitToSetLock :: Fd -> FileLock -> IO ()
waitToSetLock fd lock = do
    flock <- lock2Bytes lock
    minusone_error (_ccall_ fcntl fd (``F_SETLKW''::Int) flock) "waitToSetLock"

fdSeek :: Fd -> SeekMode -> FileOffset -> IO FileOffset
fdSeek fd mode offset = do
    rc <- _ccall_ lseek fd offset (mode2Int mode)
    if rc /= ((-1)::Int)
       then return rc
       else syserr "fdSeek"

\end{code}

Local utility functions

\begin{code}

-- Convert a Haskell SeekMode to an int

mode2Int :: SeekMode -> Int
mode2Int AbsoluteSeek = ``SEEK_SET''
mode2Int RelativeSeek = ``SEEK_CUR''
mode2Int SeekFromEnd  = ``SEEK_END''

-- Convert a Haskell FileLock to an flock structure
lockRequest2Int :: LockRequest -> Int
lockRequest2Int kind =
 case kind of
  ReadLock  -> ``F_RDLCK''
  WriteLock -> ``F_WRLCK''
  Unlock    -> ``F_UNLCK''

lock2Bytes :: FileLock -> IO (MutableByteArray RealWorld Int)
lock2Bytes (kind, mode, start, len) = do
    bytes <- allocChars ``sizeof(struct flock)''
    _casm_ ``do { struct flock *fl = (struct flock *)%0;
		  fl->l_type = %1;
		  fl->l_whence = %2;
		  fl->l_start = %3;
		  fl->l_len = %4;
             } while(0);''
	     bytes (lockRequest2Int kind) (mode2Int mode) start len
    return bytes
--  where

bytes2ProcessIDAndLock :: MutableByteArray s Int -> IO (ProcessID, FileLock)
bytes2ProcessIDAndLock bytes = do
    ltype   <- _casm_ ``%r = ((struct flock *)%0)->l_type;'' bytes
    lwhence <- _casm_ ``%r = ((struct flock *)%0)->l_whence;'' bytes
    lstart  <- _casm_ ``%r = ((struct flock *)%0)->l_start;'' bytes
    llen    <- _casm_ ``%r = ((struct flock *)%0)->l_len;'' bytes
    lpid    <- _casm_ ``%r = ((struct flock *)%0)->l_pid;'' bytes
    return (lpid, (kind ltype, mode lwhence, lstart, llen))

kind :: Int -> LockRequest
kind x
 | x == ``F_RDLCK'' = ReadLock
 | x == ``F_WRLCK'' = WriteLock
 | x == ``F_UNLCK'' = Unlock

mode :: Int -> SeekMode
mode x
 | x == ``SEEK_SET'' = AbsoluteSeek
 | x == ``SEEK_CUR'' = RelativeSeek
 | x == ``SEEK_END'' = SeekFromEnd

\end{code}

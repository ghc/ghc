%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibPosixIO]{Haskell 1.3 POSIX Input/Output Primitives}

\begin{code}
module LibPosixIO (
    ChannelOption(..),
    FileLock(..),
    LockRequest(..),
    
    closeChannel,
    createPipe,
    dupChannel,
    dupChannelTo,
    getLock,
    queryChannelOption,
    readChannel,
    seekChannel,
    setChannelOption,
    setLock,
    waitToSetLock,
    writeChannel
    ) where

import PreludeGlaST
import PS

import LibPosixUtil

createPipe :: IO (Channel, Channel)
createPipe = 
    allocChars ``(2*sizeof(int))''		    `thenStrictlyST` \ bytes ->
    _casm_ ``%r = pipe((int *)%0);'' bytes	    `thenPrimIO` \ rc ->
    if rc /= -1 then
	_casm_ ``%r = ((int *)%0)[0];'' bytes	    `thenPrimIO` \ wd ->
	_casm_ ``%r = ((int *)%0)[1];'' bytes	    `thenPrimIO` \ rd ->
	return (wd, rd)
    else    
	syserr "createPipe"

dupChannel :: Channel -> IO Channel
dupChannel fd = 
    _ccall_ dup fd				    `thenPrimIO` \ fd2 ->
    if fd2 /= -1 then
	return fd2
    else
	syserr "dupChannel"

dupChannelTo :: Channel -> Channel -> IO ()
dupChannelTo fd1 fd2 = 
    _ccall_ dup2 fd1 fd2			    `thenPrimIO` \ rc ->
    if rc /= -1 then
	return ()
    else
	syserr "dupChannelTo"

closeChannel :: Channel -> IO ()
closeChannel fd = 
    _ccall_ close fd				    `thenPrimIO` \ rc ->
    if rc /= -1 then
	return ()
    else
	syserr "closeChannel"

readChannel :: Channel -> ByteCount -> IO (String, ByteCount)
readChannel fd 0 = return ("", 0)
readChannel fd nbytes =
    allocChars nbytes				    `thenStrictlyST` \ bytes ->
    _ccall_ read fd bytes nbytes		    `thenPrimIO` \ rc ->
    case rc of
      -1 -> syserr "readChannel"
      0  -> failWith EOF
      n | n == nbytes -> 
	    freeze bytes			    `thenStrictlyST` \ buf ->
	    return (_unpackPS (_unsafeByteArrayToPS buf n), n)
        | otherwise ->
	    -- Let go of the excessively long ByteArray# by copying to a shorter one.
	    -- Maybe we need a new primitive, shrinkCharArray#?
	    allocChars n			    `thenPrimIO` \ bytes' ->
	    _casm_ ``do {I_ i; for(i = 0; i < %2; i++) ((B_)%0)[i] = ((B_)%1)[i]; 
                     } while(0);'' bytes' bytes n   `thenPrimIO` \ () ->
	    freeze bytes'			    `thenStrictlyST` \ buf ->
	    return (_unpackPS (_unsafeByteArrayToPS buf n), n)

writeChannel :: Channel -> String -> IO ByteCount
writeChannel fd str =
    _packBytesForCST str			    `thenPrimIO` \ buf ->
    _ccall_ write fd buf (length str)		    `thenPrimIO` \ rc ->
    if rc /= -1 then
	return rc
    else
	syserr "writeChannel"

data ChannelOption = AppendOnWrite    
		   | CloseOnExec
		   | NonBlockingRead

queryChannelOption :: ChannelOption -> Channel -> IO Bool
queryChannelOption CloseOnExec fd =
    _ccall_ fcntl fd (``F_GETFD''::Int) 0	    `thenPrimIO` \ (I# flags#) ->
    if flags# /=# -1# then
	return ((int2Word# flags# `and#` fd_cloexec#) `neWord#` int2Word# 0#)
    else
	syserr "queryChannelOption"
  where
    fd_cloexec# = case (``FD_CLOEXEC'') of { W# x -> x }
queryChannelOption other fd =
    _ccall_ fcntl fd (``F_GETFL''::Int) 0	    `thenPrimIO` \ (I# flags#) ->
    if flags# >=# 0# then
	return ((int2Word# flags# `and#` opt#) `neWord#` int2Word# 0#)
    else
	syserr "queryChannelOption"
  where
    opt# = case (
	case other of
	  AppendOnWrite -> ``O_APPEND''
          NonBlockingRead -> ``O_NONBLOCK'' ) of { W# x -> x }

setChannelOption :: ChannelOption -> Bool -> Channel -> IO ()
setChannelOption CloseOnExec val fd =
    _ccall_ fcntl fd (``F_GETFD''::Int) 0	    `thenPrimIO` \ flags ->
    if flags /= -1 then
	(if val then
	    _casm_ ``%r = fcntl(%0, F_SETFD, %1 | FD_CLOEXEC);'' fd flags
	else
	    _casm_ ``%r = fcntl(%0, F_SETFD, %1 & ~FD_CLOEXEC);'' fd flags)
						    `thenPrimIO` \ rc ->
	if rc /= -1 then
	    return ()
	else
	    fail
    else
	fail
  where
    fail = syserr "setChannelOption"	
setChannelOption other val fd =    
    _ccall_ fcntl fd (``F_GETFL''::Int) 0	    `thenPrimIO` \ flags ->
    if flags >= 0 then
	(if val then
	    _casm_ ``%r = fcntl(%0, F_SETFL, %1 | %2);'' fd flags opt
	else
	    _casm_ ``%r = fcntl(%0, F_SETFL, %1 & ~(%2));'' fd flags opt)
						    `thenPrimIO` \ rc ->
	if rc /= -1 then
	    return ()
	else
	    fail
    else
	fail
  where
    fail = syserr "setChannelOption"	
    opt = 
	case other of
	  AppendOnWrite -> (``O_APPEND''::_Word)
          NonBlockingRead -> (``O_NONBLOCK''::_Word)
	    
data LockRequest = ReadLock 
                 | WriteLock 
                 | Unlock

type FileLock = (LockRequest, SeekMode, FileOffset, FileOffset)

getLock :: Channel -> FileLock -> IO (Maybe (ProcessID, FileLock))
getLock fd lock =
    lock2Bytes lock				    >>= \ flock ->
    _ccall_ fcntl fd (``F_GETLK''::Int) flock	    `thenPrimIO` \ rc ->
    if rc /= -1 then
	bytes2ProcessIDAndLock flock		    `thenPrimIO` \ result ->
	    return (maybeResult result)
    else
	syserr "getLock"
  where
    maybeResult (_, (Unlock, _, _, _)) = Nothing
    maybeResult x = Just x

setLock :: Channel -> FileLock -> IO ()
setLock fd lock =
    lock2Bytes lock				    >>= \ flock ->
    _ccall_ fcntl fd (``F_SETLK''::Int) flock	    `thenPrimIO` \ rc ->
    if rc /= -1 then
	return ()
    else
	syserr "setLock"

waitToSetLock :: Channel -> FileLock -> IO ()
waitToSetLock fd lock =
    lock2Bytes lock				    >>= \ flock ->
    _ccall_ fcntl fd (``F_SETLKW''::Int) flock	    `thenPrimIO` \ rc ->
    if rc /= -1 then
	return ()
    else
	syserr "waitToSetLock"

seekChannel :: Channel -> SeekMode -> FileOffset -> IO FileOffset
seekChannel fd mode offset = 
    _ccall_ lseek fd offset (mode2Int mode)	    `thenPrimIO` \ rc ->
    if rc /= -1 then
	return rc
    else
	syserr "seekChannel"

\end{code}

Local utility functions

\begin{code}

-- Convert a Haskell SeekMode to an int

mode2Int :: SeekMode -> Int
mode2Int AbsoluteSeek = ``SEEK_SET''
mode2Int RelativeSeek = ``SEEK_CUR''
mode2Int SeekFromEnd  = ``SEEK_END''

-- Convert a Haskell FileLock to an flock structure

lock2Bytes :: FileLock -> IO (_MutableByteArray _RealWorld ())
lock2Bytes (kind, mode, start, len) =
    allocChars ``sizeof(struct flock)''		    `thenStrictlyST` \ bytes ->
    _casm_ ``do { struct flock *fl = (struct flock *)%0;
             fl->l_type = %1; fl->l_whence = %2; fl->l_start = %3; fl->l_len = %4;
             } while(0);'' bytes ltype (mode2Int mode) start len
						    `thenPrimIO` \ () ->
    return bytes
  where
    ltype :: Int
    ltype = case kind of 
	ReadLock -> ``F_RDLCK''
        WriteLock -> ``F_WRLCK''
        Unlock -> ``F_UNLCK''

bytes2ProcessIDAndLock :: _MutableByteArray s () -> PrimIO (ProcessID, FileLock)
bytes2ProcessIDAndLock bytes =
    _casm_ ``%r = ((struct flock *)%0)->l_type;'' bytes
						    `thenPrimIO` \ ltype ->
    _casm_ ``%r = ((struct flock *)%0)->l_whence;'' bytes
						    `thenPrimIO` \ lwhence ->
    _casm_ ``%r = ((struct flock *)%0)->l_start;'' bytes
						    `thenPrimIO` \ lstart ->
    _casm_ ``%r = ((struct flock *)%0)->l_len;'' bytes
						    `thenPrimIO` \ llen ->
    _casm_ ``%r = ((struct flock *)%0)->l_pid;'' bytes
						    `thenPrimIO` \ lpid ->
    returnPrimIO (lpid, (kind ltype, mode lwhence, lstart, llen))
  where
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

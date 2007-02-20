{-# OPTIONS_GHC -fno-implicit-prelude -#include "HsBase.h" #-}

#undef DEBUG_DUMP
#undef DEBUG

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Handle
-- Copyright   :  (c) The University of Glasgow, 1994-2001
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- This module defines the basic operations on I\/O \"handles\".
--
-----------------------------------------------------------------------------

-- #hide
module GHC.Handle (
  withHandle, withHandle', withHandle_,
  wantWritableHandle, wantReadableHandle, wantSeekableHandle,
  
  newEmptyBuffer, allocateBuffer, readCharFromBuffer, writeCharIntoBuffer,
  flushWriteBufferOnly, flushWriteBuffer, flushReadBuffer, 
  fillReadBuffer, fillReadBufferWithoutBlocking,
  readRawBuffer, readRawBufferPtr,
  writeRawBuffer, writeRawBufferPtr,

#ifndef mingw32_HOST_OS
  unlockFile,
#endif

  ioe_closedHandle, ioe_EOF, ioe_notReadable, ioe_notWritable,

  stdin, stdout, stderr,
  IOMode(..), openFile, openBinaryFile, openTempFile, openBinaryTempFile, openFd, fdToHandle,
  hFileSize, hSetFileSize, hIsEOF, isEOF, hLookAhead, hSetBuffering, hSetBinaryMode,
  hFlush, hDuplicate, hDuplicateTo,

  hClose, hClose_help,

  HandlePosition, HandlePosn(..), hGetPosn, hSetPosn,
  SeekMode(..), hSeek, hTell,

  hIsOpen, hIsClosed, hIsReadable, hIsWritable, hGetBuffering, hIsSeekable,
  hSetEcho, hGetEcho, hIsTerminalDevice,

  hShow,

#ifdef DEBUG_DUMP
  puts,
#endif

 ) where

import System.Directory.Internals
import Control.Monad
import Data.Bits
import Data.Maybe
import Foreign
import Foreign.C
import System.IO.Error
import System.Posix.Internals

import GHC.Real

import GHC.Arr
import GHC.Base
import GHC.Read		( Read )
import GHC.List
import GHC.IOBase
import GHC.Exception
import GHC.Enum
import GHC.Num		( Integer(..), Num(..) )
import GHC.Show
import GHC.Real		( toInteger )
#if defined(DEBUG_DUMP)
import GHC.Pack
#endif

import GHC.Conc

-- -----------------------------------------------------------------------------
-- TODO:

-- hWaitForInput blocks (should use a timeout)

-- unbuffered hGetLine is a bit dodgy

-- hSetBuffering: can't change buffering on a stream, 
--	when the read buffer is non-empty? (no way to flush the buffer)

-- ---------------------------------------------------------------------------
-- Are files opened by default in text or binary mode, if the user doesn't
-- specify?

dEFAULT_OPEN_IN_BINARY_MODE = False :: Bool

-- ---------------------------------------------------------------------------
-- Creating a new handle

newFileHandle :: FilePath -> (MVar Handle__ -> IO ()) -> Handle__ -> IO Handle
newFileHandle filepath finalizer hc = do 
  m <- newMVar hc
  addMVarFinalizer m (finalizer m)
  return (FileHandle filepath m)

-- ---------------------------------------------------------------------------
-- Working with Handles

{-
In the concurrent world, handles are locked during use.  This is done
by wrapping an MVar around the handle which acts as a mutex over
operations on the handle.

To avoid races, we use the following bracketing operations.  The idea
is to obtain the lock, do some operation and replace the lock again,
whether the operation succeeded or failed.  We also want to handle the
case where the thread receives an exception while processing the IO
operation: in these cases we also want to relinquish the lock.

There are three versions of @withHandle@: corresponding to the three
possible combinations of:

	- the operation may side-effect the handle
	- the operation may return a result

If the operation generates an error or an exception is raised, the
original handle is always replaced [ this is the case at the moment,
but we might want to revisit this in the future --SDM ].
-}

{-# INLINE withHandle #-}
withHandle :: String -> Handle -> (Handle__ -> IO (Handle__,a)) -> IO a
withHandle fun h@(FileHandle _ m)     act = withHandle' fun h m act
withHandle fun h@(DuplexHandle _ m _) act = withHandle' fun h m act

withHandle' :: String -> Handle -> MVar Handle__
   -> (Handle__ -> IO (Handle__,a)) -> IO a
withHandle' fun h m act = 
   block $ do
   h_ <- takeMVar m
   checkBufferInvariants h_
   (h',v)  <- catchException (act h_) 
		(\ err -> putMVar m h_ >>
		          case err of
		             IOException ex -> ioError (augmentIOError ex fun h)
		             _ -> throw err)
   checkBufferInvariants h'
   putMVar m h'
   return v

{-# INLINE withHandle_ #-}
withHandle_ :: String -> Handle -> (Handle__ -> IO a) -> IO a
withHandle_ fun h@(FileHandle _ m)     act = withHandle_' fun h m act
withHandle_ fun h@(DuplexHandle _ m _) act = withHandle_' fun h m act

withHandle_' :: String -> Handle -> MVar Handle__ -> (Handle__ -> IO a) -> IO a
withHandle_' fun h m act = 
   block $ do
   h_ <- takeMVar m
   checkBufferInvariants h_
   v  <- catchException (act h_) 
		(\ err -> putMVar m h_ >>
		          case err of
		             IOException ex -> ioError (augmentIOError ex fun h)
		             _ -> throw err)
   checkBufferInvariants h_
   putMVar m h_
   return v

withAllHandles__ :: String -> Handle -> (Handle__ -> IO Handle__) -> IO ()
withAllHandles__ fun h@(FileHandle _ m)     act = withHandle__' fun h m act
withAllHandles__ fun h@(DuplexHandle _ r w) act = do
  withHandle__' fun h r act
  withHandle__' fun h w act

withHandle__' fun h m act = 
   block $ do
   h_ <- takeMVar m
   checkBufferInvariants h_
   h'  <- catchException (act h_)
		(\ err -> putMVar m h_ >>
		          case err of
		             IOException ex -> ioError (augmentIOError ex fun h)
		             _ -> throw err)
   checkBufferInvariants h'
   putMVar m h'
   return ()

augmentIOError (IOError _ iot _ str fp) fun h
  = IOError (Just h) iot fun str filepath
  where filepath
	  | Just _ <- fp = fp
	  | otherwise = case h of
		    	  FileHandle fp _     -> Just fp
		    	  DuplexHandle fp _ _ -> Just fp

-- ---------------------------------------------------------------------------
-- Wrapper for write operations.

wantWritableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantWritableHandle fun h@(FileHandle _ m) act
  = wantWritableHandle' fun h m act
wantWritableHandle fun h@(DuplexHandle _ _ m) act
  = wantWritableHandle' fun h m act
  -- ToDo: in the Duplex case, we don't need to checkWritableHandle

wantWritableHandle'
	:: String -> Handle -> MVar Handle__
  	-> (Handle__ -> IO a) -> IO a
wantWritableHandle' fun h m act
   = withHandle_' fun h m (checkWritableHandle act)

checkWritableHandle act handle_
  = case haType handle_ of 
      ClosedHandle 	   -> ioe_closedHandle
      SemiClosedHandle 	   -> ioe_closedHandle
      ReadHandle 	   -> ioe_notWritable
      ReadWriteHandle  	   -> do
		let ref = haBuffer handle_
		buf <- readIORef ref
		new_buf <-
		  if not (bufferIsWritable buf)
		     then do b <- flushReadBuffer (haFD handle_) buf
			     return b{ bufState=WriteBuffer }
		     else return buf
		writeIORef ref new_buf
		act handle_
      _other 		   -> act handle_

-- ---------------------------------------------------------------------------
-- Wrapper for read operations.

wantReadableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantReadableHandle fun h@(FileHandle  _ m)   act
  = wantReadableHandle' fun h m act
wantReadableHandle fun h@(DuplexHandle _ m _) act
  = wantReadableHandle' fun h m act
  -- ToDo: in the Duplex case, we don't need to checkReadableHandle

wantReadableHandle'
	:: String -> Handle -> MVar Handle__
	-> (Handle__ -> IO a) -> IO a
wantReadableHandle' fun h m act
  = withHandle_' fun h m (checkReadableHandle act)

checkReadableHandle act handle_ = 
    case haType handle_ of 
      ClosedHandle 	   -> ioe_closedHandle
      SemiClosedHandle 	   -> ioe_closedHandle
      AppendHandle 	   -> ioe_notReadable
      WriteHandle 	   -> ioe_notReadable
      ReadWriteHandle	   -> do 
	let ref = haBuffer handle_
	buf <- readIORef ref
	when (bufferIsWritable buf) $ do
	   new_buf <- flushWriteBuffer (haFD handle_) (haIsStream handle_) buf
	   writeIORef ref new_buf{ bufState=ReadBuffer }
	act handle_
      _other 		   -> act handle_

-- ---------------------------------------------------------------------------
-- Wrapper for seek operations.

wantSeekableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantSeekableHandle fun h@(DuplexHandle _ _ _) _act =
  ioException (IOError (Just h) IllegalOperation fun 
		   "handle is not seekable" Nothing)
wantSeekableHandle fun h@(FileHandle _ m) act =
  withHandle_' fun h m (checkSeekableHandle act)
  
checkSeekableHandle act handle_ = 
    case haType handle_ of 
      ClosedHandle 	-> ioe_closedHandle
      SemiClosedHandle	-> ioe_closedHandle
      AppendHandle      -> ioe_notSeekable
      _  | haIsBin handle_ || tEXT_MODE_SEEK_ALLOWED -> act handle_
         | otherwise                                 -> ioe_notSeekable_notBin
 
-- -----------------------------------------------------------------------------
-- Handy IOErrors

ioe_closedHandle, ioe_EOF, 
  ioe_notReadable, ioe_notWritable, 
  ioe_notSeekable, ioe_notSeekable_notBin :: IO a

ioe_closedHandle = ioException 
   (IOError Nothing IllegalOperation "" 
	"handle is closed" Nothing)
ioe_EOF = ioException 
   (IOError Nothing EOF "" "" Nothing)
ioe_notReadable = ioException 
   (IOError Nothing IllegalOperation "" 
	"handle is not open for reading" Nothing)
ioe_notWritable = ioException 
   (IOError Nothing IllegalOperation "" 
	"handle is not open for writing" Nothing)
ioe_notSeekable = ioException 
   (IOError Nothing IllegalOperation ""
	"handle is not seekable" Nothing)
ioe_notSeekable_notBin = ioException 
   (IOError Nothing IllegalOperation ""
      "seek operations on text-mode handles are not allowed on this platform" 
        Nothing)
 
ioe_finalizedHandle fp = throw (IOException
   (IOError Nothing IllegalOperation "" 
	"handle is finalized" (Just fp)))

ioe_bufsiz :: Int -> IO a
ioe_bufsiz n = ioException 
   (IOError Nothing InvalidArgument "hSetBuffering"
	("illegal buffer size " ++ showsPrec 9 n []) Nothing)
				-- 9 => should be parens'ified.

-- -----------------------------------------------------------------------------
-- Handle Finalizers

-- For a duplex handle, we arrange that the read side points to the write side
-- (and hence keeps it alive if the read side is alive).  This is done by
-- having the haOtherSide field of the read side point to the read side.
-- The finalizer is then placed on the write side, and the handle only gets
-- finalized once, when both sides are no longer required.

-- NOTE about finalized handles: It's possible that a handle can be
-- finalized and then we try to use it later, for example if the
-- handle is referenced from another finalizer, or from a thread that
-- has become unreferenced and then resurrected (arguably in the
-- latter case we shouldn't finalize the Handle...).  Anyway,
-- we try to emit a helpful message which is better than nothing.

stdHandleFinalizer :: FilePath -> MVar Handle__ -> IO ()
stdHandleFinalizer fp m = do
  h_ <- takeMVar m
  flushWriteBufferOnly h_
  putMVar m (ioe_finalizedHandle fp)

handleFinalizer :: FilePath -> MVar Handle__ -> IO ()
handleFinalizer fp m = do
  handle_ <- takeMVar m
  case haType handle_ of 
      ClosedHandle -> return ()
      _ -> do flushWriteBufferOnly handle_ `catchException` \_ -> return ()
		-- ignore errors and async exceptions, and close the
		-- descriptor anyway...
	      hClose_handle_ handle_
	      return ()
  putMVar m (ioe_finalizedHandle fp)

-- ---------------------------------------------------------------------------
-- Grimy buffer operations

#ifdef DEBUG
checkBufferInvariants h_ = do
 let ref = haBuffer h_ 
 Buffer{ bufWPtr=w, bufRPtr=r, bufSize=size, bufState=state } <- readIORef ref
 if not (
  	size > 0
  	&& r <= w
  	&& w <= size
  	&& ( r /= w || (r == 0 && w == 0) )
  	&& ( state /= WriteBuffer || r == 0 )	
  	&& ( state /= WriteBuffer || w < size ) -- write buffer is never full
     )
   then error "buffer invariant violation"
   else return ()
#else
checkBufferInvariants h_ = return ()
#endif

newEmptyBuffer :: RawBuffer -> BufferState -> Int -> Buffer
newEmptyBuffer b state size
  = Buffer{ bufBuf=b, bufRPtr=0, bufWPtr=0, bufSize=size, bufState=state }

allocateBuffer :: Int -> BufferState -> IO Buffer
allocateBuffer sz@(I# size) state = IO $ \s -> 
#ifdef mingw32_HOST_OS
   -- To implement asynchronous I/O under Win32, we have to pass
   -- buffer references to external threads that handles the
   -- filling/emptying of their contents. Hence, the buffer cannot
   -- be moved around by the GC.
  case newPinnedByteArray# size s of { (# s, b #) ->
#else
  case newByteArray# size s of { (# s, b #) ->
#endif
  (# s, newEmptyBuffer b state sz #) }

writeCharIntoBuffer :: RawBuffer -> Int -> Char -> IO Int
writeCharIntoBuffer slab (I# off) (C# c)
  = IO $ \s -> case writeCharArray# slab off c s of 
		 s -> (# s, I# (off +# 1#) #)

readCharFromBuffer :: RawBuffer -> Int -> IO (Char, Int)
readCharFromBuffer slab (I# off)
  = IO $ \s -> case readCharArray# slab off s of 
		 (# s, c #) -> (# s, (C# c, I# (off +# 1#)) #)

getBuffer :: FD -> BufferState -> IO (IORef Buffer, BufferMode)
getBuffer fd state = do
  buffer <- allocateBuffer dEFAULT_BUFFER_SIZE state
  ioref  <- newIORef buffer
  is_tty <- fdIsTTY fd

  let buffer_mode 
         | is_tty    = LineBuffering 
         | otherwise = BlockBuffering Nothing

  return (ioref, buffer_mode)

mkUnBuffer :: IO (IORef Buffer)
mkUnBuffer = do
  buffer <- allocateBuffer 1 ReadBuffer
  newIORef buffer

-- flushWriteBufferOnly flushes the buffer iff it contains pending write data.
flushWriteBufferOnly :: Handle__ -> IO ()
flushWriteBufferOnly h_ = do
  let fd = haFD h_
      ref = haBuffer h_
  buf <- readIORef ref
  new_buf <- if bufferIsWritable buf 
		then flushWriteBuffer fd (haIsStream h_) buf 
		else return buf
  writeIORef ref new_buf

-- flushBuffer syncs the file with the buffer, including moving the
-- file pointer backwards in the case of a read buffer.
flushBuffer :: Handle__ -> IO ()
flushBuffer h_ = do
  let ref = haBuffer h_
  buf <- readIORef ref

  flushed_buf <-
    case bufState buf of
      ReadBuffer  -> flushReadBuffer  (haFD h_) buf
      WriteBuffer -> flushWriteBuffer (haFD h_) (haIsStream h_) buf

  writeIORef ref flushed_buf

-- When flushing a read buffer, we seek backwards by the number of
-- characters in the buffer.  The file descriptor must therefore be
-- seekable: attempting to flush the read buffer on an unseekable
-- handle is not allowed.

flushReadBuffer :: FD -> Buffer -> IO Buffer
flushReadBuffer fd buf
  | bufferEmpty buf = return buf
  | otherwise = do
     let off = negate (bufWPtr buf - bufRPtr buf)
#    ifdef DEBUG_DUMP
     puts ("flushReadBuffer: new file offset = " ++ show off ++ "\n")
#    endif
     throwErrnoIfMinus1Retry "flushReadBuffer"
   	 (c_lseek fd (fromIntegral off) sEEK_CUR)
     return buf{ bufWPtr=0, bufRPtr=0 }

flushWriteBuffer :: FD -> Bool -> Buffer -> IO Buffer
flushWriteBuffer fd is_stream buf@Buffer{ bufBuf=b, bufRPtr=r, bufWPtr=w }  =
  seq fd $ do -- strictness hack
  let bytes = w - r
#ifdef DEBUG_DUMP
  puts ("flushWriteBuffer, fd=" ++ show fd ++ ", bytes=" ++ show bytes ++ "\n")
#endif
  if bytes == 0
     then return (buf{ bufRPtr=0, bufWPtr=0 })
     else do
  res <- writeRawBuffer "flushWriteBuffer" fd is_stream b 
  			(fromIntegral r) (fromIntegral bytes)
  let res' = fromIntegral res
  if res' < bytes 
     then flushWriteBuffer fd is_stream (buf{ bufRPtr = r + res' })
     else return buf{ bufRPtr=0, bufWPtr=0 }

fillReadBuffer :: FD -> Bool -> Bool -> Buffer -> IO Buffer
fillReadBuffer fd is_line is_stream
      buf@Buffer{ bufBuf=b, bufRPtr=r, bufWPtr=w, bufSize=size } =
  -- buffer better be empty:
  assert (r == 0 && w == 0) $ do
  fillReadBufferLoop fd is_line is_stream buf b w size

-- For a line buffer, we just get the first chunk of data to arrive,
-- and don't wait for the whole buffer to be full (but we *do* wait
-- until some data arrives).  This isn't really line buffering, but it
-- appears to be what GHC has done for a long time, and I suspect it
-- is more useful than line buffering in most cases.

fillReadBufferLoop fd is_line is_stream buf b w size = do
  let bytes = size - w
  if bytes == 0  -- buffer full?
     then return buf{ bufRPtr=0, bufWPtr=w }
     else do
#ifdef DEBUG_DUMP
  puts ("fillReadBufferLoop: bytes = " ++ show bytes ++ "\n")
#endif
  res <- readRawBuffer "fillReadBuffer" fd is_stream b
  		       (fromIntegral w) (fromIntegral bytes)
  let res' = fromIntegral res
#ifdef DEBUG_DUMP
  puts ("fillReadBufferLoop:  res' = " ++ show res' ++ "\n")
#endif
  if res' == 0
     then if w == 0
	     then ioe_EOF
	     else return buf{ bufRPtr=0, bufWPtr=w }
     else if res' < bytes && not is_line
     	     then fillReadBufferLoop fd is_line is_stream buf b (w+res') size
     	     else return buf{ bufRPtr=0, bufWPtr=w+res' }
 

fillReadBufferWithoutBlocking :: FD -> Bool -> Buffer -> IO Buffer
fillReadBufferWithoutBlocking fd is_stream
      buf@Buffer{ bufBuf=b, bufRPtr=r, bufWPtr=w, bufSize=size } =
  -- buffer better be empty:
  assert (r == 0 && w == 0) $ do
#ifdef DEBUG_DUMP
  puts ("fillReadBufferLoopNoBlock: bytes = " ++ show size ++ "\n")
#endif
  res <- readRawBufferNoBlock "fillReadBuffer" fd is_stream b
  		       0 (fromIntegral size)
  let res' = fromIntegral res
#ifdef DEBUG_DUMP
  puts ("fillReadBufferLoopNoBlock:  res' = " ++ show res' ++ "\n")
#endif
  return buf{ bufRPtr=0, bufWPtr=res' }
 
-- Low level routines for reading/writing to (raw)buffers:

#ifndef mingw32_HOST_OS
readRawBuffer :: String -> FD -> Bool -> RawBuffer -> Int -> CInt -> IO CInt
readRawBuffer loc fd is_stream buf off len = 
  throwErrnoIfMinus1RetryMayBlock loc
	    (read_rawBuffer fd buf off len)
	    (threadWaitRead (fromIntegral fd))

readRawBufferNoBlock :: String -> FD -> Bool -> RawBuffer -> Int -> CInt -> IO CInt
readRawBufferNoBlock loc fd is_stream buf off len = 
  throwErrnoIfMinus1RetryOnBlock loc
	    (read_rawBuffer fd buf off len)
	    (return 0)

readRawBufferPtr :: String -> FD -> Bool -> Ptr CChar -> Int -> CInt -> IO CInt
readRawBufferPtr loc fd is_stream buf off len = 
  throwErrnoIfMinus1RetryMayBlock loc
	    (read_off fd buf off len)
	    (threadWaitRead (fromIntegral fd))

writeRawBuffer :: String -> FD -> Bool -> RawBuffer -> Int -> CInt -> IO CInt
writeRawBuffer loc fd is_stream buf off len = 
  throwErrnoIfMinus1RetryMayBlock loc
		(write_rawBuffer fd buf off len)
		(threadWaitWrite (fromIntegral fd))

writeRawBufferPtr :: String -> FD -> Bool -> Ptr CChar -> Int -> CInt -> IO CInt
writeRawBufferPtr loc fd is_stream buf off len = 
  throwErrnoIfMinus1RetryMayBlock loc
		(write_off fd buf off len)
		(threadWaitWrite (fromIntegral fd))

foreign import ccall unsafe "__hscore_PrelHandle_read"
   read_rawBuffer :: FD -> RawBuffer -> Int -> CInt -> IO CInt

foreign import ccall unsafe "__hscore_PrelHandle_read"
   read_off :: FD -> Ptr CChar -> Int -> CInt -> IO CInt

foreign import ccall unsafe "__hscore_PrelHandle_write"
   write_rawBuffer :: CInt -> RawBuffer -> Int -> CInt -> IO CInt

foreign import ccall unsafe "__hscore_PrelHandle_write"
   write_off :: CInt -> Ptr CChar -> Int -> CInt -> IO CInt

#else /* mingw32_HOST_OS.... */

readRawBuffer :: String -> FD -> Bool -> RawBuffer -> Int -> CInt -> IO CInt
readRawBuffer loc fd is_stream buf off len
  | threaded  = blockingReadRawBuffer loc fd is_stream buf off len
  | otherwise = asyncReadRawBuffer loc fd is_stream buf off len

readRawBufferPtr :: String -> FD -> Bool -> Ptr CChar -> Int -> CInt -> IO CInt
readRawBufferPtr loc fd is_stream buf off len
  | threaded  = blockingReadRawBufferPtr loc fd is_stream buf off len
  | otherwise = asyncReadRawBufferPtr loc fd is_stream buf off len

writeRawBuffer :: String -> FD -> Bool -> RawBuffer -> Int -> CInt -> IO CInt
writeRawBuffer loc fd is_stream buf off len
  | threaded =  blockingWriteRawBuffer loc fd is_stream buf off len
  | otherwise = asyncWriteRawBuffer    loc fd is_stream buf off len

writeRawBufferPtr :: String -> FD -> Bool -> Ptr CChar -> Int -> CInt -> IO CInt
writeRawBufferPtr loc fd is_stream buf off len
  | threaded  = blockingWriteRawBufferPtr loc fd is_stream buf off len
  | otherwise = asyncWriteRawBufferPtr    loc fd is_stream buf off len

-- ToDo: we don't have a non-blocking primitve read on Win32
readRawBufferNoBlock :: String -> FD -> Bool -> RawBuffer -> Int -> CInt -> IO CInt
readRawBufferNoBlock = readRawBuffer

-- Async versions of the read/write primitives, for the non-threaded RTS

asyncReadRawBuffer loc fd is_stream buf off len = do
    (l, rc) <- asyncReadBA (fromIntegral fd) (if is_stream then 1 else 0) 
		 (fromIntegral len) off buf
    if l == (-1)
      then 
	ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)
      else return (fromIntegral l)

asyncReadRawBufferPtr loc fd is_stream buf off len = do
    (l, rc) <- asyncRead (fromIntegral fd) (if is_stream then 1 else 0) 
			(fromIntegral len) (buf `plusPtr` off)
    if l == (-1)
      then 
        ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)
      else return (fromIntegral l)

asyncWriteRawBuffer loc fd is_stream buf off len = do
    (l, rc) <- asyncWriteBA (fromIntegral fd) (if is_stream then 1 else 0) 
			(fromIntegral len) off buf
    if l == (-1)
      then 
        ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)
      else return (fromIntegral l)

asyncWriteRawBufferPtr loc fd is_stream buf off len = do
    (l, rc) <- asyncWrite (fromIntegral fd) (if is_stream then 1 else 0) 
		  (fromIntegral len) (buf `plusPtr` off)
    if l == (-1)
      then 
        ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)
      else return (fromIntegral l)

-- Blocking versions of the read/write primitives, for the threaded RTS

blockingReadRawBuffer loc fd True buf off len = 
  throwErrnoIfMinus1Retry loc $
    recv_rawBuffer fd buf off len
blockingReadRawBuffer loc fd False buf off len = 
  throwErrnoIfMinus1Retry loc $
    read_rawBuffer fd buf off len

blockingReadRawBufferPtr loc fd True buf off len = 
  throwErrnoIfMinus1Retry loc $
    recv_off fd buf off len
blockingReadRawBufferPtr loc fd False buf off len = 
  throwErrnoIfMinus1Retry loc $
    read_off fd buf off len

blockingWriteRawBuffer loc fd True buf off len = 
  throwErrnoIfMinus1Retry loc $
    send_rawBuffer fd buf off len
blockingWriteRawBuffer loc fd False buf off len = 
  throwErrnoIfMinus1Retry loc $
    write_rawBuffer fd buf off len

blockingWriteRawBufferPtr loc fd True buf off len = 
  throwErrnoIfMinus1Retry loc $
    send_off fd buf off len
blockingWriteRawBufferPtr loc fd False buf off len = 
  throwErrnoIfMinus1Retry loc $
    write_off fd buf off len

-- NOTE: "safe" versions of the read/write calls for use by the threaded RTS.
-- These calls may block, but that's ok.

foreign import ccall safe "__hscore_PrelHandle_read"
   read_rawBuffer :: FD -> RawBuffer -> Int -> CInt -> IO CInt

foreign import ccall safe "__hscore_PrelHandle_read"
   read_off :: FD -> Ptr CChar -> Int -> CInt -> IO CInt

foreign import ccall safe "__hscore_PrelHandle_write"
   write_rawBuffer :: CInt -> RawBuffer -> Int -> CInt -> IO CInt

foreign import ccall safe "__hscore_PrelHandle_write"
   write_off :: CInt -> Ptr CChar -> Int -> CInt -> IO CInt

foreign import ccall safe "__hscore_PrelHandle_recv"
   recv_rawBuffer :: FD -> RawBuffer -> Int -> CInt -> IO CInt

foreign import ccall safe "__hscore_PrelHandle_recv"
   recv_off :: FD -> Ptr CChar -> Int -> CInt -> IO CInt

foreign import ccall safe "__hscore_PrelHandle_send"
   send_rawBuffer :: CInt -> RawBuffer -> Int -> CInt -> IO CInt

foreign import ccall safe "__hscore_PrelHandle_send"
   send_off :: CInt -> Ptr CChar -> Int -> CInt -> IO CInt

foreign import ccall "rtsSupportsBoundThreads" threaded :: Bool
#endif

-- ---------------------------------------------------------------------------
-- Standard Handles

-- Three handles are allocated during program initialisation.  The first
-- two manage input or output from the Haskell program's standard input
-- or output channel respectively.  The third manages output to the
-- standard error channel. These handles are initially open.

fd_stdin  = 0 :: FD
fd_stdout = 1 :: FD
fd_stderr = 2 :: FD

-- | A handle managing input from the Haskell program's standard input channel.
stdin :: Handle
stdin = unsafePerformIO $ do
   -- ToDo: acquire lock
   setNonBlockingFD fd_stdin
   (buf, bmode) <- getBuffer fd_stdin ReadBuffer
   mkStdHandle fd_stdin "<stdin>" ReadHandle buf bmode

-- | A handle managing output to the Haskell program's standard output channel.
stdout :: Handle
stdout = unsafePerformIO $ do
   -- ToDo: acquire lock
   -- We don't set non-blocking mode on stdout or sterr, because
   -- some shells don't recover properly.
   -- setNonBlockingFD fd_stdout
   (buf, bmode) <- getBuffer fd_stdout WriteBuffer
   mkStdHandle fd_stdout "<stdout>" WriteHandle buf bmode

-- | A handle managing output to the Haskell program's standard error channel.
stderr :: Handle
stderr = unsafePerformIO $ do
    -- ToDo: acquire lock
   -- We don't set non-blocking mode on stdout or sterr, because
   -- some shells don't recover properly.
   -- setNonBlockingFD fd_stderr
   buf <- mkUnBuffer
   mkStdHandle fd_stderr "<stderr>" WriteHandle buf NoBuffering

-- ---------------------------------------------------------------------------
-- Opening and Closing Files

addFilePathToIOError fun fp (IOError h iot _ str _)
  = IOError h iot fun str (Just fp)

-- | Computation 'openFile' @file mode@ allocates and returns a new, open
-- handle to manage the file @file@.  It manages input if @mode@
-- is 'ReadMode', output if @mode@ is 'WriteMode' or 'AppendMode',
-- and both input and output if mode is 'ReadWriteMode'.
--
-- If the file does not exist and it is opened for output, it should be
-- created as a new file.  If @mode@ is 'WriteMode' and the file
-- already exists, then it should be truncated to zero length.
-- Some operating systems delete empty files, so there is no guarantee
-- that the file will exist following an 'openFile' with @mode@
-- 'WriteMode' unless it is subsequently written to successfully.
-- The handle is positioned at the end of the file if @mode@ is
-- 'AppendMode', and otherwise at the beginning (in which case its
-- internal position is 0).
-- The initial buffer mode is implementation-dependent.
--
-- This operation may fail with:
--
--  * 'isAlreadyInUseError' if the file is already open and cannot be reopened;
--
--  * 'isDoesNotExistError' if the file does not exist; or
--
--  * 'isPermissionError' if the user does not have permission to open the file.
--
-- Note: if you will be working with files containing binary data, you'll want to
-- be using 'openBinaryFile'.
openFile :: FilePath -> IOMode -> IO Handle
openFile fp im = 
  catch 
    (openFile' fp im dEFAULT_OPEN_IN_BINARY_MODE)
    (\e -> ioError (addFilePathToIOError "openFile" fp e))

-- | Like 'openFile', but open the file in binary mode.
-- On Windows, reading a file in text mode (which is the default)
-- will translate CRLF to LF, and writing will translate LF to CRLF.
-- This is usually what you want with text files.  With binary files
-- this is undesirable; also, as usual under Microsoft operating systems,
-- text mode treats control-Z as EOF.  Binary mode turns off all special
-- treatment of end-of-line and end-of-file characters.
-- (See also 'hSetBinaryMode'.)

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile fp m =
  catch
    (openFile' fp m True)
    (\e -> ioError (addFilePathToIOError "openBinaryFile" fp e))

openFile' filepath mode binary =
  withCString filepath $ \ f ->

    let 
      oflags1 = case mode of
	    	  ReadMode      -> read_flags
#ifdef mingw32_HOST_OS
	    	  WriteMode     -> write_flags .|. o_TRUNC
#else
	    	  WriteMode     -> write_flags
#endif
	    	  ReadWriteMode -> rw_flags
	    	  AppendMode    -> append_flags

      binary_flags
	  | binary    = o_BINARY
	  | otherwise = 0

      oflags = oflags1 .|. binary_flags
    in do

    -- the old implementation had a complicated series of three opens,
    -- which is perhaps because we have to be careful not to open
    -- directories.  However, the man pages I've read say that open()
    -- always returns EISDIR if the file is a directory and was opened
    -- for writing, so I think we're ok with a single open() here...
    fd <- throwErrnoIfMinus1Retry "openFile"
 	        (c_open f (fromIntegral oflags) 0o666)

    fd_type <- fdType fd

    h <- openFd fd (Just fd_type) False filepath mode binary
            `catchException` \e -> do c_close fd; throw e
	-- NB. don't forget to close the FD if openFd fails, otherwise
	-- this FD leaks.
	-- ASSERT: if we just created the file, then openFd won't fail
	-- (so we don't need to worry about removing the newly created file
	--  in the event of an error).

#ifndef mingw32_HOST_OS
	-- we want to truncate() if this is an open in WriteMode, but only
	-- if the target is a RegularFile.  ftruncate() fails on special files
	-- like /dev/null.
    if mode == WriteMode && fd_type == RegularFile
      then throwErrnoIf (/=0) "openFile" 
              (c_ftruncate fd 0)
      else return 0
#endif
    return h


-- | The function creates a temporary file in ReadWrite mode.
-- The created file isn\'t deleted automatically, so you need to delete it manually.
openTempFile :: FilePath   -- ^ Directory in which to create the file
             -> String     -- ^ File name template. If the template is \"foo.ext\" then
                           -- the create file will be \"fooXXX.ext\" where XXX is some
                           -- random number.
             -> IO (FilePath, Handle)
openTempFile tmp_dir template = openTempFile' "openTempFile" tmp_dir template dEFAULT_OPEN_IN_BINARY_MODE

-- | Like 'openTempFile', but opens the file in binary mode. See 'openBinaryFile' for more comments.
openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile tmp_dir template = openTempFile' "openBinaryTempFile" tmp_dir template True

openTempFile' :: String -> FilePath -> String -> Bool -> IO (FilePath, Handle)
openTempFile' loc tmp_dir template binary = do
  pid <- c_getpid
  findTempName pid
  where
    (prefix,suffix) = break (=='.') template

    oflags1 = rw_flags .|. o_EXCL

    binary_flags
      | binary    = o_BINARY
      | otherwise = 0

    oflags = oflags1 .|. binary_flags

    findTempName x = do
      fd <- withCString filepath $ \ f ->
              c_open f oflags 0o666
      if fd < 0 
       then do
         errno <- getErrno
         if errno == eEXIST
           then findTempName (x+1)
           else ioError (errnoToIOError loc errno Nothing (Just tmp_dir))
       else do
         h <- openFd fd Nothing False filepath ReadWriteMode True
	        `catchException` \e -> do c_close fd; throw e
	 return (filepath, h)
      where
        filename        = prefix ++ show x ++ suffix
        filepath        = tmp_dir `joinFileName` filename


std_flags    = o_NONBLOCK   .|. o_NOCTTY
output_flags = std_flags    .|. o_CREAT
read_flags   = std_flags    .|. o_RDONLY 
write_flags  = output_flags .|. o_WRONLY
rw_flags     = output_flags .|. o_RDWR
append_flags = write_flags  .|. o_APPEND

-- ---------------------------------------------------------------------------
-- openFd

openFd :: FD -> Maybe FDType -> Bool -> FilePath -> IOMode -> Bool -> IO Handle
openFd fd mb_fd_type is_socket filepath mode binary = do
    -- turn on non-blocking mode
    setNonBlockingFD fd

    let (ha_type, write) =
	  case mode of
	    ReadMode      -> ( ReadHandle,      False )
	    WriteMode     -> ( WriteHandle,     True )
	    ReadWriteMode -> ( ReadWriteHandle, True )
	    AppendMode    -> ( AppendHandle,    True )

    -- open() won't tell us if it was a directory if we only opened for
    -- reading, so check again.
    fd_type <- 
      case mb_fd_type of
        Just x  -> return x
	Nothing -> fdType fd

    case fd_type of
	Directory -> 
	   ioException (IOError Nothing InappropriateType "openFile"
			   "is a directory" Nothing) 

	-- regular files need to be locked
	RegularFile -> do
#ifndef mingw32_HOST_OS
	   r <- lockFile fd (fromBool write) 1{-exclusive-}
	   when (r == -1)  $
		ioException (IOError Nothing ResourceBusy "openFile"
				   "file is locked" Nothing)
#endif
	   mkFileHandle fd is_socket filepath ha_type binary

	Stream
	   -- only *Streams* can be DuplexHandles.  Other read/write
	   -- Handles must share a buffer.
	   | ReadWriteHandle <- ha_type -> 
		mkDuplexHandle fd is_socket filepath binary
	   | otherwise ->
		mkFileHandle   fd is_socket filepath ha_type binary

	RawDevice -> 
		mkFileHandle fd is_socket filepath ha_type binary

fdToHandle :: FD -> IO Handle
fdToHandle fd = do
   mode <- fdGetMode fd
   let fd_str = "<file descriptor: " ++ show fd ++ ">"
   openFd fd Nothing False{-XXX!-} fd_str mode True{-bin mode-}


#ifndef mingw32_HOST_OS
foreign import ccall unsafe "lockFile"
  lockFile :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "unlockFile"
  unlockFile :: CInt -> IO CInt
#endif

mkStdHandle :: FD -> FilePath -> HandleType -> IORef Buffer -> BufferMode
	-> IO Handle
mkStdHandle fd filepath ha_type buf bmode = do
   spares <- newIORef BufferListNil
   newFileHandle filepath (stdHandleFinalizer filepath)
	    (Handle__ { haFD = fd,
			haType = ha_type,
                        haIsBin = dEFAULT_OPEN_IN_BINARY_MODE,
			haIsStream = False,
			haBufferMode = bmode,
			haBuffer = buf,
			haBuffers = spares,
			haOtherSide = Nothing
		      })

mkFileHandle :: FD -> Bool -> FilePath -> HandleType -> Bool -> IO Handle
mkFileHandle fd is_stream filepath ha_type binary = do
  (buf, bmode) <- getBuffer fd (initBufferState ha_type)

#ifdef mingw32_HOST_OS
  -- On Windows, if this is a read/write handle and we are in text mode,
  -- turn off buffering.  We don't correctly handle the case of switching
  -- from read mode to write mode on a buffered text-mode handle, see bug
  -- \#679.
  bmode <- case ha_type of
      		ReadWriteHandle | not binary -> return NoBuffering
		_other			     -> return bmode
#endif

  spares <- newIORef BufferListNil
  newFileHandle filepath (handleFinalizer filepath)
	    (Handle__ { haFD = fd,
			haType = ha_type,
                        haIsBin = binary,
			haIsStream = is_stream,
			haBufferMode = bmode,
			haBuffer = buf,
			haBuffers = spares,
		        haOtherSide = Nothing
		      })

mkDuplexHandle :: FD -> Bool -> FilePath -> Bool -> IO Handle
mkDuplexHandle fd is_stream filepath binary = do
  (w_buf, w_bmode) <- getBuffer fd WriteBuffer
  w_spares <- newIORef BufferListNil
  let w_handle_ = 
	     Handle__ { haFD = fd,
			haType = WriteHandle,
                        haIsBin = binary,
			haIsStream = is_stream,
			haBufferMode = w_bmode,
			haBuffer = w_buf,
			haBuffers = w_spares,
			haOtherSide = Nothing
		      }
  write_side <- newMVar w_handle_

  (r_buf, r_bmode) <- getBuffer fd ReadBuffer
  r_spares <- newIORef BufferListNil
  let r_handle_ = 
	     Handle__ { haFD = fd,
			haType = ReadHandle,
                        haIsBin = binary,
			haIsStream = is_stream,
			haBufferMode = r_bmode,
			haBuffer = r_buf,
			haBuffers = r_spares,
			haOtherSide = Just write_side
		      }
  read_side <- newMVar r_handle_

  addMVarFinalizer write_side (handleFinalizer filepath write_side)
  return (DuplexHandle filepath read_side write_side)
   

initBufferState ReadHandle = ReadBuffer
initBufferState _ 	   = WriteBuffer

-- ---------------------------------------------------------------------------
-- Closing a handle

-- | Computation 'hClose' @hdl@ makes handle @hdl@ closed.  Before the
-- computation finishes, if @hdl@ is writable its buffer is flushed as
-- for 'hFlush'.
-- Performing 'hClose' on a handle that has already been closed has no effect; 
-- doing so not an error.  All other operations on a closed handle will fail.
-- If 'hClose' fails for any reason, any further operations (apart from
-- 'hClose') on the handle will still fail as if @hdl@ had been successfully
-- closed.

hClose :: Handle -> IO ()
hClose h@(FileHandle _ m)     = hClose' h m
hClose h@(DuplexHandle _ r w) = hClose' h w >> hClose' h r

hClose' h m = withHandle__' "hClose" h m $ hClose_help

-- hClose_help is also called by lazyRead (in PrelIO) when EOF is read
-- or an IO error occurs on a lazy stream.  The semi-closed Handle is
-- then closed immediately.  We have to be careful with DuplexHandles
-- though: we have to leave the closing to the finalizer in that case,
-- because the write side may still be in use.
hClose_help :: Handle__ -> IO Handle__
hClose_help handle_ =
  case haType handle_ of 
      ClosedHandle -> return handle_
      _ -> do flushWriteBufferOnly handle_ -- interruptible
	      hClose_handle_ handle_

hClose_handle_ handle_ = do
    let fd = haFD handle_

    -- close the file descriptor, but not when this is the read
    -- side of a duplex handle.
    case haOtherSide handle_ of
      Nothing ->
  		  throwErrnoIfMinus1Retry_ "hClose" 
#ifdef mingw32_HOST_OS
	    			(closeFd (haIsStream handle_) fd)
#else
	    			(c_close fd)
#endif
      Just _  -> return ()

    -- free the spare buffers
    writeIORef (haBuffers handle_) BufferListNil
  
#ifndef mingw32_HOST_OS
    -- unlock it
    unlockFile fd
#endif

    -- we must set the fd to -1, because the finalizer is going
    -- to run eventually and try to close/unlock it.
    return (handle_{ haFD        = -1, 
  		     haType      = ClosedHandle
  		   })

-----------------------------------------------------------------------------
-- Detecting and changing the size of a file

-- | For a handle @hdl@ which attached to a physical file,
-- 'hFileSize' @hdl@ returns the size of that file in 8-bit bytes.

hFileSize :: Handle -> IO Integer
hFileSize handle =
    withHandle_ "hFileSize" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle 		-> ioe_closedHandle
      SemiClosedHandle 		-> ioe_closedHandle
      _ -> do flushWriteBufferOnly handle_
	      r <- fdFileSize (haFD handle_)
	      if r /= -1
		 then return r
		 else ioException (IOError Nothing InappropriateType "hFileSize"
				   "not a regular file" Nothing)


-- | 'hSetFileSize' @hdl@ @size@ truncates the physical file with handle @hdl@ to @size@ bytes.

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize handle size =
    withHandle_ "hSetFileSize" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle 		-> ioe_closedHandle
      SemiClosedHandle 		-> ioe_closedHandle
      _ -> do flushWriteBufferOnly handle_
	      throwErrnoIf (/=0) "hSetFileSize" 
	         (c_ftruncate (haFD handle_) (fromIntegral size))
	      return ()

-- ---------------------------------------------------------------------------
-- Detecting the End of Input

-- | For a readable handle @hdl@, 'hIsEOF' @hdl@ returns
-- 'True' if no further input can be taken from @hdl@ or for a
-- physical file, if the current I\/O position is equal to the length of
-- the file.  Otherwise, it returns 'False'.

hIsEOF :: Handle -> IO Bool
hIsEOF handle =
  catch
     (do hLookAhead handle; return False)
     (\e -> if isEOFError e then return True else ioError e)

-- | The computation 'isEOF' is identical to 'hIsEOF',
-- except that it works only on 'stdin'.

isEOF :: IO Bool
isEOF = hIsEOF stdin

-- ---------------------------------------------------------------------------
-- Looking ahead

-- | Computation 'hLookAhead' returns the next character from the handle
-- without removing it from the input buffer, blocking until a character
-- is available.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.

hLookAhead :: Handle -> IO Char
hLookAhead handle = do
  wantReadableHandle "hLookAhead"  handle $ \handle_ -> do
  let ref     = haBuffer handle_
      fd      = haFD handle_
      is_line = haBufferMode handle_ == LineBuffering
  buf <- readIORef ref

  -- fill up the read buffer if necessary
  new_buf <- if bufferEmpty buf
		then fillReadBuffer fd True (haIsStream handle_) buf
		else return buf
  
  writeIORef ref new_buf

  (c,_) <- readCharFromBuffer (bufBuf buf) (bufRPtr buf)
  return c

-- ---------------------------------------------------------------------------
-- Buffering Operations

-- Three kinds of buffering are supported: line-buffering,
-- block-buffering or no-buffering.  See GHC.IOBase for definition and
-- further explanation of what the type represent.

-- | Computation 'hSetBuffering' @hdl mode@ sets the mode of buffering for
-- handle @hdl@ on subsequent reads and writes.
--
-- If the buffer mode is changed from 'BlockBuffering' or
-- 'LineBuffering' to 'NoBuffering', then
--
--  * if @hdl@ is writable, the buffer is flushed as for 'hFlush';
--
--  * if @hdl@ is not writable, the contents of the buffer is discarded.
--
-- This operation may fail with:
--
--  * 'isPermissionError' if the handle has already been used for reading
--    or writing and the implementation does not allow the buffering mode
--    to be changed.

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering handle mode =
  withAllHandles__ "hSetBuffering" handle $ \ handle_ -> do
  case haType handle_ of
    ClosedHandle -> ioe_closedHandle
    _ -> do
	 {- Note:
	    - we flush the old buffer regardless of whether
	      the new buffer could fit the contents of the old buffer 
	      or not.
	    - allow a handle's buffering to change even if IO has
	      occurred (ANSI C spec. does not allow this, nor did
	      the previous implementation of IO.hSetBuffering).
	    - a non-standard extension is to allow the buffering
	      of semi-closed handles to change [sof 6/98]
	  -}
	  flushBuffer handle_

	  let state = initBufferState (haType handle_)
	  new_buf <-
	    case mode of
		-- we always have a 1-character read buffer for 
		-- unbuffered  handles: it's needed to 
		-- support hLookAhead.
	      NoBuffering            ->	allocateBuffer 1 ReadBuffer
	      LineBuffering          -> allocateBuffer dEFAULT_BUFFER_SIZE state
	      BlockBuffering Nothing ->	allocateBuffer dEFAULT_BUFFER_SIZE state
	      BlockBuffering (Just n) | n <= 0    -> ioe_bufsiz n
				      | otherwise -> allocateBuffer n state
	  writeIORef (haBuffer handle_) new_buf

	  -- for input terminals we need to put the terminal into
	  -- cooked or raw mode depending on the type of buffering.
	  is_tty <- fdIsTTY (haFD handle_)
	  when (is_tty && isReadableHandleType (haType handle_)) $
		case mode of
#ifndef mingw32_HOST_OS
	-- 'raw' mode under win32 is a bit too specialised (and troublesome
	-- for most common uses), so simply disable its use here.
		  NoBuffering -> setCooked (haFD handle_) False
#else
		  NoBuffering -> return ()
#endif
		  _           -> setCooked (haFD handle_) True

 	  -- throw away spare buffers, they might be the wrong size
	  writeIORef (haBuffers handle_) BufferListNil

	  return (handle_{ haBufferMode = mode })

-- -----------------------------------------------------------------------------
-- hFlush

-- | The action 'hFlush' @hdl@ causes any items buffered for output
-- in handle @hdl@ to be sent immediately to the operating system.
--
-- This operation may fail with:
--
--  * 'isFullError' if the device is full;
--
--  * 'isPermissionError' if a system resource limit would be exceeded.
--    It is unspecified whether the characters in the buffer are discarded
--    or retained under these circumstances.

hFlush :: Handle -> IO () 
hFlush handle =
   wantWritableHandle "hFlush" handle $ \ handle_ -> do
   buf <- readIORef (haBuffer handle_)
   if bufferIsWritable buf && not (bufferEmpty buf)
	then do flushed_buf <- flushWriteBuffer (haFD handle_) (haIsStream handle_) buf
		writeIORef (haBuffer handle_) flushed_buf
	else return ()


-- -----------------------------------------------------------------------------
-- Repositioning Handles

data HandlePosn = HandlePosn Handle HandlePosition

instance Eq HandlePosn where
    (HandlePosn h1 p1) == (HandlePosn h2 p2) = p1==p2 && h1==h2

instance Show HandlePosn where
   showsPrec p (HandlePosn h pos) = 
	showsPrec p h . showString " at position " . shows pos

  -- HandlePosition is the Haskell equivalent of POSIX' off_t.
  -- We represent it as an Integer on the Haskell side, but
  -- cheat slightly in that hGetPosn calls upon a C helper
  -- that reports the position back via (merely) an Int.
type HandlePosition = Integer

-- | Computation 'hGetPosn' @hdl@ returns the current I\/O position of
-- @hdl@ as a value of the abstract type 'HandlePosn'.

hGetPosn :: Handle -> IO HandlePosn
hGetPosn handle = do
    posn <- hTell handle
    return (HandlePosn handle posn)

-- | If a call to 'hGetPosn' @hdl@ returns a position @p@,
-- then computation 'hSetPosn' @p@ sets the position of @hdl@
-- to the position it held at the time of the call to 'hGetPosn'.
--
-- This operation may fail with:
--
--  * 'isPermissionError' if a system resource limit would be exceeded.

hSetPosn :: HandlePosn -> IO () 
hSetPosn (HandlePosn h i) = hSeek h AbsoluteSeek i

-- ---------------------------------------------------------------------------
-- hSeek

-- | A mode that determines the effect of 'hSeek' @hdl mode i@, as follows:
data SeekMode
  = AbsoluteSeek	-- ^ the position of @hdl@ is set to @i@.
  | RelativeSeek	-- ^ the position of @hdl@ is set to offset @i@
			-- from the current position.
  | SeekFromEnd		-- ^ the position of @hdl@ is set to offset @i@
			-- from the end of the file.
    deriving (Eq, Ord, Ix, Enum, Read, Show)

{- Note: 
 - when seeking using `SeekFromEnd', positive offsets (>=0) means
   seeking at or past EOF.

 - we possibly deviate from the report on the issue of seeking within
   the buffer and whether to flush it or not.  The report isn't exactly
   clear here.
-}

-- | Computation 'hSeek' @hdl mode i@ sets the position of handle
-- @hdl@ depending on @mode@.
-- The offset @i@ is given in terms of 8-bit bytes.
--
-- If @hdl@ is block- or line-buffered, then seeking to a position which is not
-- in the current buffer will first cause any items in the output buffer to be
-- written to the device, and then cause the input buffer to be discarded.
-- Some handles may not be seekable (see 'hIsSeekable'), or only support a
-- subset of the possible positioning operations (for instance, it may only
-- be possible to seek to the end of a tape, or to a positive offset from
-- the beginning or current position).
-- It is not possible to set a negative I\/O position, or for
-- a physical file, an I\/O position beyond the current end-of-file.
--
-- This operation may fail with:
--
--  * 'isPermissionError' if a system resource limit would be exceeded.

hSeek :: Handle -> SeekMode -> Integer -> IO () 
hSeek handle mode offset =
    wantSeekableHandle "hSeek" handle $ \ handle_ -> do
#   ifdef DEBUG_DUMP
    puts ("hSeek " ++ show (mode,offset) ++ "\n")
#   endif
    let ref = haBuffer handle_
    buf <- readIORef ref
    let r = bufRPtr buf
        w = bufWPtr buf
        fd = haFD handle_

    let do_seek =
	  throwErrnoIfMinus1Retry_ "hSeek"
	    (c_lseek (haFD handle_) (fromIntegral offset) whence)

        whence :: CInt
        whence = case mode of
                   AbsoluteSeek -> sEEK_SET
                   RelativeSeek -> sEEK_CUR
                   SeekFromEnd  -> sEEK_END

    if bufferIsWritable buf
	then do new_buf <- flushWriteBuffer fd (haIsStream handle_) buf
	        writeIORef ref new_buf
	        do_seek
	else do

    if mode == RelativeSeek && offset >= 0 && offset < fromIntegral (w - r)
	then writeIORef ref buf{ bufRPtr = r + fromIntegral offset }
	else do 

    new_buf <- flushReadBuffer (haFD handle_) buf
    writeIORef ref new_buf
    do_seek


hTell :: Handle -> IO Integer
hTell handle = 
    wantSeekableHandle "hGetPosn" handle $ \ handle_ -> do

#if defined(mingw32_HOST_OS)
	-- urgh, on Windows we have to worry about \n -> \r\n translation, 
	-- so we can't easily calculate the file position using the
	-- current buffer size.  Just flush instead.
      flushBuffer handle_
#endif
      let fd = haFD handle_
      posn <- fromIntegral `liftM`
	        throwErrnoIfMinus1Retry "hGetPosn"
		   (c_lseek fd 0 sEEK_CUR)

      let ref = haBuffer handle_
      buf <- readIORef ref

      let real_posn 
	   | bufferIsWritable buf = posn + fromIntegral (bufWPtr buf)
	   | otherwise = posn - fromIntegral (bufWPtr buf - bufRPtr buf)
#     ifdef DEBUG_DUMP
      puts ("\nhGetPosn: (fd, posn, real_posn) = " ++ show (fd, posn, real_posn) ++ "\n")
      puts ("   (bufWPtr, bufRPtr) = " ++ show (bufWPtr buf, bufRPtr buf) ++ "\n")
#     endif
      return real_posn

-- -----------------------------------------------------------------------------
-- Handle Properties

-- A number of operations return information about the properties of a
-- handle.  Each of these operations returns `True' if the handle has
-- the specified property, and `False' otherwise.

hIsOpen :: Handle -> IO Bool
hIsOpen handle =
    withHandle_ "hIsOpen" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle         -> return False
      SemiClosedHandle     -> return False
      _ 		   -> return True

hIsClosed :: Handle -> IO Bool
hIsClosed handle =
    withHandle_ "hIsClosed" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle 	   -> return True
      _ 		   -> return False

{- not defined, nor exported, but mentioned
   here for documentation purposes:

    hSemiClosed :: Handle -> IO Bool
    hSemiClosed h = do
       ho <- hIsOpen h
       hc <- hIsClosed h
       return (not (ho || hc))
-}

hIsReadable :: Handle -> IO Bool
hIsReadable (DuplexHandle _ _ _) = return True
hIsReadable handle =
    withHandle_ "hIsReadable" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle 	   -> ioe_closedHandle
      SemiClosedHandle 	   -> ioe_closedHandle
      htype 		   -> return (isReadableHandleType htype)

hIsWritable :: Handle -> IO Bool
hIsWritable (DuplexHandle _ _ _) = return True
hIsWritable handle =
    withHandle_ "hIsWritable" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle 	   -> ioe_closedHandle
      SemiClosedHandle 	   -> ioe_closedHandle
      htype 		   -> return (isWritableHandleType htype)

-- | Computation 'hGetBuffering' @hdl@ returns the current buffering mode
-- for @hdl@.

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering handle = 
    withHandle_ "hGetBuffering" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle 	   -> ioe_closedHandle
      _ -> 
	   -- We're being non-standard here, and allow the buffering
	   -- of a semi-closed handle to be queried.   -- sof 6/98
	  return (haBufferMode handle_)  -- could be stricter..

hIsSeekable :: Handle -> IO Bool
hIsSeekable handle =
    withHandle_ "hIsSeekable" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle 	   -> ioe_closedHandle
      SemiClosedHandle 	   -> ioe_closedHandle
      AppendHandle 	   -> return False
      _                    -> do t <- fdType (haFD handle_)
                                 return ((t == RegularFile    || t == RawDevice)
                                         && (haIsBin handle_  || tEXT_MODE_SEEK_ALLOWED))

-- -----------------------------------------------------------------------------
-- Changing echo status (Non-standard GHC extensions)

-- | Set the echoing status of a handle connected to a terminal.

hSetEcho :: Handle -> Bool -> IO ()
hSetEcho handle on = do
    isT   <- hIsTerminalDevice handle
    if not isT
     then return ()
     else
      withHandle_ "hSetEcho" handle $ \ handle_ -> do
      case haType handle_ of 
         ClosedHandle -> ioe_closedHandle
         _            -> setEcho (haFD handle_) on

-- | Get the echoing status of a handle connected to a terminal.

hGetEcho :: Handle -> IO Bool
hGetEcho handle = do
    isT   <- hIsTerminalDevice handle
    if not isT
     then return False
     else
       withHandle_ "hGetEcho" handle $ \ handle_ -> do
       case haType handle_ of 
         ClosedHandle -> ioe_closedHandle
         _            -> getEcho (haFD handle_)

-- | Is the handle connected to a terminal?

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice handle = do
    withHandle_ "hIsTerminalDevice" handle $ \ handle_ -> do
     case haType handle_ of 
       ClosedHandle -> ioe_closedHandle
       _            -> fdIsTTY (haFD handle_)

-- -----------------------------------------------------------------------------
-- hSetBinaryMode

-- | Select binary mode ('True') or text mode ('False') on a open handle.
-- (See also 'openBinaryFile'.)

hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode handle bin =
  withAllHandles__ "hSetBinaryMode" handle $ \ handle_ ->
    do throwErrnoIfMinus1_ "hSetBinaryMode"
          (setmode (haFD handle_) bin)
       return handle_{haIsBin=bin}
  
foreign import ccall unsafe "__hscore_setmode"
  setmode :: CInt -> Bool -> IO CInt

-- -----------------------------------------------------------------------------
-- Duplicating a Handle

-- | Returns a duplicate of the original handle, with its own buffer.
-- The two Handles will share a file pointer, however.  The original
-- handle's buffer is flushed, including discarding any input data,
-- before the handle is duplicated.

hDuplicate :: Handle -> IO Handle
hDuplicate h@(FileHandle path m) = do
  new_h_ <- withHandle' "hDuplicate" h m (dupHandle h Nothing)
  newFileHandle path (handleFinalizer path) new_h_
hDuplicate h@(DuplexHandle path r w) = do
  new_w_ <- withHandle' "hDuplicate" h w (dupHandle h Nothing)
  new_w <- newMVar new_w_
  new_r_ <- withHandle' "hDuplicate" h r (dupHandle h (Just new_w))
  new_r <- newMVar new_r_
  addMVarFinalizer new_w (handleFinalizer path new_w)
  return (DuplexHandle path new_r new_w)

dupHandle :: Handle -> Maybe (MVar Handle__) -> Handle__
          -> IO (Handle__, Handle__)
dupHandle h other_side h_ = do
  -- flush the buffer first, so we don't have to copy its contents
  flushBuffer h_
  new_fd <- case other_side of
                Nothing -> throwErrnoIfMinus1 "dupHandle" $ c_dup (haFD h_)
                Just r -> withHandle_' "dupHandle" h r (return . haFD)
  dupHandle_ other_side h_ new_fd

dupHandleTo other_side hto_ h_ = do
  flushBuffer h_
  -- Windows' dup2 does not return the new descriptor, unlike Unix
  throwErrnoIfMinus1 "dupHandleTo" $ 
	c_dup2 (haFD h_) (haFD hto_)
  dupHandle_ other_side h_ (haFD hto_)

dupHandle_ :: Maybe (MVar Handle__) -> Handle__ -> FD
           -> IO (Handle__, Handle__)
dupHandle_ other_side h_ new_fd = do
  buffer <- allocateBuffer dEFAULT_BUFFER_SIZE (initBufferState (haType h_))
  ioref <- newIORef buffer
  ioref_buffers <- newIORef BufferListNil

  let new_handle_ = h_{ haFD = new_fd, 
	         	haBuffer = ioref, 
	          	haBuffers = ioref_buffers,
	          	haOtherSide = other_side }
  return (h_, new_handle_)

-- -----------------------------------------------------------------------------
-- Replacing a Handle

{- |
Makes the second handle a duplicate of the first handle.  The second 
handle will be closed first, if it is not already.

This can be used to retarget the standard Handles, for example:

> do h <- openFile "mystdout" WriteMode
>    hDuplicateTo h stdout
-}

hDuplicateTo :: Handle -> Handle -> IO ()
hDuplicateTo h1@(FileHandle _ m1) h2@(FileHandle _ m2)  = do
 withHandle__' "hDuplicateTo" h2 m2 $ \h2_ -> do
   _ <- hClose_help h2_
   withHandle' "hDuplicateTo" h1 m1 (dupHandleTo Nothing h2_)
hDuplicateTo h1@(DuplexHandle _ r1 w1) h2@(DuplexHandle _ r2 w2)  = do
 withHandle__' "hDuplicateTo" h2 w2  $ \w2_ -> do
   _ <- hClose_help w2_
   withHandle' "hDuplicateTo" h1 r1 (dupHandleTo Nothing w2_)
 withHandle__' "hDuplicateTo" h2 r2  $ \r2_ -> do
   _ <- hClose_help r2_
   withHandle' "hDuplicateTo" h1 r1 (dupHandleTo (Just w1) r2_)
hDuplicateTo h1 _ =
   ioException (IOError (Just h1) IllegalOperation "hDuplicateTo" 
		"handles are incompatible" Nothing)

-- ---------------------------------------------------------------------------
-- showing Handles.
--
-- | 'hShow' is in the 'IO' monad, and gives more comprehensive output
-- than the (pure) instance of 'Show' for 'Handle'.

hShow :: Handle -> IO String
hShow h@(FileHandle path _) = showHandle' path False h
hShow h@(DuplexHandle path _ _) = showHandle' path True h

showHandle' filepath is_duplex h = 
  withHandle_ "showHandle" h $ \hdl_ ->
    let
     showType | is_duplex = showString "duplex (read-write)"
	      | otherwise = shows (haType hdl_)
    in
    return 
      (( showChar '{' . 
        showHdl (haType hdl_) 
	    (showString "loc=" . showString filepath . showChar ',' .
	     showString "type=" . showType . showChar ',' .
	     showString "binary=" . shows (haIsBin hdl_) . showChar ',' .
	     showString "buffering=" . showBufMode (unsafePerformIO (readIORef (haBuffer hdl_))) (haBufferMode hdl_) . showString "}" )
      ) "")
   where

    showHdl :: HandleType -> ShowS -> ShowS
    showHdl ht cont = 
       case ht of
        ClosedHandle  -> shows ht . showString "}"
	_ -> cont

    showBufMode :: Buffer -> BufferMode -> ShowS
    showBufMode buf bmo =
      case bmo of
        NoBuffering   -> showString "none"
	LineBuffering -> showString "line"
	BlockBuffering (Just n) -> showString "block " . showParen True (shows n)
	BlockBuffering Nothing  -> showString "block " . showParen True (shows def)
      where
       def :: Int 
       def = bufSize buf

-- ---------------------------------------------------------------------------
-- debugging

#if defined(DEBUG_DUMP)
puts :: String -> IO ()
puts s = do write_rawBuffer 1 (unsafeCoerce# (packCString# s)) 0 (fromIntegral (length s))
	    return ()
#endif

-- -----------------------------------------------------------------------------
-- utils

throwErrnoIfMinus1RetryOnBlock  :: String -> IO CInt -> IO CInt -> IO CInt
throwErrnoIfMinus1RetryOnBlock loc f on_block  = 
  do
    res <- f
    if (res :: CInt) == -1
      then do
	err <- getErrno
	if err == eINTR
	  then throwErrnoIfMinus1RetryOnBlock loc f on_block
          else if err == eWOULDBLOCK || err == eAGAIN
	         then do on_block
                 else throwErrno loc
      else return res

-- -----------------------------------------------------------------------------
-- wrappers to platform-specific constants:

foreign import ccall unsafe "__hscore_supportsTextMode"
  tEXT_MODE_SEEK_ALLOWED :: Bool

foreign import ccall unsafe "__hscore_bufsiz"   dEFAULT_BUFFER_SIZE :: Int
foreign import ccall unsafe "__hscore_seek_cur" sEEK_CUR :: CInt
foreign import ccall unsafe "__hscore_seek_set" sEEK_SET :: CInt
foreign import ccall unsafe "__hscore_seek_end" sEEK_END :: CInt

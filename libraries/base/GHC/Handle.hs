{-# OPTIONS -fno-implicit-prelude -#include "HsBase.h" #-}

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

module GHC.Handle (
  withHandle, withHandle', withHandle_,
  wantWritableHandle, wantReadableHandle, wantSeekableHandle,
  
  newEmptyBuffer, allocateBuffer, readCharFromBuffer, writeCharIntoBuffer,
  flushWriteBufferOnly, flushWriteBuffer, flushReadBuffer, fillReadBuffer,
  read_off,  read_off_ba,
  write_off, write_off_ba, unlockFile,

  ioe_closedHandle, ioe_EOF, ioe_notReadable, ioe_notWritable,

  stdin, stdout, stderr,
  IOMode(..), IOModeEx(..), openFile, openFileEx, openFd, fdToHandle,
  hFileSize, hIsEOF, isEOF, hLookAhead, hSetBuffering, hSetBinaryMode,
  hFlush, hDuplicate, hDuplicateTo,

  hClose, hClose_help,

  HandlePosn(..), hGetPosn, hSetPosn,
  SeekMode(..), hSeek, hTell,

  hIsOpen, hIsClosed, hIsReadable, hIsWritable, hGetBuffering, hIsSeekable,
  hSetEcho, hGetEcho, hIsTerminalDevice,

#ifdef DEBUG_DUMP
  puts,
#endif

 ) where

import Control.Monad
import Data.Bits
import Data.Maybe
import Foreign
import Foreign.C
import System.IO.Error

import GHC.Posix
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

newFileHandle     :: (MVar Handle__ -> IO ()) -> Handle__ -> IO Handle
newFileHandle finalizer hc = do 
  m <- newMVar hc
  addMVarFinalizer m (finalizer m)
  return (FileHandle m)

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
withHandle fun h@(FileHandle m)     act = withHandle' fun h m act
withHandle fun h@(DuplexHandle m _) act = withHandle' fun h m act

withHandle' :: String -> Handle -> MVar Handle__
   -> (Handle__ -> IO (Handle__,a)) -> IO a
withHandle' fun h m act = 
   block $ do
   h_ <- takeMVar m
   checkBufferInvariants h_
   (h',v)  <- catch (act h_) 
		(\ ex -> putMVar m h_ >> ioError (augmentIOError ex fun h h_))
   checkBufferInvariants h'
   putMVar m h'
   return v

{-# INLINE withHandle_ #-}
withHandle_ :: String -> Handle -> (Handle__ -> IO a) -> IO a
withHandle_ fun h@(FileHandle m)     act = withHandle_' fun h m act
withHandle_ fun h@(DuplexHandle m _) act = withHandle_' fun h m act

withHandle_' fun h m act = 
   block $ do
   h_ <- takeMVar m
   checkBufferInvariants h_
   v  <- catch (act h_) 
	    (\ ex -> putMVar m h_ >> ioError (augmentIOError ex fun h h_))
   checkBufferInvariants h_
   putMVar m h_
   return v

withAllHandles__ :: String -> Handle -> (Handle__ -> IO Handle__) -> IO ()
withAllHandles__ fun h@(FileHandle m)     act = withHandle__' fun h m act
withAllHandles__ fun h@(DuplexHandle r w) act = do
  withHandle__' fun h r act
  withHandle__' fun h w act

withHandle__' fun h m act = 
   block $ do
   h_ <- takeMVar m
   checkBufferInvariants h_
   h'  <- catch (act h_)
	    (\ ex -> putMVar m h_ >> ioError (augmentIOError ex fun h h_))
   checkBufferInvariants h'
   putMVar m h'
   return ()

augmentIOError (IOError _ iot _ str fp) fun h h_
  = IOError (Just h) iot fun str filepath
  where filepath | Just _ <- fp = fp
		 | otherwise    = Just (haFilePath h_)

-- ---------------------------------------------------------------------------
-- Wrapper for write operations.

wantWritableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantWritableHandle fun h@(FileHandle m) act
  = wantWritableHandle' fun h m act
wantWritableHandle fun h@(DuplexHandle _ m) act
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
wantReadableHandle fun h@(FileHandle   m)   act
  = wantReadableHandle' fun h m act
wantReadableHandle fun h@(DuplexHandle m _) act
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
wantSeekableHandle fun h@(DuplexHandle _ _) _act =
  ioException (IOError (Just h) IllegalOperation fun 
		   "handle is not seekable" Nothing)
wantSeekableHandle fun h@(FileHandle m) act =
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

stdHandleFinalizer :: MVar Handle__ -> IO ()
stdHandleFinalizer m = do
  h_ <- takeMVar m
  flushWriteBufferOnly h_

handleFinalizer :: MVar Handle__ -> IO ()
handleFinalizer m = do
  h_ <- takeMVar m
  let
    -- hClose puts both the fd and the handle's type
    -- into a closed state, so it's a bit excessive
    -- to test for both here, but caution sometimes
    -- pays off..
   alreadyClosed = 
     case haType h_ of { ClosedHandle{} -> True; _ -> False }
   fd = fromIntegral (haFD h_)

  when (not alreadyClosed && fd /= -1) $ do
       flushWriteBufferOnly h_
       unlockFile fd
#ifdef mingw32_TARGET_OS
       (closeFd (haIsStream h_) fd >> return ())
#else
       (c_close fd >> return ())
#endif

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
  case newByteArray# size s of { (# s, b #) ->
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
   	 (c_lseek (fromIntegral fd) (fromIntegral off) sEEK_CUR)
     return buf{ bufWPtr=0, bufRPtr=0 }

flushWriteBuffer :: FD -> Bool -> Buffer -> IO Buffer
flushWriteBuffer fd is_stream buf@Buffer{ bufBuf=b, bufRPtr=r, bufWPtr=w }  = do
  let bytes = w - r
#ifdef DEBUG_DUMP
  puts ("flushWriteBuffer, fd=" ++ show fd ++ ", bytes=" ++ show bytes ++ "\n")
#endif
  if bytes == 0
     then return (buf{ bufRPtr=0, bufWPtr=0 })
     else do
  res <- throwErrnoIfMinus1RetryMayBlock "flushWriteBuffer"
		(write_off_ba (fromIntegral fd) is_stream b (fromIntegral r)
			      (fromIntegral bytes))
		(threadWaitWrite fd)
  let res' = fromIntegral res
  if res' < bytes 
     then flushWriteBuffer fd is_stream (buf{ bufRPtr = r + res' })
     else return buf{ bufRPtr=0, bufWPtr=0 }

foreign import ccall unsafe "__hscore_PrelHandle_write"
   write_off_ba :: CInt -> Bool -> RawBuffer -> Int -> CInt -> IO CInt

foreign import ccall unsafe "__hscore_PrelHandle_write"
   write_off :: CInt -> Bool -> Ptr CChar -> Int -> CInt -> IO CInt

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
  res <- throwErrnoIfMinus1RetryMayBlock "fillReadBuffer"
	    (read_off_ba fd is_stream b (fromIntegral w) (fromIntegral bytes))
	    (threadWaitRead fd)
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
 
foreign import ccall unsafe "__hscore_PrelHandle_read"
   read_off_ba :: FD -> Bool -> RawBuffer -> Int -> CInt -> IO CInt

foreign import ccall unsafe "__hscore_PrelHandle_read"
   read_off :: FD -> Bool -> Ptr CChar -> Int -> CInt -> IO CInt

-- ---------------------------------------------------------------------------
-- Standard Handles

-- Three handles are allocated during program initialisation.  The first
-- two manage input or output from the Haskell program's standard input
-- or output channel respectively.  The third manages output to the
-- standard error channel. These handles are initially open.

fd_stdin  = 0 :: FD
fd_stdout = 1 :: FD
fd_stderr = 2 :: FD

stdin :: Handle
stdin = unsafePerformIO $ do
   -- ToDo: acquire lock
   setNonBlockingFD fd_stdin
   (buf, bmode) <- getBuffer fd_stdin ReadBuffer
   mkStdHandle fd_stdin "<stdin>" ReadHandle buf bmode

stdout :: Handle
stdout = unsafePerformIO $ do
   -- ToDo: acquire lock
   -- We don't set non-blocking mode on stdout or sterr, because
   -- some shells don't recover properly.
   -- setNonBlockingFD fd_stdout
   (buf, bmode) <- getBuffer fd_stdout WriteBuffer
   mkStdHandle fd_stdout "<stdout>" WriteHandle buf bmode

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

{-
Computation `openFile file mode' allocates and returns a new, open
handle to manage the file `file'.  It manages input if `mode'
is `ReadMode', output if `mode' is `WriteMode' or `AppendMode',
and both input and output if mode is `ReadWriteMode'.

If the file does not exist and it is opened for output, it should be
created as a new file.  If `mode' is `WriteMode' and the file
already exists, then it should be truncated to zero length.  The
handle is positioned at the end of the file if `mode' is
`AppendMode', and otherwise at the beginning (in which case its
internal position is 0).

Implementations should enforce, locally to the Haskell process,
multiple-reader single-writer locking on files, which is to say that
there may either be many handles on the same file which manage input,
or just one handle on the file which manages output.  If any open or
semi-closed handle is managing a file for output, no new handle can be
allocated for that file.  If any open or semi-closed handle is
managing a file for input, new handles can only be allocated if they
do not manage output.

Two files are the same if they have the same absolute name.  An
implementation is free to impose stricter conditions.
-}

data IOModeEx 
 = BinaryMode IOMode
 | TextMode   IOMode
   deriving (Eq, Read, Show)

addFilePathToIOError fun fp (IOError h iot _ str _)
  = IOError h iot fun str (Just fp)

openFile :: FilePath -> IOMode -> IO Handle
openFile fp im = 
  catch 
    (openFile' fp (if   dEFAULT_OPEN_IN_BINARY_MODE 
                   then BinaryMode im
                   else TextMode im))
    (\e -> ioError (addFilePathToIOError "openFile" fp e))

openFileEx :: FilePath -> IOModeEx -> IO Handle
openFileEx fp m =
  catch
    (openFile' fp m)
    (\e -> ioError (addFilePathToIOError "openFileEx" fp e))


openFile' filepath ex_mode =
  withCString filepath $ \ f ->

    let 
      (mode, binary) =
      	case ex_mode of
           BinaryMode bmo -> (bmo, True)
	   TextMode   tmo -> (tmo, False)

      oflags1 = case mode of
	    	  ReadMode      -> read_flags  
	    	  WriteMode     -> write_flags 
	    	  ReadWriteMode -> rw_flags    
	    	  AppendMode    -> append_flags

      truncate | WriteMode <- mode = True
	       | otherwise	   = False

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
    fd <- fromIntegral `liftM`
	      throwErrnoIfMinus1Retry "openFile"
 	        (c_open f (fromIntegral oflags) 0o666)

    openFd fd Nothing filepath mode binary truncate
	-- ASSERT: if we just created the file, then openFd won't fail
	-- (so we don't need to worry about removing the newly created file
	--  in the event of an error).


std_flags    = o_NONBLOCK   .|. o_NOCTTY
output_flags = std_flags    .|. o_CREAT
read_flags   = std_flags    .|. o_RDONLY 
write_flags  = output_flags .|. o_WRONLY
rw_flags     = output_flags .|. o_RDWR
append_flags = write_flags  .|. o_APPEND

-- ---------------------------------------------------------------------------
-- openFd

openFd :: FD -> Maybe FDType -> FilePath -> IOMode -> Bool -> Bool -> IO Handle
openFd fd mb_fd_type filepath mode binary truncate = do
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
    let is_stream = fd_type == Stream
    case fd_type of
	Directory -> 
	   ioException (IOError Nothing InappropriateType "openFile"
			   "is a directory" Nothing) 

	Stream
	   | ReadWriteHandle <- ha_type -> mkDuplexHandle fd is_stream filepath binary
	   | otherwise                  -> mkFileHandle fd is_stream filepath ha_type binary

	-- regular files need to be locked
	RegularFile -> do
	   r <- lockFile (fromIntegral fd) (fromBool write) 1{-exclusive-}
	   when (r == -1)  $
		ioException (IOError Nothing ResourceBusy "openFile"
				   "file is locked" Nothing)

	   -- truncate the file if necessary
	   when truncate (fileTruncate filepath)

	   mkFileHandle fd is_stream filepath ha_type binary


fdToHandle :: FD -> IO Handle
fdToHandle fd = do
   mode <- fdGetMode fd
   let fd_str = "<file descriptor: " ++ show fd ++ ">"
   openFd fd Nothing fd_str mode True{-bin mode-} False{-no truncate-}

foreign import ccall unsafe "lockFile"
  lockFile :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "unlockFile"
  unlockFile :: CInt -> IO CInt

mkStdHandle :: FD -> FilePath -> HandleType -> IORef Buffer -> BufferMode
	-> IO Handle
mkStdHandle fd filepath ha_type buf bmode = do
   spares <- newIORef BufferListNil
   newFileHandle stdHandleFinalizer
	    (Handle__ { haFD = fd,
			haType = ha_type,
                        haIsBin = dEFAULT_OPEN_IN_BINARY_MODE,
			haIsStream = False,
			haBufferMode = bmode,
			haFilePath = filepath,
			haBuffer = buf,
			haBuffers = spares,
			haOtherSide = Nothing
		      })

mkFileHandle :: FD -> Bool -> FilePath -> HandleType -> Bool -> IO Handle
mkFileHandle fd is_stream filepath ha_type binary = do
  (buf, bmode) <- getBuffer fd (initBufferState ha_type)
  spares <- newIORef BufferListNil
  newFileHandle handleFinalizer
	    (Handle__ { haFD = fd,
			haType = ha_type,
                        haIsBin = binary,
			haIsStream = is_stream,
			haBufferMode = bmode,
			haFilePath = filepath,
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
			haFilePath = filepath,
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
			haFilePath = filepath,
			haBuffer = r_buf,
			haBuffers = r_spares,
			haOtherSide = Just write_side
		      }
  read_side <- newMVar r_handle_

  addMVarFinalizer read_side (handleFinalizer read_side)
  return (DuplexHandle read_side write_side)
   

initBufferState ReadHandle = ReadBuffer
initBufferState _ 	   = WriteBuffer

-- ---------------------------------------------------------------------------
-- Closing a handle

-- Computation `hClose hdl' makes handle `hdl' closed.  Before the
-- computation finishes, any items buffered for output and not already
-- sent to the operating system are flushed as for `hFlush'.

-- For a duplex handle, we close&flush the write side, and just close
-- the read side.

hClose :: Handle -> IO ()
hClose h@(FileHandle m)     = hClose' h m
hClose h@(DuplexHandle r w) = hClose' h w >> hClose' h r

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
      _ -> do
	  let fd = haFD handle_
	      c_fd = fromIntegral fd

	  flushWriteBufferOnly handle_

	  -- close the file descriptor, but not when this is the read
	  -- side of a duplex handle, and not when this is one of the
	  -- std file handles.
	  case haOtherSide handle_ of
	    Nothing -> 
		when (fd /= fd_stdin && fd /= fd_stdout && fd /= fd_stderr) $
			throwErrnoIfMinus1Retry_ "hClose" 
#ifdef mingw32_TARGET_OS
	    			(closeFd (haIsStream handle_) c_fd)
#else
	    			(c_close c_fd)
#endif
	    Just _  -> return ()

	  -- free the spare buffers
	  writeIORef (haBuffers handle_) BufferListNil

	  -- unlock it
	  unlockFile c_fd

	  -- we must set the fd to -1, because the finalizer is going
	  -- to run eventually and try to close/unlock it.
	  return (handle_{ haFD        = -1, 
		    	   haType      = ClosedHandle
			 })

-----------------------------------------------------------------------------
-- Detecting the size of a file

-- For a handle `hdl' which attached to a physical file, `hFileSize
-- hdl' returns the size of `hdl' in terms of the number of items
-- which can be read from `hdl'.

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

-- ---------------------------------------------------------------------------
-- Detecting the End of Input

-- For a readable handle `hdl', `hIsEOF hdl' returns
-- `True' if no further input can be taken from `hdl' or for a
-- physical file, if the current I/O position is equal to the length of
-- the file.  Otherwise, it returns `False'.

hIsEOF :: Handle -> IO Bool
hIsEOF handle =
  catch
     (do hLookAhead handle; return False)
     (\e -> if isEOFError e then return True else ioError e)

isEOF :: IO Bool
isEOF = hIsEOF stdin

-- ---------------------------------------------------------------------------
-- Looking ahead

-- hLookahead returns the next character from the handle without
-- removing it from the input buffer, blocking until a character is
-- available.

hLookAhead :: Handle -> IO Char
hLookAhead handle = do
  wantReadableHandle "hLookAhead"  handle $ \handle_ -> do
  let ref     = haBuffer handle_
      fd      = haFD handle_
      is_line = haBufferMode handle_ == LineBuffering
  buf <- readIORef ref

  -- fill up the read buffer if necessary
  new_buf <- if bufferEmpty buf
		then fillReadBuffer fd is_line (haIsStream handle_) buf
		else return buf
  
  writeIORef ref new_buf

  (c,_) <- readCharFromBuffer (bufBuf buf) (bufRPtr buf)
  return c

-- ---------------------------------------------------------------------------
-- Buffering Operations

-- Three kinds of buffering are supported: line-buffering,
-- block-buffering or no-buffering.  See GHC.IOBase for definition and
-- further explanation of what the type represent.

-- Computation `hSetBuffering hdl mode' sets the mode of buffering for
-- handle hdl on subsequent reads and writes.
--
--   * If mode is LineBuffering, line-buffering should be enabled if possible.
--
--   * If mode is `BlockBuffering size', then block-buffering
--     should be enabled if possible.  The size of the buffer is n items
--     if size is `Just n' and is otherwise implementation-dependent.
--
--   * If mode is NoBuffering, then buffering is disabled if possible.

-- If the buffer mode is changed from BlockBuffering or
-- LineBuffering to NoBuffering, then any items in the output
-- buffer are written to the device, and any items in the input buffer
-- are discarded.  The default buffering mode when a handle is opened
-- is implementation-dependent and may depend on the object which is
-- attached to that handle.

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
		  NoBuffering -> setCooked (haFD handle_) False
		  _           -> setCooked (haFD handle_) True

 	  -- throw away spare buffers, they might be the wrong size
	  writeIORef (haBuffers handle_) BufferListNil

	  return (handle_{ haBufferMode = mode })

-- -----------------------------------------------------------------------------
-- hFlush

-- The action `hFlush hdl' causes any items buffered for output
-- in handle `hdl' to be sent immediately to the operating
-- system.

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

-- Computation `hGetPosn hdl' returns the current I/O position of
-- `hdl' as an abstract position.  Computation `hSetPosn p' sets the
-- position of `hdl' to a previously obtained position `p'.

hGetPosn :: Handle -> IO HandlePosn
hGetPosn handle = do
    posn <- hTell handle
    return (HandlePosn handle posn)

hSetPosn :: HandlePosn -> IO () 
hSetPosn (HandlePosn h i) = hSeek h AbsoluteSeek i

-- ---------------------------------------------------------------------------
-- hSeek

{-
The action `hSeek hdl mode i' sets the position of handle
`hdl' depending on `mode'.  If `mode' is

 * AbsoluteSeek - The position of `hdl' is set to `i'.
 * RelativeSeek - The position of `hdl' is set to offset `i' from
                  the current position.
 * SeekFromEnd  - The position of `hdl' is set to offset `i' from
                  the end of the file.

Some handles may not be seekable (see `hIsSeekable'), or only
support a subset of the possible positioning operations (e.g. it may
only be possible to seek to the end of a tape, or to a positive
offset from the beginning or current position).

It is not possible to set a negative I/O position, or for a physical
file, an I/O position beyond the current end-of-file. 

Note: 
 - when seeking using `SeekFromEnd', positive offsets (>=0) means
   seeking at or past EOF.

 - we possibly deviate from the report on the issue of seeking within
   the buffer and whether to flush it or not.  The report isn't exactly
   clear here.
-}

data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                    deriving (Eq, Ord, Ix, Enum, Read, Show)

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
	    (c_lseek (fromIntegral (haFD handle_)) (fromIntegral offset) whence)

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

#if defined(mingw32_TARGET_OS)
	-- urgh, on Windows we have to worry about \n -> \r\n translation, 
	-- so we can't easily calculate the file position using the
	-- current buffer size.  Just flush instead.
      flushBuffer handle_
#endif
      let fd = fromIntegral (haFD handle_)
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
hIsReadable (DuplexHandle _ _) = return True
hIsReadable handle =
    withHandle_ "hIsReadable" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle 	   -> ioe_closedHandle
      SemiClosedHandle 	   -> ioe_closedHandle
      htype 		   -> return (isReadableHandleType htype)

hIsWritable :: Handle -> IO Bool
hIsWritable (DuplexHandle _ _) = return False
hIsWritable handle =
    withHandle_ "hIsWritable" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle 	   -> ioe_closedHandle
      SemiClosedHandle 	   -> ioe_closedHandle
      htype 		   -> return (isWritableHandleType htype)

-- Querying how a handle buffers its data:

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
                                 return (t == RegularFile
                                         && (haIsBin handle_ 
						|| tEXT_MODE_SEEK_ALLOWED))

-- -----------------------------------------------------------------------------
-- Changing echo status

-- Non-standard GHC extension is to allow the echoing status
-- of a handles connected to terminals to be reconfigured:

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

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice handle = do
    withHandle_ "hIsTerminalDevice" handle $ \ handle_ -> do
     case haType handle_ of 
       ClosedHandle -> ioe_closedHandle
       _            -> fdIsTTY (haFD handle_)

-- -----------------------------------------------------------------------------
-- hSetBinaryMode

-- | On Windows, reading a file in text mode (which is the default) will
-- translate CRLF to LF, and writing will translate LF to CRLF. This
-- is usually what you want with text files. With binary files this is
-- undesirable; also, as usual under Microsoft operating systems, text
-- mode treats control-Z as EOF.  Setting binary mode using
-- 'hSetBinaryMode' turns off all special treatment of end-of-line and
-- end-of-file characters.
--
hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode handle bin =
  withAllHandles__ "hSetBinaryMode" handle $ \ handle_ ->
    do throwErrnoIfMinus1_ "hSetBinaryMode"
          (setmode (fromIntegral (haFD handle_)) bin)
       return handle_{haIsBin=bin}
  
foreign import ccall unsafe "__hscore_setmode"
  setmode :: CInt -> Bool -> IO CInt

-- -----------------------------------------------------------------------------
-- Duplicating a Handle

-- |Returns a duplicate of the original handle, with its own buffer
-- and file pointer.  The original handle's buffer is flushed, including
-- discarding any input data, before the handle is duplicated.

hDuplicate :: Handle -> IO Handle
hDuplicate h@(FileHandle m) = do
  new_h_ <- withHandle' "hDuplicate" h m (dupHandle_ Nothing)
  new_m <- newMVar new_h_
  return (FileHandle new_m)
hDuplicate h@(DuplexHandle r w) = do
  new_w_ <- withHandle' "hDuplicate" h w (dupHandle_ Nothing)
  new_w <- newMVar new_w_
  new_r_ <- withHandle' "hDuplicate" h r (dupHandle_ (Just new_w))
  new_r <- newMVar new_r_
  return (DuplexHandle new_r new_w)

dupHandle_ other_side h_ = do
  -- flush the buffer first, so we don't have to copy its contents
  flushBuffer h_
  new_fd <- c_dup (fromIntegral (haFD h_))
  buffer <- allocateBuffer dEFAULT_BUFFER_SIZE (initBufferState (haType h_))
  ioref <- newIORef buffer
  ioref_buffers <- newIORef BufferListNil

  let new_handle_ = h_{ haFD = fromIntegral new_fd, 
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
hDuplicateTo h1@(FileHandle m1) h2@(FileHandle m2)  = do
 withHandle__' "hDuplicateTo" h2 m2 $ \h2_ -> do
   _ <- hClose_help h2_
   withHandle' "hDuplicateTo" h1 m1 (dupHandle_ Nothing)
hDuplicateTo h1@(DuplexHandle r1 w1) h2@(DuplexHandle r2 w2)  = do
 withHandle__' "hDuplicateTo" h2 w2  $ \w2_ -> do
   _ <- hClose_help w2_
   withHandle' "hDuplicateTo" h1 r1 (dupHandle_ Nothing)
 withHandle__' "hDuplicateTo" h2 r2  $ \r2_ -> do
   _ <- hClose_help r2_
   withHandle' "hDuplicateTo" h1 r1 (dupHandle_ (Just w1))
hDuplicateTo h1 _ =
   ioException (IOError (Just h1) IllegalOperation "hDuplicateTo" 
		"handles are incompatible" Nothing)

-- ---------------------------------------------------------------------------
-- debugging

#ifdef DEBUG_DUMP
puts :: String -> IO ()
puts s = withCString s $ \cstr -> do write_off_ba 1 False cstr 0 (fromIntegral (length s))
				     return ()
#endif

-- -----------------------------------------------------------------------------
-- wrappers to platform-specific constants:

foreign import ccall unsafe "__hscore_supportsTextMode"
  tEXT_MODE_SEEK_ALLOWED :: Bool

foreign import ccall unsafe "__hscore_bufsiz"   dEFAULT_BUFFER_SIZE :: Int
foreign import ccall unsafe "__hscore_seek_cur" sEEK_CUR :: CInt
foreign import ccall unsafe "__hscore_seek_set" sEEK_SET :: CInt
foreign import ccall unsafe "__hscore_seek_end" sEEK_END :: CInt

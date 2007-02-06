{-# OPTIONS_GHC -fno-implicit-prelude -#include "HsBase.h" #-}

#undef DEBUG_DUMP

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO
-- Copyright   :  (c) The University of Glasgow, 1992-2001
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- String I\/O functions
--
-----------------------------------------------------------------------------

-- #hide
module GHC.IO ( 
   hWaitForInput, hGetChar, hGetLine, hGetContents, hPutChar, hPutStr,
   commitBuffer',	-- hack, see below
   hGetcBuffered,	-- needed by ghc/compiler/utils/StringBuffer.lhs
   hGetBuf, hGetBufNonBlocking, hPutBuf, hPutBufNonBlocking, slurpFile,
   memcpy_ba_baoff,
   memcpy_ptr_baoff,
   memcpy_baoff_ba,
   memcpy_baoff_ptr,
 ) where

import Foreign
import Foreign.C

import System.IO.Error
import Data.Maybe
import Control.Monad
import System.Posix.Internals

import GHC.Enum
import GHC.Base
import GHC.IOBase
import GHC.Handle	-- much of the real stuff is in here
import GHC.Real
import GHC.Num
import GHC.Show
import GHC.List
import GHC.Exception    ( ioError, catch )

#ifdef mingw32_HOST_OS
import GHC.Conc
#endif

-- ---------------------------------------------------------------------------
-- Simple input operations

-- If hWaitForInput finds anything in the Handle's buffer, it
-- immediately returns.  If not, it tries to read from the underlying
-- OS handle. Notice that for buffered Handles connected to terminals
-- this means waiting until a complete line is available.

-- | Computation 'hWaitForInput' @hdl t@
-- waits until input is available on handle @hdl@.
-- It returns 'True' as soon as input is available on @hdl@,
-- or 'False' if no input is available within @t@ milliseconds.
--
-- If @t@ is less than zero, then @hWaitForInput@ waits indefinitely.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.
--
-- NOTE for GHC users: unless you use the @-threaded@ flag,
-- @hWaitForInput t@ where @t >= 0@ will block all other Haskell
-- threads for the duration of the call.  It behaves like a
-- @safe@ foreign call in this respect.

hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput h msecs = do
  wantReadableHandle "hWaitForInput" h $ \ handle_ -> do
  let ref = haBuffer handle_
  buf <- readIORef ref

  if not (bufferEmpty buf)
	then return True
	else do

  if msecs < 0 
	then do buf' <- fillReadBuffer (haFD handle_) True 
				(haIsStream handle_) buf
	        writeIORef ref buf'
		return True
	else do r <- throwErrnoIfMinus1Retry "hWaitForInput" $
	  		inputReady (fromIntegral (haFD handle_)) 
			   (fromIntegral msecs) (haIsStream handle_)
		return (r /= 0)

foreign import ccall safe "inputReady"
  inputReady :: CInt -> CInt -> Bool -> IO CInt

-- ---------------------------------------------------------------------------
-- hGetChar

-- | Computation 'hGetChar' @hdl@ reads a character from the file or
-- channel managed by @hdl@, blocking until a character is available.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.

hGetChar :: Handle -> IO Char
hGetChar handle =
  wantReadableHandle "hGetChar" handle $ \handle_ -> do

  let fd = haFD handle_
      ref = haBuffer handle_

  buf <- readIORef ref
  if not (bufferEmpty buf)
	then hGetcBuffered fd ref buf
	else do

  -- buffer is empty.
  case haBufferMode handle_ of
    LineBuffering    -> do
	new_buf <- fillReadBuffer fd True (haIsStream handle_) buf
	hGetcBuffered fd ref new_buf
    BlockBuffering _ -> do
	new_buf <- fillReadBuffer fd True (haIsStream handle_) buf
		--		     ^^^^
		-- don't wait for a completely full buffer.
	hGetcBuffered fd ref new_buf
    NoBuffering -> do
	-- make use of the minimal buffer we already have
	let raw = bufBuf buf
	r <- readRawBuffer "hGetChar" (fromIntegral fd) (haIsStream handle_) raw 0 1
	if r == 0
	   then ioe_EOF
	   else do (c,_) <- readCharFromBuffer raw 0
		   return c

hGetcBuffered fd ref buf@Buffer{ bufBuf=b, bufRPtr=r, bufWPtr=w }
 = do (c,r) <- readCharFromBuffer b r
      let new_buf | r == w    = buf{ bufRPtr=0, bufWPtr=0 }
	          | otherwise = buf{ bufRPtr=r }
      writeIORef ref new_buf
      return c

-- ---------------------------------------------------------------------------
-- hGetLine

-- ToDo: the unbuffered case is wrong: it doesn't lock the handle for
-- the duration.

-- | Computation 'hGetLine' @hdl@ reads a line from the file or
-- channel managed by @hdl@.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file is encountered when reading
--    the /first/ character of the line.
--
-- If 'hGetLine' encounters end-of-file at any other point while reading
-- in a line, it is treated as a line terminator and the (partial)
-- line is returned.

hGetLine :: Handle -> IO String
hGetLine h = do
  m <- wantReadableHandle "hGetLine" h $ \ handle_ -> do
    	case haBufferMode handle_ of
    	   NoBuffering      -> return Nothing
    	   LineBuffering    -> do
    	      l <- hGetLineBuffered handle_
    	      return (Just l)
    	   BlockBuffering _ -> do 
    	      l <- hGetLineBuffered handle_
    	      return (Just l)
  case m of
	Nothing -> hGetLineUnBuffered h
	Just l  -> return l

hGetLineBuffered :: Handle__ -> IO String
hGetLineBuffered handle_ = do
  let ref = haBuffer handle_
  buf <- readIORef ref
  hGetLineBufferedLoop handle_ ref buf []

hGetLineBufferedLoop :: Handle__ -> IORef Buffer -> Buffer -> [String]
                     -> IO String
hGetLineBufferedLoop handle_ ref 
	buf@Buffer{ bufRPtr=r, bufWPtr=w, bufBuf=raw } xss =
  let 
	-- find the end-of-line character, if there is one
	loop raw r
	   | r == w = return (False, w)
	   | otherwise =  do
		(c,r') <- readCharFromBuffer raw r
		if c == '\n' 
		   then return (True, r) -- NB. not r': don't include the '\n'
		   else loop raw r'
  in do
  (eol, off) <- loop raw r

#ifdef DEBUG_DUMP
  puts ("hGetLineBufferedLoop: r=" ++ show r ++ ", w=" ++ show w ++ ", off=" ++ show off ++ "\n")
#endif

  xs <- unpack raw r off

  -- if eol == True, then off is the offset of the '\n'
  -- otherwise off == w and the buffer is now empty.
  if eol
	then do if (w == off + 1)
	    		then writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
		   	else writeIORef ref buf{ bufRPtr = off + 1 }
	        return (concat (reverse (xs:xss)))
	else do
	     maybe_buf <- maybeFillReadBuffer (haFD handle_) True (haIsStream handle_)
				buf{ bufWPtr=0, bufRPtr=0 }
	     case maybe_buf of
		-- Nothing indicates we caught an EOF, and we may have a
		-- partial line to return.
		Nothing -> do
		     writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
		     let str = concat (reverse (xs:xss))
		     if not (null str)
		        then return str
		        else ioe_EOF
		Just new_buf -> 
		     hGetLineBufferedLoop handle_ ref new_buf (xs:xss)


maybeFillReadBuffer fd is_line is_stream buf
  = catch 
     (do buf <- fillReadBuffer fd is_line is_stream buf
	 return (Just buf)
     )
     (\e -> do if isEOFError e 
		  then return Nothing 
		  else ioError e)


unpack :: RawBuffer -> Int -> Int -> IO [Char]
unpack buf r 0   = return ""
unpack buf (I# r) (I# len) = IO $ \s -> unpack [] (len -# 1#) s
   where
    unpack acc i s
     | i <# r  = (# s, acc #)
     | otherwise = 
          case readCharArray# buf i s of
	    (# s, ch #) -> unpack (C# ch : acc) (i -# 1#) s


hGetLineUnBuffered :: Handle -> IO String
hGetLineUnBuffered h = do
  c <- hGetChar h
  if c == '\n' then
     return ""
   else do
    l <- getRest
    return (c:l)
 where
  getRest = do
    c <- 
      catch 
        (hGetChar h)
        (\ err -> do
          if isEOFError err then
	     return '\n'
	   else
	     ioError err)
    if c == '\n' then
       return ""
     else do
       s <- getRest
       return (c:s)

-- -----------------------------------------------------------------------------
-- hGetContents

-- hGetContents on a DuplexHandle only affects the read side: you can
-- carry on writing to it afterwards.

-- | Computation 'hGetContents' @hdl@ returns the list of characters
-- corresponding to the unread portion of the channel or file managed
-- by @hdl@, which is put into an intermediate state, /semi-closed/.
-- In this state, @hdl@ is effectively closed,
-- but items are read from @hdl@ on demand and accumulated in a special
-- list returned by 'hGetContents' @hdl@.
--
-- Any operation that fails because a handle is closed,
-- also fails if a handle is semi-closed.  The only exception is 'hClose'.
-- A semi-closed handle becomes closed:
--
--  * if 'hClose' is applied to it;
--
--  * if an I\/O error occurs when reading an item from the handle;
--
--  * or once the entire contents of the handle has been read.
--
-- Once a semi-closed handle becomes closed, the contents of the
-- associated list becomes fixed.  The contents of this final list is
-- only partially specified: it will contain at least all the items of
-- the stream that were evaluated prior to the handle becoming closed.
--
-- Any I\/O errors encountered while a handle is semi-closed are simply
-- discarded.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.

hGetContents :: Handle -> IO String
hGetContents handle = 
    withHandle "hGetContents" handle $ \handle_ ->
    case haType handle_ of 
      ClosedHandle 	   -> ioe_closedHandle
      SemiClosedHandle 	   -> ioe_closedHandle
      AppendHandle 	   -> ioe_notReadable
      WriteHandle 	   -> ioe_notReadable
      _ -> do xs <- lazyRead handle
	      return (handle_{ haType=SemiClosedHandle}, xs )

-- Note that someone may close the semi-closed handle (or change its
-- buffering), so each time these lazy read functions are pulled on,
-- they have to check whether the handle has indeed been closed.

lazyRead :: Handle -> IO String
lazyRead handle = 
   unsafeInterleaveIO $
	withHandle "lazyRead" handle $ \ handle_ -> do
	case haType handle_ of
	  ClosedHandle     -> return (handle_, "")
	  SemiClosedHandle -> lazyRead' handle handle_
	  _ -> ioException 
	 	  (IOError (Just handle) IllegalOperation "lazyRead"
			"illegal handle type" Nothing)

lazyRead' h handle_ = do
  let ref = haBuffer handle_
      fd  = haFD handle_

  -- even a NoBuffering handle can have a char in the buffer... 
  -- (see hLookAhead)
  buf <- readIORef ref
  if not (bufferEmpty buf)
	then lazyReadHaveBuffer h handle_ fd ref buf
	else do

  case haBufferMode handle_ of
     NoBuffering      -> do
	-- make use of the minimal buffer we already have
	let raw = bufBuf buf
	r <- readRawBuffer "lazyRead" (fromIntegral fd) (haIsStream handle_) raw 0 1
	if r == 0
	   then do handle_ <- hClose_help handle_ 
		   return (handle_, "")
	   else do (c,_) <- readCharFromBuffer raw 0
		   rest <- lazyRead h
		   return (handle_, c : rest)

     LineBuffering    -> lazyReadBuffered h handle_ fd ref buf
     BlockBuffering _ -> lazyReadBuffered h handle_ fd ref buf

-- we never want to block during the read, so we call fillReadBuffer with
-- is_line==True, which tells it to "just read what there is".
lazyReadBuffered h handle_ fd ref buf = do
   catch 
   	(do buf <- fillReadBuffer fd True{-is_line-} (haIsStream handle_) buf
	    lazyReadHaveBuffer h handle_ fd ref buf
     	)
	-- all I/O errors are discarded.  Additionally, we close the handle.
     	(\e -> do handle_ <- hClose_help handle_
		  return (handle_, "")
	)

lazyReadHaveBuffer h handle_ fd ref buf = do
   more <- lazyRead h
   writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
   s <- unpackAcc (bufBuf buf) (bufRPtr buf) (bufWPtr buf) more
   return (handle_, s)


unpackAcc :: RawBuffer -> Int -> Int -> [Char] -> IO [Char]
unpackAcc buf r 0 acc  = return acc
unpackAcc buf (I# r) (I# len) acc = IO $ \s -> unpack acc (len -# 1#) s
   where
    unpack acc i s
     | i <# r  = (# s, acc #)
     | otherwise = 
          case readCharArray# buf i s of
	    (# s, ch #) -> unpack (C# ch : acc) (i -# 1#) s

-- ---------------------------------------------------------------------------
-- hPutChar

-- | Computation 'hPutChar' @hdl ch@ writes the character @ch@ to the
-- file or channel managed by @hdl@.  Characters may be buffered if
-- buffering is enabled for @hdl@.
--
-- This operation may fail with:
--
--  * 'isFullError' if the device is full; or
--
--  * 'isPermissionError' if another system resource limit would be exceeded.

hPutChar :: Handle -> Char -> IO ()
hPutChar handle c = do
    c `seq` return ()
    wantWritableHandle "hPutChar" handle $ \ handle_  -> do
    let fd = haFD handle_
    case haBufferMode handle_ of
	LineBuffering    -> hPutcBuffered handle_ True  c
	BlockBuffering _ -> hPutcBuffered handle_ False c
	NoBuffering      ->
		with (castCharToCChar c) $ \buf -> do
  		  writeRawBufferPtr "hPutChar" (fromIntegral fd) (haIsStream handle_) buf 0 1
		  return ()

hPutcBuffered handle_ is_line c = do
  let ref = haBuffer handle_
  buf <- readIORef ref
  let w = bufWPtr buf
  w'  <- writeCharIntoBuffer (bufBuf buf) w c
  let new_buf = buf{ bufWPtr = w' }
  if bufferFull new_buf || is_line && c == '\n'
     then do 
  	flushed_buf <- flushWriteBuffer (haFD handle_) (haIsStream handle_) new_buf
  	writeIORef ref flushed_buf
     else do 
  	writeIORef ref new_buf


hPutChars :: Handle -> [Char] -> IO ()
hPutChars handle [] = return ()
hPutChars handle (c:cs) = hPutChar handle c >> hPutChars handle cs

-- ---------------------------------------------------------------------------
-- hPutStr

-- We go to some trouble to avoid keeping the handle locked while we're
-- evaluating the string argument to hPutStr, in case doing so triggers another
-- I/O operation on the same handle which would lead to deadlock.  The classic
-- case is
--
--		putStr (trace "hello" "world")
--
-- so the basic scheme is this:
--
--	* copy the string into a fresh buffer,
--	* "commit" the buffer to the handle.
--
-- Committing may involve simply copying the contents of the new
-- buffer into the handle's buffer, flushing one or both buffers, or
-- maybe just swapping the buffers over (if the handle's buffer was
-- empty).  See commitBuffer below.

-- | Computation 'hPutStr' @hdl s@ writes the string
-- @s@ to the file or channel managed by @hdl@.
--
-- This operation may fail with:
--
--  * 'isFullError' if the device is full; or
--
--  * 'isPermissionError' if another system resource limit would be exceeded.

hPutStr :: Handle -> String -> IO ()
hPutStr handle str = do
    buffer_mode <- wantWritableHandle "hPutStr" handle 
			(\ handle_ -> do getSpareBuffer handle_)
    case buffer_mode of
       (NoBuffering, _) -> do
	    hPutChars handle str	-- v. slow, but we don't care
       (LineBuffering, buf) -> do
	    writeLines handle buf str
       (BlockBuffering _, buf) -> do
            writeBlocks handle buf str


getSpareBuffer :: Handle__ -> IO (BufferMode, Buffer)
getSpareBuffer Handle__{haBuffer=ref, 
			haBuffers=spare_ref,
			haBufferMode=mode}
 = do
   case mode of
     NoBuffering -> return (mode, error "no buffer!")
     _ -> do
          bufs <- readIORef spare_ref
	  buf  <- readIORef ref
	  case bufs of
 	    BufferListCons b rest -> do
		writeIORef spare_ref rest
		return ( mode, newEmptyBuffer b WriteBuffer (bufSize buf))
	    BufferListNil -> do
		new_buf <- allocateBuffer (bufSize buf) WriteBuffer
		return (mode, new_buf)


writeLines :: Handle -> Buffer -> String -> IO ()
writeLines hdl Buffer{ bufBuf=raw, bufSize=len } s =
  let
   shoveString :: Int -> [Char] -> IO ()
	-- check n == len first, to ensure that shoveString is strict in n.
   shoveString n cs | n == len = do
	new_buf <- commitBuffer hdl raw len n True{-needs flush-} False
	writeLines hdl new_buf cs
   shoveString n [] = do
	commitBuffer hdl raw len n False{-no flush-} True{-release-}
	return ()
   shoveString n (c:cs) = do
	n' <- writeCharIntoBuffer raw n c
        if (c == '\n') 
         then do 
              new_buf <- commitBuffer hdl raw len n' True{-needs flush-} False
              writeLines hdl new_buf cs
         else 
              shoveString n' cs
  in
  shoveString 0 s

writeBlocks :: Handle -> Buffer -> String -> IO ()
writeBlocks hdl Buffer{ bufBuf=raw, bufSize=len } s =
  let
   shoveString :: Int -> [Char] -> IO ()
	-- check n == len first, to ensure that shoveString is strict in n.
   shoveString n cs | n == len = do
	new_buf <- commitBuffer hdl raw len n True{-needs flush-} False
	writeBlocks hdl new_buf cs
   shoveString n [] = do
	commitBuffer hdl raw len n False{-no flush-} True{-release-}
	return ()
   shoveString n (c:cs) = do
	n' <- writeCharIntoBuffer raw n c
	shoveString n' cs
  in
  shoveString 0 s

-- -----------------------------------------------------------------------------
-- commitBuffer handle buf sz count flush release
-- 
-- Write the contents of the buffer 'buf' ('sz' bytes long, containing
-- 'count' bytes of data) to handle (handle must be block or line buffered).
-- 
-- Implementation:
-- 
--    for block/line buffering,
-- 	 1. If there isn't room in the handle buffer, flush the handle
-- 	    buffer.
-- 
-- 	 2. If the handle buffer is empty,
-- 		 if flush, 
-- 		     then write buf directly to the device.
-- 		     else swap the handle buffer with buf.
-- 
-- 	 3. If the handle buffer is non-empty, copy buf into the
-- 	    handle buffer.  Then, if flush != 0, flush
-- 	    the buffer.

commitBuffer
	:: Handle			-- handle to commit to
	-> RawBuffer -> Int		-- address and size (in bytes) of buffer
	-> Int				-- number of bytes of data in buffer
	-> Bool				-- True <=> flush the handle afterward
	-> Bool 			-- release the buffer?
	-> IO Buffer

commitBuffer hdl raw sz@(I# _) count@(I# _) flush release = do
  wantWritableHandle "commitAndReleaseBuffer" hdl $
     commitBuffer' raw sz count flush release

-- Explicitly lambda-lift this function to subvert GHC's full laziness
-- optimisations, which otherwise tends to float out subexpressions
-- past the \handle, which is really a pessimisation in this case because
-- that lambda is a one-shot lambda.
--
-- Don't forget to export the function, to stop it being inlined too
-- (this appears to be better than NOINLINE, because the strictness
-- analyser still gets to worker-wrapper it).
--
-- This hack is a fairly big win for hPutStr performance.  --SDM 18/9/2001
--
commitBuffer' raw sz@(I# _) count@(I# _) flush release
  handle_@Handle__{ haFD=fd, haBuffer=ref, haBuffers=spare_buf_ref } = do

#ifdef DEBUG_DUMP
      puts ("commitBuffer: sz=" ++ show sz ++ ", count=" ++ show count
	    ++ ", flush=" ++ show flush ++ ", release=" ++ show release ++"\n")
#endif

      old_buf@Buffer{ bufBuf=old_raw, bufRPtr=r, bufWPtr=w, bufSize=size }
	  <- readIORef ref

      buf_ret <-
        -- enough room in handle buffer?
	 if (not flush && (size - w > count))
		-- The > is to be sure that we never exactly fill
		-- up the buffer, which would require a flush.  So
		-- if copying the new data into the buffer would
		-- make the buffer full, we just flush the existing
		-- buffer and the new data immediately, rather than
		-- copying before flushing.

		-- not flushing, and there's enough room in the buffer:
		-- just copy the data in and update bufWPtr.
	    then do memcpy_baoff_ba old_raw w raw (fromIntegral count)
		    writeIORef ref old_buf{ bufWPtr = w + count }
		    return (newEmptyBuffer raw WriteBuffer sz)

		-- else, we have to flush
	    else do flushed_buf <- flushWriteBuffer fd (haIsStream handle_) old_buf

		    let this_buf = 
			    Buffer{ bufBuf=raw, bufState=WriteBuffer, 
				    bufRPtr=0, bufWPtr=count, bufSize=sz }

			-- if:  (a) we don't have to flush, and
			--      (b) size(new buffer) == size(old buffer), and
			--      (c) new buffer is not full,
			-- we can just just swap them over...
		    if (not flush && sz == size && count /= sz)
			then do 
			  writeIORef ref this_buf
			  return flushed_buf			     

			-- otherwise, we have to flush the new data too,
			-- and start with a fresh buffer
			else do
			  flushWriteBuffer fd (haIsStream handle_) this_buf
			  writeIORef ref flushed_buf
			    -- if the sizes were different, then allocate
			    -- a new buffer of the correct size.
			  if sz == size
			     then return (newEmptyBuffer raw WriteBuffer sz)
			     else allocateBuffer size WriteBuffer

      -- release the buffer if necessary
      case buf_ret of
        Buffer{ bufSize=buf_ret_sz, bufBuf=buf_ret_raw } -> do
          if release && buf_ret_sz == size
	    then do
	      spare_bufs <- readIORef spare_buf_ref
	      writeIORef spare_buf_ref 
		(BufferListCons buf_ret_raw spare_bufs)
	      return buf_ret
	    else
	      return buf_ret

-- ---------------------------------------------------------------------------
-- Reading/writing sequences of bytes.

-- ---------------------------------------------------------------------------
-- hPutBuf

-- | 'hPutBuf' @hdl buf count@ writes @count@ 8-bit bytes from the
-- buffer @buf@ to the handle @hdl@.  It returns ().
--
-- This operation may fail with:
--
--  * 'ResourceVanished' if the handle is a pipe or socket, and the
--    reading end is closed.  (If this is a POSIX system, and the program
--    has not asked to ignore SIGPIPE, then a SIGPIPE may be delivered
--    instead, whose default action is to terminate the program).

hPutBuf :: Handle			-- handle to write to
	-> Ptr a			-- address of buffer
	-> Int				-- number of bytes of data in buffer
	-> IO ()
hPutBuf h ptr count = do hPutBuf' h ptr count True; return ()

hPutBufNonBlocking
	:: Handle			-- handle to write to
	-> Ptr a			-- address of buffer
	-> Int				-- number of bytes of data in buffer
	-> IO Int			-- returns: number of bytes written
hPutBufNonBlocking h ptr count = hPutBuf' h ptr count False

hPutBuf':: Handle			-- handle to write to
	-> Ptr a			-- address of buffer
	-> Int				-- number of bytes of data in buffer
	-> Bool				-- allow blocking?
	-> IO Int
hPutBuf' handle ptr count can_block
  | count == 0 = return 0
  | count <  0 = illegalBufferSize handle "hPutBuf" count
  | otherwise = 
    wantWritableHandle "hPutBuf" handle $ 
      \ handle_@Handle__{ haFD=fd, haBuffer=ref, haIsStream=is_stream } -> 
	  bufWrite fd ref is_stream ptr count can_block

bufWrite fd ref is_stream ptr count can_block =
  seq count $ seq fd $ do  -- strictness hack
  old_buf@Buffer{ bufBuf=old_raw, bufRPtr=r, bufWPtr=w, bufSize=size }
     <- readIORef ref

  -- enough room in handle buffer?
  if (size - w > count)
	-- There's enough room in the buffer:
	-- just copy the data in and update bufWPtr.
	then do memcpy_baoff_ptr old_raw w ptr (fromIntegral count)
	        writeIORef ref old_buf{ bufWPtr = w + count }
		return count

	-- else, we have to flush
	else do flushed_buf <- flushWriteBuffer fd is_stream old_buf
			-- TODO: we should do a non-blocking flush here
		writeIORef ref flushed_buf
		-- if we can fit in the buffer, then just loop	
		if count < size
		   then bufWrite fd ref is_stream ptr count can_block
		   else if can_block
		   	   then do writeChunk fd is_stream (castPtr ptr) count
			           return count
		   	   else writeChunkNonBlocking fd is_stream ptr count

writeChunk :: FD -> Bool -> Ptr CChar -> Int -> IO ()
writeChunk fd is_stream ptr bytes = loop 0 bytes 
 where
  loop :: Int -> Int -> IO ()
  loop _   bytes | bytes <= 0 = return ()
  loop off bytes = do
    r <- fromIntegral `liftM`
    	   writeRawBufferPtr "writeChunk" (fromIntegral fd) is_stream ptr
	   		     off (fromIntegral bytes)
    -- write can't return 0
    loop (off + r) (bytes - r)

writeChunkNonBlocking :: FD -> Bool -> Ptr a -> Int -> IO Int
writeChunkNonBlocking fd is_stream ptr bytes = loop 0 bytes 
 where
  loop :: Int -> Int -> IO Int
  loop off bytes | bytes <= 0 = return off
  loop off bytes = do
#ifndef mingw32_HOST_OS
    ssize <- c_write (fromIntegral fd) (ptr `plusPtr` off) (fromIntegral bytes)
    let r = fromIntegral ssize :: Int
    if (r == -1)
      then do errno <- getErrno
	      if (errno == eAGAIN || errno == eWOULDBLOCK)
		 then return off
		 else throwErrno "writeChunk"
      else loop (off + r) (bytes - r)
#else
    (ssize, rc) <- asyncWrite fd (fromIntegral $ fromEnum is_stream)
    				 (fromIntegral bytes)
				 (ptr `plusPtr` off)
    let r = fromIntegral ssize :: Int
    if r == (-1)
      then ioError (errnoToIOError "hPutBufNonBlocking" (Errno (fromIntegral rc)) Nothing Nothing)
      else loop (off + r) (bytes - r)
#endif

-- ---------------------------------------------------------------------------
-- hGetBuf

-- | 'hGetBuf' @hdl buf count@ reads data from the handle @hdl@
-- into the buffer @buf@ until either EOF is reached or
-- @count@ 8-bit bytes have been read.
-- It returns the number of bytes actually read.  This may be zero if
-- EOF was reached before any data was read (or if @count@ is zero).
--
-- 'hGetBuf' never raises an EOF exception, instead it returns a value
-- smaller than @count@.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGetBuf' will behave as if EOF was reached.

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf h ptr count
  | count == 0 = return 0
  | count <  0 = illegalBufferSize h "hGetBuf" count
  | otherwise = 
      wantReadableHandle "hGetBuf" h $ 
	\ handle_@Handle__{ haFD=fd, haBuffer=ref, haIsStream=is_stream } -> do
	    bufRead fd ref is_stream ptr 0 count

-- small reads go through the buffer, large reads are satisfied by
-- taking data first from the buffer and then direct from the file
-- descriptor.
bufRead fd ref is_stream ptr so_far count =
  seq fd $ seq so_far $ seq count $ do -- strictness hack
  buf@Buffer{ bufBuf=raw, bufWPtr=w, bufRPtr=r, bufSize=sz } <- readIORef ref
  if bufferEmpty buf
     then if count > sz  -- small read?
		then do rest <- readChunk fd is_stream ptr count
			return (so_far + rest)
	 	else do mb_buf <- maybeFillReadBuffer fd True is_stream buf
			case mb_buf of
		          Nothing -> return so_far -- got nothing, we're done
		          Just buf' -> do
			   	writeIORef ref buf'
				bufRead fd ref is_stream ptr so_far count
     else do 
  	let avail = w - r
	if (count == avail)
	   then do 
		memcpy_ptr_baoff ptr raw r (fromIntegral count)
		writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
		return (so_far + count)
	   else do
	if (count < avail)
	   then do 
		memcpy_ptr_baoff ptr raw r (fromIntegral count)
		writeIORef ref buf{ bufRPtr = r + count }
		return (so_far + count)
	   else do
  
	memcpy_ptr_baoff ptr raw r (fromIntegral avail)
	writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
	let remaining = count - avail
	    so_far' = so_far + avail
	    ptr' = ptr `plusPtr` avail

	if remaining < sz
	   then bufRead fd ref is_stream ptr' so_far' remaining
	   else do 

	rest <- readChunk fd is_stream ptr' remaining
	return (so_far' + rest)

readChunk :: FD -> Bool -> Ptr a -> Int -> IO Int
readChunk fd is_stream ptr bytes = loop 0 bytes 
 where
  loop :: Int -> Int -> IO Int
  loop off bytes | bytes <= 0 = return off
  loop off bytes = do
    r <- fromIntegral `liftM`
           readRawBufferPtr "readChunk" (fromIntegral fd) is_stream 
	   		    (castPtr ptr) off (fromIntegral bytes)
    if r == 0
	then return off
	else loop (off + r) (bytes - r)


-- | 'hGetBufNonBlocking' @hdl buf count@ reads data from the handle @hdl@
-- into the buffer @buf@ until either EOF is reached, or
-- @count@ 8-bit bytes have been read, or there is no more data available
-- to read immediately.
--
-- 'hGetBufNonBlocking' is identical to 'hGetBuf', except that it will
-- never block waiting for data to become available, instead it returns
-- only whatever data is available.  To wait for data to arrive before
-- calling 'hGetBufNonBlocking', use 'hWaitForInput'.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGetBufNonBlocking' will behave as if EOF was reached.
--
hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking h ptr count
  | count == 0 = return 0
  | count <  0 = illegalBufferSize h "hGetBufNonBlocking" count
  | otherwise = 
      wantReadableHandle "hGetBufNonBlocking" h $ 
	\ handle_@Handle__{ haFD=fd, haBuffer=ref, haIsStream=is_stream } -> do
	    bufReadNonBlocking fd ref is_stream ptr 0 count

bufReadNonBlocking fd ref is_stream ptr so_far count =
  seq fd $ seq so_far $ seq count $ do -- strictness hack
  buf@Buffer{ bufBuf=raw, bufWPtr=w, bufRPtr=r, bufSize=sz } <- readIORef ref
  if bufferEmpty buf
     then if count > sz  -- large read?
		then do rest <- readChunkNonBlocking fd is_stream ptr count
			return (so_far + rest)
	 	else do buf' <- fillReadBufferWithoutBlocking fd is_stream buf
			case buf' of { Buffer{ bufWPtr=w }  ->
			if (w == 0) 
			   then return so_far
			   else do writeIORef ref buf'
				   bufReadNonBlocking fd ref is_stream ptr
					 so_far (min count w)
				  -- NOTE: new count is 'min count w'
				  -- so we will just copy the contents of the
				  -- buffer in the recursive call, and not
				  -- loop again.
			}
     else do
  	let avail = w - r
	if (count == avail)
	   then do 
		memcpy_ptr_baoff ptr raw r (fromIntegral count)
		writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
		return (so_far + count)
	   else do
	if (count < avail)
	   then do 
		memcpy_ptr_baoff ptr raw r (fromIntegral count)
		writeIORef ref buf{ bufRPtr = r + count }
		return (so_far + count)
	   else do

	memcpy_ptr_baoff ptr raw r (fromIntegral avail)
	writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
	let remaining = count - avail
	    so_far' = so_far + avail
	    ptr' = ptr `plusPtr` avail

	-- we haven't attempted to read anything yet if we get to here.
	if remaining < sz
	   then bufReadNonBlocking fd ref is_stream ptr' so_far' remaining
	   else do 

	rest <- readChunkNonBlocking fd is_stream ptr' remaining
	return (so_far' + rest)


readChunkNonBlocking :: FD -> Bool -> Ptr a -> Int -> IO Int
readChunkNonBlocking fd is_stream ptr bytes = do
#ifndef mingw32_HOST_OS
    ssize <- c_read (fromIntegral fd) (castPtr ptr) (fromIntegral bytes)
    let r = fromIntegral ssize :: Int
    if (r == -1)
      then do errno <- getErrno
	      if (errno == eAGAIN || errno == eWOULDBLOCK)
		 then return 0
		 else throwErrno "readChunk"
      else return r
#else
    fromIntegral `liftM`
        readRawBufferPtr "readChunkNonBlocking" (fromIntegral fd) is_stream 
	   		    (castPtr ptr) 0 (fromIntegral bytes)

    -- we don't have non-blocking read support on Windows, so just invoke
    -- the ordinary low-level read which will block until data is available,
    -- but won't wait for the whole buffer to fill.
#endif

slurpFile :: FilePath -> IO (Ptr (), Int)
slurpFile fname = do
  handle <- openFile fname ReadMode
  sz     <- hFileSize handle
  if sz > fromIntegral (maxBound::Int) then 
    ioError (userError "slurpFile: file too big")
   else do
    let sz_i = fromIntegral sz
    if sz_i == 0 then return (nullPtr, 0) else do
    chunk <- mallocBytes sz_i
    r <- hGetBuf handle chunk sz_i
    hClose handle
    return (chunk, r)

-- ---------------------------------------------------------------------------
-- memcpy wrappers

foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ba_baoff :: RawBuffer -> RawBuffer -> Int -> CSize -> IO (Ptr ())
foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ptr_baoff :: Ptr a -> RawBuffer -> Int -> CSize -> IO (Ptr ())
foreign import ccall unsafe "__hscore_memcpy_dst_off"
   memcpy_baoff_ba :: RawBuffer -> Int -> RawBuffer -> CSize -> IO (Ptr ())
foreign import ccall unsafe "__hscore_memcpy_dst_off"
   memcpy_baoff_ptr :: RawBuffer -> Int -> Ptr a -> CSize -> IO (Ptr ())

-----------------------------------------------------------------------------
-- Internal Utils

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn (sz :: Int) = 
	ioException (IOError (Just handle)
			    InvalidArgument  fn
			    ("illegal buffer size " ++ showsPrec 9 sz [])
			    Nothing)

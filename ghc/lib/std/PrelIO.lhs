%
% (c) The GRAP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelIO]{Module @PrelIO@}

This module defines all basic IO operations.
These are needed for the IO operations exported by Prelude,
but as it happens they also do everything required by library
module IO.


\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/stgio.h" #-}

module PrelIO where

import PrelBase

import PrelIOBase
import PrelHandle	-- much of the real stuff is in here

import PrelNum
import PrelRead         ( readParen, Read(..), reads, lex, readIO )
import PrelShow
import PrelMaybe	( Either(..), Maybe(..) )
import PrelAddr		( Addr(..), AddrOff(..), nullAddr, plusAddr )
import PrelList		( concat, reverse, null )
import PrelByteArr	( ByteArray )
import PrelPack		( unpackNBytesST, unpackNBytesAccST )
import PrelException    ( ioError, catch, catchException, throw, 
			  blockAsyncExceptions )
import PrelConc
\end{code}


%*********************************************************
%*							 *
\subsection{Standard IO}
%*							 *
%*********************************************************

The Prelude has from Day 1 provided a collection of common
IO functions. We define these here, but let the Prelude
export them.

\begin{code}
putChar         :: Char -> IO ()
putChar c       =  hPutChar stdout c

putStr          :: String -> IO ()
putStr s        =  hPutStr stdout s

putStrLn        :: String -> IO ()
putStrLn s      =  do putStr s
                      putChar '\n'

print           :: Show a => a -> IO ()
print x         =  putStrLn (show x)

getChar         :: IO Char
getChar         =  hGetChar stdin

getLine         :: IO String
getLine         =  hGetLine stdin
            
getContents     :: IO String
getContents     =  hGetContents stdin

interact        ::  (String -> String) -> IO ()
interact f      =   do s <- getContents
                       putStr (f s)

readFile        :: FilePath -> IO String
readFile name	=  openFile name ReadMode >>= hGetContents

writeFile       :: FilePath -> String -> IO ()
writeFile name str = do
    hdl <- openFile name WriteMode
    hPutStr hdl str
    hClose hdl

appendFile      :: FilePath -> String -> IO ()
appendFile name str = do
    hdl <- openFile name AppendMode
    hPutStr hdl str
    hClose hdl

readLn          :: Read a => IO a
readLn          =  do l <- getLine
                      r <- readIO l
                      return r
\end{code}


%*********************************************************
%*							*
\subsection{Simple input operations}
%*							*
%*********************************************************

Computation @hReady hdl@ indicates whether at least
one item is available for input from handle {\em hdl}.

@hWaitForInput@ is the generalisation, wait for \tr{n} milliseconds
before deciding whether the Handle has run dry or not.

If @hWaitForInput@ finds anything in the Handle's buffer, it immediately returns.
If not, it tries to read from the underlying OS handle. Notice that
for buffered Handles connected to terminals this means waiting until a complete
line is available.

\begin{code}
hReady :: Handle -> IO Bool
hReady h = hWaitForInput h 0

hWaitForInput :: Handle -> Int -> IO Bool 
hWaitForInput handle msecs =
    wantReadableHandle "hWaitForInput" handle $ \ handle_ -> do
    rc       <- inputReady (haFO__ handle_) (msecs::Int)     -- ConcHask: SAFE, won't block
    case (rc::Int) of
      0 -> return False
      1 -> return True
      _ -> constructErrorAndFail "hWaitForInput"
\end{code}

@hGetChar hdl@ reads the next character from handle @hdl@,
blocking until a character is available.

\begin{code}
hGetChar :: Handle -> IO Char
hGetChar handle = do
  c <- mayBlockRead "hGetChar" handle fileGetc
  return (chr c)

{-
  If EOF is reached before EOL is encountered, ignore the
  EOF and return the partial line. Next attempt at calling
  hGetLine on the handle will yield an EOF IO exception though.
-}

hGetLine :: Handle -> IO String
hGetLine h = do
    buffer_mode <- wantWriteableHandle_ "hGetLine" h
			(\ handle_ -> do getBuffer handle_)
    case buffer_mode of
       (NoBuffering, _, _)          -> hGetLineUnBuffered h
       (LineBuffering, buf, bsz)    -> hGetLineBuf' []
       (BlockBuffering _, buf, bsz) -> hGetLineBuf' []

  where hGetLineBuf' xss = do
	   (eol, xss) <- catch 
	    ( do
	      mayBlockRead' "hGetLine" h 
	        (\fo -> readLine fo)
	        (\fo bytes -> do
	    	  buf <- getBufStart fo bytes
	    	  eol <- readCharOffAddr buf (bytes-1)
		  xs <- if (eol == '\n') 
			  then stToIO (unpackNBytesST buf (bytes-1))
 	        	  else stToIO (unpackNBytesST buf bytes)
	          return (eol, xs:xss)
	       )
            )
            (\e -> if isEOFError e && not (null xss)
			then return ('\n', xss)
			else ioError e)
		
	   if (eol == '\n')
		then return (concat (reverse xss))
        	else hGetLineBuf' xss


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


readCharOffAddr (A# a) (I# i)
  = IO $ \s -> case readCharOffAddr# a i s of { (# s,x #) -> (# s, C# x #) }
\end{code}

@hLookahead hdl@ returns the next character from handle @hdl@
without removing it from the input buffer, blocking until a
character is available.

\begin{code}
hLookAhead :: Handle -> IO Char
hLookAhead handle = do
  rc <- mayBlockRead "hLookAhead" handle fileLookAhead
  return (chr rc)
\end{code}


%*********************************************************
%*							*
\subsection{Getting the entire contents of a handle}
%*							*
%*********************************************************

@hGetContents hdl@ returns the list of characters corresponding
to the unread portion of the channel or file managed by @hdl@,
which is made semi-closed.

\begin{code}
hGetContents :: Handle -> IO String
hGetContents handle = 
	-- can't use wantReadableHandle here, because we want to side effect
	-- the handle.
    withHandle handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle "hGetContents" handle
      SemiClosedHandle 	   -> ioe_closedHandle "hGetContents" handle
      AppendHandle 	   -> ioError not_readable_error
      WriteHandle 	   -> ioError not_readable_error
      _ -> do
    	  {- 
    	    To avoid introducing an extra layer of buffering here,
    	    we provide three lazy read methods, based on character,
    	    line, and block buffering.
    	  -}
   	let handle_' = handle_{ haType__ = SemiClosedHandle }
    	case (haBufferMode__ handle_) of
    	 LineBuffering    -> do
    	    str <- unsafeInterleaveIO (lazyReadLine handle (haFO__ handle_))
    	    return (handle_', str)
    	 BlockBuffering _ -> do
    	    str <- unsafeInterleaveIO (lazyReadBlock handle (haFO__ handle_))
    	    return (handle_', str)
    	 NoBuffering      -> do
    	    str <- unsafeInterleaveIO (lazyReadChar handle (haFO__ handle_))
    	    return (handle_', str)
  where
   not_readable_error = 
	   IOError (Just handle) IllegalOperation "hGetContents"
		   ("handle is not open for reading")
\end{code}

Note that someone may close the semi-closed handle (or change its buffering), 
so each these lazy read functions are pulled on, they have to check whether
the handle has indeed been closed.

\begin{code}
#ifndef __PARALLEL_HASKELL__
lazyReadBlock :: Handle -> ForeignObj -> IO String
lazyReadLine  :: Handle -> ForeignObj -> IO String
lazyReadChar  :: Handle -> ForeignObj -> IO String
#else
lazyReadBlock :: Handle -> Addr -> IO String
lazyReadLine  :: Handle -> Addr -> IO String
lazyReadChar  :: Handle -> Addr -> IO String
#endif

lazyReadBlock handle fo = do
   buf   <- getBufStart fo 0
   bytes <- mayBlock fo (readBlock fo) -- ConcHask: UNSAFE, may block.
   case (bytes::Int) of
     -3 -> -- buffering has been turned off, use lazyReadChar instead
           lazyReadChar handle fo
     -2 -> return ""
     -1 -> -- an error occurred, close the handle
	  withHandle handle $ \ handle_ -> do
          closeFile (haFO__ handle_) 0{-don't bother flushing-}  -- ConcHask: SAFE, won't block.
	  return (handle_ { haType__    = ClosedHandle,
			    haFO__      = nullFile__ }, 
		  "")
     _ -> do
      more <- unsafeInterleaveIO (lazyReadBlock handle fo)
      stToIO (unpackNBytesAccST buf bytes more)

lazyReadLine handle fo = do
     bytes <- mayBlock fo (readLine fo)   -- ConcHask: UNSAFE, may block.
     case (bytes::Int) of
       -3 -> -- buffering has been turned off, use lazyReadChar instead
             lazyReadChar handle fo
       -2 -> return "" -- handle closed by someone else, stop reading.
       -1 -> -- an error occurred, close the handle
  	     withHandle handle $ \ handle_ -> do
             closeFile (haFO__ handle_) 0{- don't bother flushing-}  -- ConcHask: SAFE, won't block
	     return (handle_ { haType__    = ClosedHandle,
			       haFO__      = nullFile__ },
		     "")
       _ -> do
          more <- unsafeInterleaveIO (lazyReadLine handle fo)
          buf  <- getBufStart fo bytes  -- ConcHask: won't block
	  stToIO (unpackNBytesAccST buf bytes more)

lazyReadChar handle fo = do
    char <- mayBlock fo (readChar fo)   -- ConcHask: UNSAFE, may block.
    case (char::Int) of
      -4 -> -- buffering is now block-buffered, use lazyReadBlock instead
	    lazyReadBlock handle fo
	    
      -3 -> -- buffering is now line-buffered, use lazyReadLine instead
	    lazyReadLine handle fo
      -2 -> return ""
      -1 -> -- error, silently close handle.
 	 withHandle handle $ \ handle_ -> do
         closeFile (haFO__ handle_) 0{-don't bother flusing-}  -- ConcHask: SAFE, won't block
	 return (handle_{ haType__  = ClosedHandle,
			  haFO__    = nullFile__ },
		 "")
      _ -> do
	 more <- unsafeInterleaveIO (lazyReadChar handle fo)
         return (chr char : more)

\end{code}


%*********************************************************
%*							*
\subsection{Simple output functions}
%*							*
%*********************************************************

@hPutChar hdl ch@ writes the character @ch@ to the file
or channel managed by @hdl@.  Characters may be buffered if
buffering is enabled for @hdl@

\begin{code}
hPutChar :: Handle -> Char -> IO ()
hPutChar handle c = 
    c `seq` do   -- must evaluate c before grabbing the handle lock
    wantWriteableHandle "hPutChar" handle $ \ handle_  -> do
    let fo = haFO__ handle_
    flushConnectedBuf fo
    rc <- mayBlock fo (filePutc fo c)   -- ConcHask: UNSAFE, may block.
    if rc == 0
     then return ()
     else constructErrorAndFail "hPutChar"

hPutChars :: Handle -> [Char] -> IO ()
hPutChars handle [] = return ()
hPutChars handle (c:cs) = hPutChar handle c >> hPutChars handle cs
\end{code}

@hPutStr hdl s@ writes the string @s@ to the file or
channel managed by @hdl@, buffering the output if needs be.


\begin{code}
hPutStr :: Handle -> String -> IO ()
hPutStr handle str = do
    buffer_mode <- wantWriteableHandle_ "hPutStr" handle 
			(\ handle_ -> do getBuffer handle_)
    case buffer_mode of
       (NoBuffering, _, _) -> do
	    hPutChars handle str	-- v. slow, but we don't care
       (LineBuffering, buf, bsz) -> do
	    writeLines handle buf bsz str
       (BlockBuffering _, buf, bsz) -> do
            writeBlocks handle buf bsz str
	-- ToDo: async exceptions during writeLines & writeBlocks will cause
	-- the buffer to get lost in the void.  Using ByteArrays instead of
	-- malloced buffers is one way around this, but we really ought to
	-- be able to handle it with exception handlers/block/unblock etc.

getBuffer :: Handle__ -> IO (Handle__, (BufferMode, Addr, Int))
getBuffer handle_ = do
   let bufs = haBuffers__ handle_
       fo   = haFO__ handle_
       mode = haBufferMode__ handle_	
   sz <- getBufSize fo
   case mode of
	NoBuffering -> return (handle_, (mode, nullAddr, 0))
	_ -> case bufs of
		[] -> do  buf <- allocMemory__ sz
			  return (handle_, (mode, buf, sz))
		(b:bs) -> return (handle_{ haBuffers__ = bs }, (mode, b, sz))

freeBuffer :: Handle__ -> Addr -> Int -> IO Handle__
freeBuffer handle_ buf sz = do
   fo_sz <- getBufSize (haFO__ handle_)
   if (sz /= fo_sz) 
	then do { free buf; return handle_ }
	else do { return handle_{ haBuffers__ = buf : haBuffers__ handle_ } }

swapBuffers :: Handle__ -> Addr -> Int -> IO Handle__
swapBuffers handle_ buf sz = do
   let fo = haFO__ handle_
   fo_buf <- getBuf fo
   setBuf fo buf sz
   return (handle_{ haBuffers__ = fo_buf : haBuffers__ handle_ })

-------------------------------------------------------------------------------
-- commitAndReleaseBuffer handle buf sz count flush
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

commitAndReleaseBuffer
	:: Handle			-- handle to commit to
	-> Addr -> Int			-- address and size (in bytes) of buffer
	-> Int				-- number of bytes of data in buffer
	-> Bool				-- flush the handle afterward?
	-> IO ()

commitAndReleaseBuffer hdl@(Handle h) buf sz count flush = do
      h_ <- takeMVar h

	-- First deal with any possible exceptions, by freeing the buffer.
	-- Async exceptions are blocked, but there are still some interruptible
	-- ops below.

	-- note that commit doesn't *always* free the buffer, it might
	-- swap it for the current handle buffer instead.  This makes things
	-- a whole lot more complicated, because we can't just do 
	-- "finally (... free buffer ...)" here.
      catchException (commit hdl h_) 
		     (\e -> do { h_ <- freeBuffer h_ buf sz; putMVar h h_ })

  where
   commit hdl@(Handle h) handle_ = 
     checkWriteableHandle "commitAndReleaseBuffer" hdl handle_ $ do
      let fo = haFO__ handle_
      flushConnectedBuf fo		-- ????  -SDM
      getWriteableBuf fo		-- flush read buf if necessary
      fo_buf     <- getBuf fo
      fo_wptr    <- getBufWPtr fo
      fo_bufSize <- getBufSize fo

      let ok    h_ = putMVar h h_ >> return ()

	  -- enough room in handle buffer for the new data?
      if (flush || fo_bufSize - fo_wptr <= count)

	  -- The <= is to be sure that we never exactly fill up the
	  -- buffer, which would require a flush.  So if copying the
	  -- new data into the buffer would make the buffer full, we
	  -- just flush the existing buffer and the new data immediately,
	  -- rather than copying before flushing.

	    then do rc <- mayBlock fo (flushFile fo)
		    if (rc < 0) 
			then constructErrorAndFail "commitAndReleaseBuffer"
			else
		     if (flush || sz /= fo_bufSize || count == sz)
			then do rc <- write_buf fo buf count
		    		if (rc < 0)
				    then constructErrorAndFail "commitAndReleaseBuffer"
			      	    else do handle_ <- freeBuffer handle_ buf sz
					    ok handle_

			-- if:  (a) we don't have to flush, and
			--      (b) size(new buffer) == size(old buffer), and
			--      (c) new buffer is not full,
			-- we can just just swap them over...
			else do handle_ <- swapBuffers handle_ buf sz
				setBufWPtr fo count
				ok handle_

		-- not flushing, and there's enough room in the buffer:
		-- just copy the data in and update bufWPtr.
	    else do memcpy (plusAddr fo_buf (AddrOff# fo_wptr)) buf count
		    setBufWPtr fo (fo_wptr + count)
		    handle_ <- freeBuffer handle_ buf sz
		    ok handle_

--------------------------------------------------------------------------------
-- commitBuffer handle buf sz count flush
-- 
-- Flushes 'count' bytes from the buffer 'buf' (size 'sz') to 'handle'.
-- There are several cases to consider altogether:
-- 
-- If flush, 
-- 	   - flush handle buffer,
-- 	   - write out new buffer directly
-- 
-- else
-- 	   - if there's enough room in the handle buffer, 
--	       then copy new buf into it
-- 	       else flush handle buffer, then copy new buffer into it
--
-- Make sure that we maintain the invariant that the handle buffer is never
-- left in a full state.  Several functions rely on this (eg. filePutc), so
-- if we're about to exactly fill the buffer then we make sure we do a flush
-- here (also see above in commitAndReleaseBuffer).

commitBuffer
	:: Handle			-- handle to commit to
	-> Addr -> Int			-- address and size (in bytes) of buffer
	-> Int				-- number of bytes of data in buffer
	-> Bool				-- flush the handle afterward?
	-> IO ()

commitBuffer handle buf sz count flush = do
    wantWriteableHandle "commitBuffer" handle $ \handle_ -> do
      let fo = haFO__ handle_
      flushConnectedBuf fo		-- ????  -SDM
      getWriteableBuf fo		-- flush read buf if necessary
      fo_buf     <- getBuf fo
      fo_wptr    <- getBufWPtr fo
      fo_bufSize <- getBufSize fo

      new_wptr <-                       -- not enough room in handle buffer?
	(if flush || (fo_bufSize - fo_wptr <= count)
	    then do rc <- mayBlock fo (flushFile fo)
		    if (rc < 0) then constructErrorAndFail "commitBuffer"
				else return 0
	    else return fo_wptr )

      if (flush || fo_bufSize <= count)  -- committed buffer too large?

	    then do rc <- write_buf fo buf count
		    if (rc < 0) then constructErrorAndFail "commitBuffer"
			        else return ()

	    else do memcpy (plusAddr fo_buf (AddrOff# new_wptr)) buf count
		    setBufWPtr fo (new_wptr + count)
		    return ()

write_buf fo buf 0 = return 0
write_buf fo buf count = do
  rc <- mayBlock fo (write_ fo buf count)
  if (rc > 0)
	then  write_buf fo buf (count - rc) -- partial write
	else  return rc

-- a version of commitBuffer that will free the buffer if an exception is 
-- received.  DON'T use this if you intend to use the buffer again!
checkedCommitBuffer handle buf sz count flush 
  = catchException (commitBuffer handle buf sz count flush) 
		   (\e -> do withHandle__ handle (\h_ -> freeBuffer h_ buf sz)
			     throw e)

foreign import "memcpy" unsafe memcpy :: Addr -> Addr -> Int -> IO ()
\end{code}

Going across the border between Haskell and C is relatively costly,
so for block writes we pack the character strings on the Haskell-side
before passing the external write routine a pointer to the buffer.

\begin{code}
#ifdef __HUGS__

#ifdef __CONCURRENT_HASKELL__
/* See comment in shoveString below for explanation */
#warning delayed update of buffer disnae work with killThread
#endif

writeLines :: Handle -> Addr -> Int -> String -> IO ()
writeLines handle buf bufLen s =
  let
   shoveString :: Int -> [Char] -> IO ()
   shoveString n ls = 
     case ls of
      [] -> commitAndReleaseBuffer handle buf buflen n False{-no need to flush-}

      (x:xs) -> do
        primWriteCharOffAddr buf n x
          {- Flushing on buffer exhaustion or newlines (even if it isn't the last one) -}
	let next_n = n + 1
	if next_n == bufLen || x == '\n'
	 then do
	   checkedCommitBuffer hdl buf len next_n True{-needs flush-} 
	   shoveString 0 xs
         else
	   shoveString next_n xs
  in
  shoveString 0 s

#else /* ndef __HUGS__ */

writeLines :: Handle -> Addr -> Int -> String -> IO ()
writeLines hdl buf len@(I# bufLen) s =
  let
   shoveString :: Int# -> [Char] -> IO ()
   shoveString n ls = 
     case ls of
      [] -> commitAndReleaseBuffer hdl buf len (I# n) False{-no need to flush-}

      ((C# x):xs) -> do
        write_char buf n x
          -- Flushing on buffer exhaustion or newlines 
	  -- (even if it isn't the last one)
	let next_n = n +# 1#
	if next_n ==# bufLen || x `eqChar#` '\n'#
	 then do
	   checkedCommitBuffer hdl buf len (I# next_n) True{-needs flush-} 
	   shoveString 0# xs
         else
	   shoveString next_n xs
  in
  shoveString 0# s
#endif /* ndef __HUGS__ */

#ifdef __HUGS__
writeBlocks :: Handle -> Addr -> Int -> String -> IO ()
writeBlocks hdl buf bufLen s =
  let
   shoveString :: Int -> [Char] -> IO ()
   shoveString n ls = 
     case ls of
      [] -> commitAndReleaseBuffer hdl buf len n False{-no need to flush-} 

      (x:xs) -> do
        primWriteCharOffAddr buf n x
	let next_n = n + 1
	if next_n == bufLen
	 then do
	   checkedCommitBuffer hdl buf len next_n True{-needs flush-}
	   shoveString 0 xs
         else
	   shoveString next_n xs
  in
  shoveString 0 s

#else /* ndef __HUGS__ */

writeBlocks :: Handle -> Addr -> Int -> String -> IO ()
writeBlocks hdl buf len@(I# bufLen) s =
  let
   shoveString :: Int# -> [Char] -> IO ()
   shoveString n ls = 
     case ls of
      [] -> commitAndReleaseBuffer hdl buf len (I# n) False{-no need to flush-} 

      ((C# x):xs) -> do
        write_char buf n x
	let next_n = n +# 1#
	if next_n ==# bufLen
	 then do
	   checkedCommitBuffer hdl buf len (I# next_n) True{-needs flush-}
	   shoveString 0# xs
         else
	   shoveString next_n xs
  in
  shoveString 0# s

write_char :: Addr -> Int# -> Char# -> IO ()
write_char (A# buf#) n# c# =
   IO $ \ s# ->
   case (writeCharOffAddr# buf# n# c# s#) of s2# -> (# s2#, () #)
#endif /* ndef __HUGS__ */
\end{code}

Computation @hPrint hdl t@ writes the string representation of {\em t}
given by the @shows@ function to the file or channel managed by {\em
hdl}.

[ Seem to have disappeared from the 1.4 interface  - SOF 2/97 ]

\begin{code}
hPrint :: Show a => Handle -> a -> IO ()
hPrint hdl = hPutStrLn hdl . show
\end{code}

Derived action @hPutStrLn hdl str@ writes the string \tr{str} to
the handle \tr{hdl}, adding a newline at the end.

\begin{code}
hPutStrLn :: Handle -> String -> IO ()
hPutStrLn hndl str = do
 hPutStr  hndl str
 hPutChar hndl '\n'
\end{code}

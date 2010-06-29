{-# OPTIONS_GHC -XNoImplicitPrelude -#include "HsBase.h" #-}
{-# OPTIONS_GHC -XRecordWildCards -XBangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Text
-- Copyright   :  (c) The University of Glasgow, 1992-2008
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
module GHC.IO.Handle.Text ( 
   hWaitForInput, hGetChar, hGetLine, hGetContents, hPutChar, hPutStr,
   commitBuffer',       -- hack, see below
   hGetBuf, hGetBufSome, hGetBufNonBlocking, hPutBuf, hPutBufNonBlocking,
   memcpy,
 ) where

import GHC.IO
import GHC.IO.FD
import GHC.IO.Buffer
import qualified GHC.IO.BufferedIO as Buffered
import GHC.IO.Exception
import GHC.Exception
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import qualified GHC.IO.Device as IODevice
import qualified GHC.IO.Device as RawIO

import Foreign
import Foreign.C

import Data.Typeable
import System.IO.Error
import Data.Maybe
import Control.Monad

import GHC.IORef
import GHC.Base
import GHC.Real
import GHC.Num
import GHC.Show
import GHC.List

-- ---------------------------------------------------------------------------
-- Simple input operations

-- If hWaitForInput finds anything in the Handle's buffer, it
-- immediately returns.  If not, it tries to read from the underlying
-- OS handle. Notice that for buffered Handles connected to terminals
-- this means waiting until a complete line is available.

-- | Computation 'hWaitForInput' @hdl t@
-- waits until input is available on handle @hdl@.
-- It returns 'True' as soon as input is available on @hdl@,
-- or 'False' if no input is available within @t@ milliseconds.  Note that
-- 'hWaitForInput' waits until one or more full /characters/ are available,
-- which means that it needs to do decoding, and hence may fail
-- with a decoding error.
--
-- If @t@ is less than zero, then @hWaitForInput@ waits indefinitely.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.
--
--  * a decoding error, if the input begins with an invalid byte sequence
--    in this Handle's encoding.
--
-- NOTE for GHC users: unless you use the @-threaded@ flag,
-- @hWaitForInput t@ where @t >= 0@ will block all other Haskell
-- threads for the duration of the call.  It behaves like a
-- @safe@ foreign call in this respect.
--

hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput h msecs = do
  wantReadableHandle_ "hWaitForInput" h $ \ handle_@Handle__{..} -> do
  cbuf <- readIORef haCharBuffer

  if not (isEmptyBuffer cbuf) then return True else do

  if msecs < 0 
        then do cbuf' <- readTextDevice handle_ cbuf
                writeIORef haCharBuffer cbuf'
                return True
        else do
               -- there might be bytes in the byte buffer waiting to be decoded
               cbuf' <- decodeByteBuf handle_ cbuf
               writeIORef haCharBuffer cbuf'

               if not (isEmptyBuffer cbuf') then return True else do

                r <- IODevice.ready haDevice False{-read-} msecs
                if r then do -- Call hLookAhead' to throw an EOF
                             -- exception if appropriate
                             _ <- hLookAhead_ handle_
                             return True
                     else return False
                -- XXX we should only return when there are full characters
                -- not when there are only bytes.  That would mean looping
                -- and re-running IODevice.ready if we don't have any full
                -- characters; but we don't know how long we've waited
                -- so far.

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
  wantReadableHandle_ "hGetChar" handle $ \handle_@Handle__{..} -> do

  -- buffering mode makes no difference: we just read whatever is available
  -- from the device (blocking only if there is nothing available), and then
  -- return the first character.
  -- See [note Buffered Reading] in GHC.IO.Handle.Types
  buf0 <- readIORef haCharBuffer

  buf1 <- if isEmptyBuffer buf0
             then readTextDevice handle_ buf0
             else return buf0

  (c1,i) <- readCharBuf (bufRaw buf1) (bufL buf1)
  let buf2 = bufferAdjustL i buf1

  if haInputNL == CRLF && c1 == '\r'
     then do
            mbuf3 <- if isEmptyBuffer buf2
                      then maybeFillReadBuffer handle_ buf2
                      else return (Just buf2)

            case mbuf3 of
               -- EOF, so just return the '\r' we have
               Nothing -> do
                  writeIORef haCharBuffer buf2
                  return '\r'
               Just buf3 -> do
                  (c2,i2) <- readCharBuf (bufRaw buf2) (bufL buf2)
                  if c2 == '\n'
                     then do
                       writeIORef haCharBuffer (bufferAdjustL i2 buf3)
                       return '\n'
                     else do
                       -- not a \r\n sequence, so just return the \r
                       writeIORef haCharBuffer buf3
                       return '\r'
     else do
            writeIORef haCharBuffer buf2
            return c1

-- ---------------------------------------------------------------------------
-- hGetLine

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
hGetLine h =
  wantReadableHandle_ "hGetLine" h $ \ handle_ -> do
     hGetLineBuffered handle_

hGetLineBuffered :: Handle__ -> IO String
hGetLineBuffered handle_@Handle__{..} = do
  buf <- readIORef haCharBuffer
  hGetLineBufferedLoop handle_ buf []

hGetLineBufferedLoop :: Handle__
                     -> CharBuffer -> [String]
                     -> IO String
hGetLineBufferedLoop handle_@Handle__{..}
        buf@Buffer{ bufL=r0, bufR=w, bufRaw=raw0 } xss =
  let
        -- find the end-of-line character, if there is one
        loop raw r
           | r == w = return (False, w)
           | otherwise =  do
                (c,r') <- readCharBuf raw r
                if c == '\n'
                   then return (True, r) -- NB. not r': don't include the '\n'
                   else loop raw r'
  in do
  (eol, off) <- loop raw0 r0

  debugIO ("hGetLineBufferedLoop: r=" ++ show r0 ++ ", w=" ++ show w ++ ", off=" ++ show off)

  (xs,r') <- if haInputNL == CRLF
                then unpack_nl raw0 r0 off ""
                else do xs <- unpack raw0 r0 off ""
                        return (xs,off)

  -- if eol == True, then off is the offset of the '\n'
  -- otherwise off == w and the buffer is now empty.
  if eol -- r' == off
        then do writeIORef haCharBuffer (bufferAdjustL (off+1) buf)
                return (concat (reverse (xs:xss)))
        else do
             let buf1 = bufferAdjustL r' buf
             maybe_buf <- maybeFillReadBuffer handle_ buf1
             case maybe_buf of
                -- Nothing indicates we caught an EOF, and we may have a
                -- partial line to return.
                Nothing -> do
                     -- we reached EOF.  There might be a lone \r left
                     -- in the buffer, so check for that and
                     -- append it to the line if necessary.
                     -- 
                     let pre = if not (isEmptyBuffer buf1) then "\r" else ""
                     writeIORef haCharBuffer buf1{ bufL=0, bufR=0 }
                     let str = concat (reverse (pre:xs:xss))
                     if not (null str)
                        then return str
                        else ioe_EOF
                Just new_buf ->
                     hGetLineBufferedLoop handle_ new_buf (xs:xss)

maybeFillReadBuffer :: Handle__ -> CharBuffer -> IO (Maybe CharBuffer)
maybeFillReadBuffer handle_ buf
  = catch 
     (do buf' <- getSomeCharacters handle_ buf
         return (Just buf')
     )
     (\e -> do if isEOFError e 
                  then return Nothing 
                  else ioError e)

-- See GHC.IO.Buffer
#define CHARBUF_UTF32
-- #define CHARBUF_UTF16

-- NB. performance-critical code: eyeball the Core.
unpack :: RawCharBuffer -> Int -> Int -> [Char] -> IO [Char]
unpack !buf !r !w acc0
 | r == w    = return acc0
 | otherwise = 
  withRawBuffer buf $ \pbuf -> 
    let
        unpackRB acc !i
         | i < r  = return acc
         | otherwise = do
#ifdef CHARBUF_UTF16
              -- reverse-order decoding of UTF-16
              c2 <- peekElemOff pbuf i
              if (c2 < 0xdc00 || c2 > 0xdffff)
                 then unpackRB (unsafeChr (fromIntegral c2) : acc) (i-1)
                 else do c1 <- peekElemOff pbuf (i-1)
                         let c = (fromIntegral c1 - 0xd800) * 0x400 +
                                 (fromIntegral c2 - 0xdc00) + 0x10000
                         unpackRB (unsafeChr c : acc) (i-2)
#else
              c <- peekElemOff pbuf i
              unpackRB (c:acc) (i-1)
#endif
     in
     unpackRB acc0 (w-1)

-- NB. performance-critical code: eyeball the Core.
unpack_nl :: RawCharBuffer -> Int -> Int -> [Char] -> IO ([Char],Int)
unpack_nl !buf !r !w acc0
 | r == w    =  return (acc0, 0)
 | otherwise =
  withRawBuffer buf $ \pbuf ->
    let
        unpackRB acc !i
         | i < r  = return acc
         | otherwise = do
              c <- peekElemOff pbuf i
              if (c == '\n' && i > r)
                 then do
                         c1 <- peekElemOff pbuf (i-1)
                         if (c1 == '\r')
                            then unpackRB ('\n':acc) (i-2)
                            else unpackRB ('\n':acc) (i-1)
                 else do
                         unpackRB (c:acc) (i-1)
     in do
     c <- peekElemOff pbuf (w-1)
     if (c == '\r')
        then do 
                -- If the last char is a '\r', we need to know whether or
                -- not it is followed by a '\n', so leave it in the buffer
                -- for now and just unpack the rest.
                str <- unpackRB acc0 (w-2)
                return (str, w-1)
        else do
                str <- unpackRB acc0 (w-1)
                return (str, w)


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
   wantReadableHandle "hGetContents" handle $ \handle_ -> do
      xs <- lazyRead handle
      return (handle_{ haType=SemiClosedHandle}, xs )

-- Note that someone may close the semi-closed handle (or change its
-- buffering), so each time these lazy read functions are pulled on,
-- they have to check whether the handle has indeed been closed.

lazyRead :: Handle -> IO String
lazyRead handle = 
   unsafeInterleaveIO $
        withHandle "hGetContents" handle $ \ handle_ -> do
        case haType handle_ of
          ClosedHandle     -> return (handle_, "")
          SemiClosedHandle -> lazyReadBuffered handle handle_
          _ -> ioException 
                  (IOError (Just handle) IllegalOperation "hGetContents"
                        "illegal handle type" Nothing Nothing)

lazyReadBuffered :: Handle -> Handle__ -> IO (Handle__, [Char])
lazyReadBuffered h handle_@Handle__{..} = do
   buf <- readIORef haCharBuffer
   catch 
        (do 
            buf'@Buffer{..} <- getSomeCharacters handle_ buf
            lazy_rest <- lazyRead h
            (s,r) <- if haInputNL == CRLF
                         then unpack_nl bufRaw bufL bufR lazy_rest
                         else do s <- unpack bufRaw bufL bufR lazy_rest
                                 return (s,bufR)
            writeIORef haCharBuffer (bufferAdjustL r buf')
            return (handle_, s)
        )
        (\e -> do (handle_', _) <- hClose_help handle_
                  debugIO ("hGetContents caught: " ++ show e)
                  -- We might have a \r cached in CRLF mode.  So we
                  -- need to check for that and return it:
                  let r = if isEOFError e
                             then if not (isEmptyBuffer buf)
                                     then "\r"
                                     else ""
                             else
                                  throw (augmentIOError e "hGetContents" h)

                  return (handle_', r)
        )

-- ensure we have some characters in the buffer
getSomeCharacters :: Handle__ -> CharBuffer -> IO CharBuffer
getSomeCharacters handle_@Handle__{..} buf@Buffer{..} =
  case bufferElems buf of

    -- buffer empty: read some more
    0 -> readTextDevice handle_ buf

    -- if the buffer has a single '\r' in it and we're doing newline
    -- translation: read some more
    1 | haInputNL == CRLF -> do
      (c,_) <- readCharBuf bufRaw bufL
      if c == '\r'
         then do -- shuffle the '\r' to the beginning.  This is only safe
                 -- if we're about to call readTextDevice, otherwise it
                 -- would mess up flushCharBuffer.
                 -- See [note Buffer Flushing], GHC.IO.Handle.Types
                 _ <- writeCharBuf bufRaw 0 '\r'
                 let buf' = buf{ bufL=0, bufR=1 }
                 readTextDevice handle_ buf'
         else do
                 return buf

    -- buffer has some chars in it already: just return it
    _otherwise ->
      return buf

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
    case haBufferMode handle_ of
        LineBuffering -> hPutcBuffered handle_ True  c
        _other        -> hPutcBuffered handle_ False c

hPutcBuffered :: Handle__ -> Bool -> Char -> IO ()
hPutcBuffered handle_@Handle__{..} is_line c = do
  buf <- readIORef haCharBuffer
  if c == '\n'
     then do buf1 <- if haOutputNL == CRLF
                        then do
                          buf1 <- putc buf '\r'
                          putc buf1 '\n'
                        else do
                          putc buf '\n'
             if is_line 
                then do
                  flushed_buf <- flushWriteBuffer_ handle_ buf1
                  writeIORef haCharBuffer flushed_buf
                else
                  writeIORef haCharBuffer buf1
      else do
          buf1 <- putc buf c
          writeIORef haCharBuffer buf1
  where
    putc buf@Buffer{ bufRaw=raw, bufR=w } c = do
       debugIO ("putc: " ++ summaryBuffer buf)
       w'  <- writeCharBuf raw w c
       let buf' = buf{ bufR = w' }
       if isFullCharBuffer buf'
          then flushWriteBuffer_ handle_ buf'
          else return buf'

-- ---------------------------------------------------------------------------
-- hPutStr

-- We go to some trouble to avoid keeping the handle locked while we're
-- evaluating the string argument to hPutStr, in case doing so triggers another
-- I/O operation on the same handle which would lead to deadlock.  The classic
-- case is
--
--              putStr (trace "hello" "world")
--
-- so the basic scheme is this:
--
--      * copy the string into a fresh buffer,
--      * "commit" the buffer to the handle.
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
    (buffer_mode, nl) <- 
         wantWritableHandle "hPutStr" handle $ \h_ -> do
                       bmode <- getSpareBuffer h_
                       return (bmode, haOutputNL h_)

    case buffer_mode of
       (NoBuffering, _) -> do
            hPutChars handle str        -- v. slow, but we don't care
       (LineBuffering, buf) -> do
            writeBlocks handle True  nl buf str
       (BlockBuffering _, buf) -> do
            writeBlocks handle False nl buf str

hPutChars :: Handle -> [Char] -> IO ()
hPutChars _      [] = return ()
hPutChars handle (c:cs) = hPutChar handle c >> hPutChars handle cs

getSpareBuffer :: Handle__ -> IO (BufferMode, CharBuffer)
getSpareBuffer Handle__{haCharBuffer=ref, 
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
                return ( mode, emptyBuffer b (bufSize buf) WriteBuffer)
            BufferListNil -> do
                new_buf <- newCharBuffer (bufSize buf) WriteBuffer
                return (mode, new_buf)


-- NB. performance-critical code: eyeball the Core.
writeBlocks :: Handle -> Bool -> Newline -> Buffer CharBufElem -> String -> IO ()
writeBlocks hdl line_buffered nl
            buf@Buffer{ bufRaw=raw, bufSize=len } s =
  let
   shoveString :: Int -> [Char] -> IO ()
   shoveString !n [] = do
        _ <- commitBuffer hdl raw len n False{-no flush-} True{-release-}
        return ()
   shoveString !n (c:cs)
     -- n+1 so we have enough room to write '\r\n' if necessary
     | n + 1 >= len = do
        new_buf <- commitBuffer hdl raw len n True{-needs flush-} False
        writeBlocks hdl line_buffered nl new_buf (c:cs)
     | c == '\n'  =  do
        n' <- if nl == CRLF
                 then do 
                    n1 <- writeCharBuf raw n  '\r'
                    writeCharBuf raw n1 '\n'
                 else do
                    writeCharBuf raw n c
        if line_buffered
           then do
               new_buf <- commitBuffer hdl raw len n' True{-needs flush-} False
               writeBlocks hdl line_buffered nl new_buf cs
           else do
               shoveString n' cs
     | otherwise = do
        n' <- writeCharBuf raw n c
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
--       1. If there isn't room in the handle buffer, flush the handle
--          buffer.
-- 
--       2. If the handle buffer is empty,
--               if flush, 
--                   then write buf directly to the device.
--                   else swap the handle buffer with buf.
-- 
--       3. If the handle buffer is non-empty, copy buf into the
--          handle buffer.  Then, if flush != 0, flush
--          the buffer.

commitBuffer
        :: Handle                       -- handle to commit to
        -> RawCharBuffer -> Int         -- address and size (in bytes) of buffer
        -> Int                          -- number of bytes of data in buffer
        -> Bool                         -- True <=> flush the handle afterward
        -> Bool                         -- release the buffer?
        -> IO CharBuffer

commitBuffer hdl !raw !sz !count flush release = 
  wantWritableHandle "commitAndReleaseBuffer" hdl $
     commitBuffer' raw sz count flush release
{-# NOINLINE commitBuffer #-}

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
commitBuffer' :: RawCharBuffer -> Int -> Int -> Bool -> Bool -> Handle__
              -> IO CharBuffer
commitBuffer' raw sz@(I# _) count@(I# _) flush release
  handle_@Handle__{ haCharBuffer=ref, haBuffers=spare_buf_ref } = do

      debugIO ("commitBuffer: sz=" ++ show sz ++ ", count=" ++ show count
            ++ ", flush=" ++ show flush ++ ", release=" ++ show release)

      old_buf@Buffer{ bufRaw=old_raw, bufR=w, bufSize=size }
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
                -- just copy the data in and update bufR.
            then do withRawBuffer raw     $ \praw ->
                      copyToRawBuffer old_raw (w*charSize)
                                      praw (fromIntegral (count*charSize))
                    writeIORef ref old_buf{ bufR = w + count }
                    return (emptyBuffer raw sz WriteBuffer)

                -- else, we have to flush
            else do flushed_buf <- flushWriteBuffer_ handle_ old_buf

                    let this_buf = 
                            Buffer{ bufRaw=raw, bufState=WriteBuffer, 
                                    bufL=0, bufR=count, bufSize=sz }

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
                          -- We're aren't going to use this buffer again
                          -- so we ignore the result of flushWriteBuffer_
                          _ <- flushWriteBuffer_ handle_ this_buf
                          writeIORef ref flushed_buf
                            -- if the sizes were different, then allocate
                            -- a new buffer of the correct size.
                          if sz == size
                             then return (emptyBuffer raw sz WriteBuffer)
                             else newCharBuffer size WriteBuffer

      -- release the buffer if necessary
      case buf_ret of
        Buffer{ bufSize=buf_ret_sz, bufRaw=buf_ret_raw } -> do
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
-- 'hPutBuf' ignores any text encoding that applies to the 'Handle',
-- writing the bytes directly to the underlying file or device.
--
-- 'hPutBuf' ignores the prevailing 'TextEncoding' and
-- 'NewlineMode' on the 'Handle', and writes bytes directly.
--
-- This operation may fail with:
--
--  * 'ResourceVanished' if the handle is a pipe or socket, and the
--    reading end is closed.  (If this is a POSIX system, and the program
--    has not asked to ignore SIGPIPE, then a SIGPIPE may be delivered
--    instead, whose default action is to terminate the program).

hPutBuf :: Handle                       -- handle to write to
        -> Ptr a                        -- address of buffer
        -> Int                          -- number of bytes of data in buffer
        -> IO ()
hPutBuf h ptr count = do _ <- hPutBuf' h ptr count True
                         return ()

hPutBufNonBlocking
        :: Handle                       -- handle to write to
        -> Ptr a                        -- address of buffer
        -> Int                          -- number of bytes of data in buffer
        -> IO Int                       -- returns: number of bytes written
hPutBufNonBlocking h ptr count = hPutBuf' h ptr count False

hPutBuf':: Handle                       -- handle to write to
        -> Ptr a                        -- address of buffer
        -> Int                          -- number of bytes of data in buffer
        -> Bool                         -- allow blocking?
        -> IO Int
hPutBuf' handle ptr count can_block
  | count == 0 = return 0
  | count <  0 = illegalBufferSize handle "hPutBuf" count
  | otherwise = 
    wantWritableHandle "hPutBuf" handle $ 
      \ h_@Handle__{..} -> do
          debugIO ("hPutBuf count=" ++ show count)
          -- first flush the Char buffer if it is non-empty, then we
          -- can work directly with the byte buffer
          cbuf <- readIORef haCharBuffer
          when (not (isEmptyBuffer cbuf)) $ flushWriteBuffer h_

          r <- bufWrite h_ (castPtr ptr) count can_block

          -- we must flush if this Handle is set to NoBuffering.  If
          -- it is set to LineBuffering, be conservative and flush
          -- anyway (we didn't check for newlines in the data).
          case haBufferMode of
             BlockBuffering _      -> do return ()
             _line_or_no_buffering -> do flushWriteBuffer h_
          return r

bufWrite :: Handle__-> Ptr Word8 -> Int -> Bool -> IO Int
bufWrite h_@Handle__{..} ptr count can_block =
  seq count $ do  -- strictness hack
  old_buf@Buffer{ bufRaw=old_raw, bufR=w, bufSize=size }
     <- readIORef haByteBuffer

  -- enough room in handle buffer?
  if (size - w > count)
        -- There's enough room in the buffer:
        -- just copy the data in and update bufR.
        then do debugIO ("hPutBuf: copying to buffer, w=" ++ show w)
                copyToRawBuffer old_raw w ptr (fromIntegral count)
                writeIORef haByteBuffer old_buf{ bufR = w + count }
                return count

        -- else, we have to flush
        else do debugIO "hPutBuf: flushing first"
                old_buf' <- Buffered.flushWriteBuffer haDevice old_buf
                        -- TODO: we should do a non-blocking flush here
                writeIORef haByteBuffer old_buf'
                -- if we can fit in the buffer, then just loop  
                if count < size
                   then bufWrite h_ ptr count can_block
                   else if can_block
                           then do writeChunk h_ (castPtr ptr) count
                                   return count
                           else writeChunkNonBlocking h_ (castPtr ptr) count

writeChunk :: Handle__ -> Ptr Word8 -> Int -> IO ()
writeChunk h_@Handle__{..} ptr bytes
  | Just fd <- cast haDevice  =  RawIO.write (fd::FD) ptr bytes
  | otherwise = error "Todo: hPutBuf"

writeChunkNonBlocking :: Handle__ -> Ptr Word8 -> Int -> IO Int
writeChunkNonBlocking h_@Handle__{..} ptr bytes 
  | Just fd <- cast haDevice  =  RawIO.writeNonBlocking (fd::FD) ptr bytes
  | otherwise = error "Todo: hPutBuf"

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
--
-- 'hGetBuf' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle', and reads bytes directly.

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf h ptr count
  | count == 0 = return 0
  | count <  0 = illegalBufferSize h "hGetBuf" count
  | otherwise = 
      wantReadableHandle_ "hGetBuf" h $ \ h_@Handle__{..} -> do
         flushCharReadBuffer h_
         buf@Buffer{ bufRaw=raw, bufR=w, bufL=r, bufSize=sz }
            <- readIORef haByteBuffer
         if isEmptyBuffer buf
            then bufReadEmpty    h_ buf (castPtr ptr) 0 count
            else bufReadNonEmpty h_ buf (castPtr ptr) 0 count

-- small reads go through the buffer, large reads are satisfied by
-- taking data first from the buffer and then direct from the file
-- descriptor.

bufReadNonEmpty :: Handle__ -> Buffer Word8 -> Ptr Word8 -> Int -> Int -> IO Int
bufReadNonEmpty h_@Handle__{..}
                buf@Buffer{ bufRaw=raw, bufR=w, bufL=r, bufSize=sz }
                ptr !so_far !count 
 = do
        let avail = w - r
        if (count < avail)
           then do 
                copyFromRawBuffer ptr raw r count
                writeIORef haByteBuffer buf{ bufL = r + count }
                return (so_far + count)
           else do
  
        copyFromRawBuffer ptr raw (fromIntegral r) (fromIntegral avail)
        let buf' = buf{ bufR=0, bufL=0 }
        writeIORef haByteBuffer buf'
        let remaining = count - avail
            so_far' = so_far + avail
            ptr' = ptr `plusPtr` avail

        if remaining == 0 
           then return so_far'
           else bufReadEmpty h_ buf' ptr' so_far' remaining


bufReadEmpty :: Handle__ -> Buffer Word8 -> Ptr Word8 -> Int -> Int -> IO Int
bufReadEmpty h_@Handle__{..}
             buf@Buffer{ bufRaw=raw, bufR=w, bufL=r, bufSize=sz }
             ptr so_far count
 | count > sz, Just fd <- cast haDevice = loop fd 0 count
 | otherwise = do
     (r,buf') <- Buffered.fillReadBuffer haDevice buf
     if r == 0 
        then return so_far
        else do writeIORef haByteBuffer buf'
                bufReadNonEmpty h_ buf' ptr so_far count
 where
  loop :: FD -> Int -> Int -> IO Int
  loop fd off bytes | bytes <= 0 = return off
  loop fd off bytes = do
    r <- RawIO.read (fd::FD) (ptr `plusPtr` off) (fromIntegral bytes)
    if r == 0
        then return (so_far + off)
        else loop fd (off + r) (bytes - r)

-- ---------------------------------------------------------------------------
-- hGetBufSome

-- | 'hGetBufSome' @hdl buf count@ reads data from the handle @hdl@
-- into the buffer @buf@.  If there is any data available to read,
-- then 'hGetBufSome' returns it immediately; it only blocks if there
-- is no data to be read.
--
-- It returns the number of bytes actually read.  This may be zero if
-- EOF was reached before any data was read (or if @count@ is zero).
--
-- 'hGetBufSome' never raises an EOF exception, instead it returns a value
-- smaller than @count@.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGetBufSome' will behave as if EOF was reached.
--
-- 'hGetBufSome' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle', and reads bytes directly.

hGetBufSome :: Handle -> Ptr a -> Int -> IO Int
hGetBufSome h ptr count
  | count == 0 = return 0
  | count <  0 = illegalBufferSize h "hGetBuf" count
  | otherwise =
      wantReadableHandle_ "hGetBuf" h $ \ h_@Handle__{..} -> do
         flushCharReadBuffer h_
         buf@Buffer{ bufSize=sz } <- readIORef haByteBuffer
         if isEmptyBuffer buf
            then if count > sz  -- large read?
                    then do RawIO.read (haFD h_) (castPtr ptr) count
                    else do (r,buf') <- Buffered.fillReadBuffer haDevice buf
                            if r == 0
                               then return 0
                               else do writeIORef haByteBuffer buf'
                                       bufReadNBNonEmpty h_ buf' (castPtr ptr) 0 count
            else
              bufReadNBEmpty h_ buf (castPtr ptr) 0 count

haFD :: Handle__ -> FD
haFD h_@Handle__{..} =
   case cast haDevice of
             Nothing -> error "not an FD"
             Just fd -> fd

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
-- 'hGetBufNonBlocking' ignores the prevailing 'TextEncoding' and
-- 'NewlineMode' on the 'Handle', and reads bytes directly.
--
-- NOTE: on Windows, this function does not work correctly; it
-- behaves identically to 'hGetBuf'.

hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking h ptr count
  | count == 0 = return 0
  | count <  0 = illegalBufferSize h "hGetBufNonBlocking" count
  | otherwise = 
      wantReadableHandle_ "hGetBufNonBlocking" h $ \ h_@Handle__{..} -> do
         flushCharReadBuffer h_
         buf@Buffer{ bufRaw=raw, bufR=w, bufL=r, bufSize=sz }
            <- readIORef haByteBuffer
         if isEmptyBuffer buf
            then bufReadNBEmpty    h_ buf (castPtr ptr) 0 count
            else bufReadNBNonEmpty h_ buf (castPtr ptr) 0 count

bufReadNBEmpty :: Handle__ -> Buffer Word8 -> Ptr Word8 -> Int -> Int -> IO Int
bufReadNBEmpty   h_@Handle__{..}
                 buf@Buffer{ bufRaw=raw, bufR=w, bufL=r, bufSize=sz }
                 ptr so_far count
  | count > sz, False,
    Just fd <- cast haDevice = do
       m <- RawIO.readNonBlocking (fd::FD) ptr count
       case m of
         Nothing -> return so_far
         Just n  -> return (so_far + n)

 | otherwise = do
     buf <- readIORef haByteBuffer
     (r,buf') <- Buffered.fillReadBuffer0 haDevice buf
     case r of
       Nothing -> return so_far
       Just 0  -> return so_far
       Just r  -> do
         writeIORef haByteBuffer buf'
         bufReadNBNonEmpty h_ buf' ptr so_far (min count r)
                          -- NOTE: new count is    min count r
                          -- so we will just copy the contents of the
                          -- buffer in the recursive call, and not
                          -- loop again.


bufReadNBNonEmpty :: Handle__ -> Buffer Word8 -> Ptr Word8 -> Int -> Int -> IO Int
bufReadNBNonEmpty h_@Handle__{..}
                  buf@Buffer{ bufRaw=raw, bufR=w, bufL=r, bufSize=sz }
                  ptr so_far count
  = do
        let avail = w - r
        if (count < avail)
           then do 
                copyFromRawBuffer ptr raw r count
                writeIORef haByteBuffer buf{ bufL = r + count }
                return (so_far + count)
           else do

        copyFromRawBuffer ptr raw (fromIntegral r) (fromIntegral avail)
        let buf' = buf{ bufR=0, bufL=0 }
        writeIORef haByteBuffer buf'
        let remaining = count - avail
            so_far' = so_far + avail
            ptr' = ptr `plusPtr` avail

        if remaining == 0
           then return so_far'
           else bufReadNBEmpty h_ buf' ptr' so_far' remaining

-- ---------------------------------------------------------------------------
-- memcpy wrappers

copyToRawBuffer :: RawBuffer e -> Int -> Ptr e -> Int -> IO ()
copyToRawBuffer raw off ptr bytes =
 withRawBuffer raw $ \praw ->
   do _ <- memcpy (praw `plusPtr` off) ptr (fromIntegral bytes)
      return ()

copyFromRawBuffer :: Ptr e -> RawBuffer e -> Int -> Int -> IO ()
copyFromRawBuffer ptr raw off bytes =
 withRawBuffer raw $ \praw ->
   do _ <- memcpy ptr (praw `plusPtr` off) (fromIntegral bytes)
      return ()

foreign import ccall unsafe "memcpy"
   memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr ())

-----------------------------------------------------------------------------
-- Internal Utils

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
        ioException (IOError (Just handle)
                            InvalidArgument  fn
                            ("illegal buffer size " ++ showsPrec 9 sz [])
                            Nothing Nothing)

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , RecordWildCards
           , NondecreasingIndentation
  #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Handle
-- Copyright   :  (c) The University of Glasgow, 1994-2009
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable
--
-- External API for GHC's Handle implementation
--
-----------------------------------------------------------------------------

module GHC.IO.Handle (
   Handle,
   BufferMode(..),
 
   mkFileHandle, mkDuplexHandle,
 
   hFileSize, hSetFileSize, hIsEOF, hLookAhead,
   hSetBuffering, hSetBinaryMode, hSetEncoding, hGetEncoding,
   hFlush, hFlushAll, hDuplicate, hDuplicateTo,
 
   hClose, hClose_help,
 
   HandlePosition, HandlePosn(..), hGetPosn, hSetPosn,
   SeekMode(..), hSeek, hTell,
 
   hIsOpen, hIsClosed, hIsReadable, hIsWritable, hGetBuffering, hIsSeekable,
   hSetEcho, hGetEcho, hIsTerminalDevice,
 
   hSetNewlineMode, Newline(..), NewlineMode(..), nativeNewline,
   noNewlineTranslation, universalNewlineMode, nativeNewlineMode,

   hShow,

   hWaitForInput, hGetChar, hGetLine, hGetContents, hPutChar, hPutStr,

   hGetBuf, hGetBufNonBlocking, hPutBuf, hPutBufNonBlocking
 ) where

import GHC.IO
import GHC.IO.Exception
import GHC.IO.Encoding
import GHC.IO.Buffer
import GHC.IO.BufferedIO ( BufferedIO )
import GHC.IO.Device as IODevice
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Text
import qualified GHC.IO.BufferedIO as Buffered

import GHC.Base
import GHC.Exception
import GHC.MVar
import GHC.IORef
import GHC.Show
import GHC.Num
import GHC.Real
import Data.Maybe
import Data.Typeable
import Control.Monad

-- ---------------------------------------------------------------------------
-- Closing a handle

-- | Computation 'hClose' @hdl@ makes handle @hdl@ closed.  Before the
-- computation finishes, if @hdl@ is writable its buffer is flushed as
-- for 'hFlush'.
-- Performing 'hClose' on a handle that has already been closed has no effect; 
-- doing so is not an error.  All other operations on a closed handle will fail.
-- If 'hClose' fails for any reason, any further operations (apart from
-- 'hClose') on the handle will still fail as if @hdl@ had been successfully
-- closed.

hClose :: Handle -> IO ()
hClose h@(FileHandle _ m)     = do 
  mb_exc <- hClose' h m
  hClose_maybethrow mb_exc h
hClose h@(DuplexHandle _ r w) = do
  excs <- mapM (hClose' h) [r,w]
  hClose_maybethrow (listToMaybe (catMaybes excs)) h

hClose_maybethrow :: Maybe SomeException -> Handle -> IO ()
hClose_maybethrow Nothing  h = return ()
hClose_maybethrow (Just e) h = hClose_rethrow e h

hClose_rethrow :: SomeException -> Handle -> IO ()
hClose_rethrow e h = 
  case fromException e of
    Just ioe -> ioError (augmentIOError ioe "hClose" h)
    Nothing  -> throwIO e

hClose' :: Handle -> MVar Handle__ -> IO (Maybe SomeException)
hClose' h m = withHandle' "hClose" h m $ hClose_help

-----------------------------------------------------------------------------
-- Detecting and changing the size of a file

-- | For a handle @hdl@ which attached to a physical file,
-- 'hFileSize' @hdl@ returns the size of that file in 8-bit bytes.

hFileSize :: Handle -> IO Integer
hFileSize handle =
    withHandle_ "hFileSize" handle $ \ handle_@Handle__{haDevice=dev} -> do
    case haType handle_ of 
      ClosedHandle              -> ioe_closedHandle
      SemiClosedHandle          -> ioe_closedHandle
      _ -> do flushWriteBuffer handle_
              r <- IODevice.getSize dev
              if r /= -1
                 then return r
                 else ioException (IOError Nothing InappropriateType "hFileSize"
                                   "not a regular file" Nothing Nothing)


-- | 'hSetFileSize' @hdl@ @size@ truncates the physical file with handle @hdl@ to @size@ bytes.

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize handle size =
    withHandle_ "hSetFileSize" handle $ \ handle_@Handle__{haDevice=dev} -> do
    case haType handle_ of 
      ClosedHandle              -> ioe_closedHandle
      SemiClosedHandle          -> ioe_closedHandle
      _ -> do flushWriteBuffer handle_
              IODevice.setSize dev size
              return ()

-- ---------------------------------------------------------------------------
-- Detecting the End of Input

-- | For a readable handle @hdl@, 'hIsEOF' @hdl@ returns
-- 'True' if no further input can be taken from @hdl@ or for a
-- physical file, if the current I\/O position is equal to the length of
-- the file.  Otherwise, it returns 'False'.
--
-- NOTE: 'hIsEOF' may block, because it has to attempt to read from
-- the stream to determine whether there is any more data to be read.

hIsEOF :: Handle -> IO Bool
hIsEOF handle = wantReadableHandle_ "hIsEOF" handle $ \Handle__{..} -> do

  cbuf <- readIORef haCharBuffer
  if not (isEmptyBuffer cbuf) then return False else do

  bbuf <- readIORef haByteBuffer
  if not (isEmptyBuffer bbuf) then return False else do

  -- NB. do no decoding, just fill the byte buffer; see #3808
  (r,bbuf') <- Buffered.fillReadBuffer haDevice bbuf
  if r == 0
     then return True
     else do writeIORef haByteBuffer bbuf'
             return False

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
hLookAhead handle =
  wantReadableHandle_ "hLookAhead"  handle hLookAhead_

-- ---------------------------------------------------------------------------
-- Buffering Operations

-- Three kinds of buffering are supported: line-buffering,
-- block-buffering or no-buffering.  See GHC.IO.Handle for definition and
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
  withAllHandles__ "hSetBuffering" handle $ \ handle_@Handle__{..} -> do
  case haType of
    ClosedHandle -> ioe_closedHandle
    _ -> do
         if mode == haBufferMode then return handle_ else do

         -- See [note Buffer Sizing] in GHC.IO.Handle.Types

          -- check for errors:
          case mode of
              BlockBuffering (Just n) | n <= 0    -> ioe_bufsiz n
              _ -> return ()

          -- for input terminals we need to put the terminal into
          -- cooked or raw mode depending on the type of buffering.
          is_tty <- IODevice.isTerminal haDevice
          when (is_tty && isReadableHandleType haType) $
                case mode of
#ifndef mingw32_HOST_OS
        -- 'raw' mode under win32 is a bit too specialised (and troublesome
        -- for most common uses), so simply disable its use here.
                  NoBuffering -> IODevice.setRaw haDevice True
#else
                  NoBuffering -> return ()
#endif
                  _           -> IODevice.setRaw haDevice False

          -- throw away spare buffers, they might be the wrong size
          writeIORef haBuffers BufferListNil

          return Handle__{ haBufferMode = mode,.. }

-- -----------------------------------------------------------------------------
-- hSetEncoding

-- | The action 'hSetEncoding' @hdl@ @encoding@ changes the text encoding
-- for the handle @hdl@ to @encoding@.  The default encoding when a 'Handle' is
-- created is 'localeEncoding', namely the default encoding for the current
-- locale.
--
-- To create a 'Handle' with no encoding at all, use 'openBinaryFile'.  To
-- stop further encoding or decoding on an existing 'Handle', use
-- 'hSetBinaryMode'.
--
-- 'hSetEncoding' may need to flush buffered data in order to change
-- the encoding.
--
hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetEncoding hdl encoding = do
  withAllHandles__ "hSetEncoding" hdl $ \h_@Handle__{..} -> do
    flushCharBuffer h_
    closeTextCodecs h_
    openTextEncoding (Just encoding) haType $ \ mb_encoder mb_decoder -> do
    bbuf <- readIORef haByteBuffer
    ref <- newIORef (error "last_decode")
    return (Handle__{ haLastDecode = ref, 
                      haDecoder = mb_decoder, 
                      haEncoder = mb_encoder,
                      haCodec   = Just encoding, .. })

-- | Return the current 'TextEncoding' for the specified 'Handle', or
-- 'Nothing' if the 'Handle' is in binary mode.
--
-- Note that the 'TextEncoding' remembers nothing about the state of
-- the encoder/decoder in use on this 'Handle'.  For example, if the
-- encoding in use is UTF-16, then using 'hGetEncoding' and
-- 'hSetEncoding' to save and restore the encoding may result in an
-- extra byte-order-mark being written to the file.
--
hGetEncoding :: Handle -> IO (Maybe TextEncoding)
hGetEncoding hdl =
  withHandle_ "hGetEncoding" hdl $ \h_@Handle__{..} -> return haCodec

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
hFlush handle = wantWritableHandle "hFlush" handle flushWriteBuffer

-- | The action 'hFlushAll' @hdl@ flushes all buffered data in @hdl@,
-- including any buffered read data.  Buffered read data is flushed
-- by seeking the file position back to the point before the bufferred
-- data was read, and hence only works if @hdl@ is seekable (see
-- 'hIsSeekable').
--
-- This operation may fail with:
--
--  * 'isFullError' if the device is full;
--
--  * 'isPermissionError' if a system resource limit would be exceeded.
--    It is unspecified whether the characters in the buffer are discarded
--    or retained under these circumstances;
--
--  * 'isIllegalOperation' if @hdl@ has buffered read data, and is not
--    seekable.

hFlushAll :: Handle -> IO () 
hFlushAll handle = withHandle_ "hFlushAll" handle flushBuffer

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
--  * 'isIllegalOperationError' if the Handle is not seekable, or does
--     not support the requested seek mode.
--
--  * 'isPermissionError' if a system resource limit would be exceeded.

hSeek :: Handle -> SeekMode -> Integer -> IO () 
hSeek handle mode offset =
    wantSeekableHandle "hSeek" handle $ \ handle_@Handle__{..} -> do
    debugIO ("hSeek " ++ show (mode,offset))
    buf <- readIORef haCharBuffer

    if isWriteBuffer buf
        then do flushWriteBuffer handle_
                IODevice.seek haDevice mode offset
        else do

    let r = bufL buf; w = bufR buf
    if mode == RelativeSeek && isNothing haDecoder && 
       offset >= 0 && offset < fromIntegral (w - r)
        then writeIORef haCharBuffer buf{ bufL = r + fromIntegral offset }
        else do 

    flushCharReadBuffer handle_
    flushByteReadBuffer handle_
    IODevice.seek haDevice mode offset


-- | Computation 'hTell' @hdl@ returns the current position of the
-- handle @hdl@, as the number of bytes from the beginning of
-- the file.  The value returned may be subsequently passed to
-- 'hSeek' to reposition the handle to the current position.
-- 
-- This operation may fail with:
--
--  * 'isIllegalOperationError' if the Handle is not seekable.
--
hTell :: Handle -> IO Integer
hTell handle = 
    wantSeekableHandle "hGetPosn" handle $ \ handle_@Handle__{..} -> do

      posn <- IODevice.tell haDevice

      -- we can't tell the real byte offset if there are buffered
      -- Chars, so must flush first:
      flushCharBuffer handle_

      bbuf <- readIORef haByteBuffer

      let real_posn
           | isWriteBuffer bbuf = posn + fromIntegral (bufferElems bbuf)
           | otherwise          = posn - fromIntegral (bufferElems bbuf)

      cbuf <- readIORef haCharBuffer
      debugIO ("\nhGetPosn: (posn, real_posn) = " ++ show (posn, real_posn))
      debugIO ("   cbuf: " ++ summaryBuffer cbuf ++
            "   bbuf: " ++ summaryBuffer bbuf)

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
      _                    -> return True

hIsClosed :: Handle -> IO Bool
hIsClosed handle =
    withHandle_ "hIsClosed" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle         -> return True
      _                    -> return False

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
      ClosedHandle         -> ioe_closedHandle
      SemiClosedHandle     -> ioe_closedHandle
      htype                -> return (isReadableHandleType htype)

hIsWritable :: Handle -> IO Bool
hIsWritable (DuplexHandle _ _ _) = return True
hIsWritable handle =
    withHandle_ "hIsWritable" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle         -> ioe_closedHandle
      SemiClosedHandle     -> ioe_closedHandle
      htype                -> return (isWritableHandleType htype)

-- | Computation 'hGetBuffering' @hdl@ returns the current buffering mode
-- for @hdl@.

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering handle = 
    withHandle_ "hGetBuffering" handle $ \ handle_ -> do
    case haType handle_ of 
      ClosedHandle         -> ioe_closedHandle
      _ -> 
           -- We're being non-standard here, and allow the buffering
           -- of a semi-closed handle to be queried.   -- sof 6/98
          return (haBufferMode handle_)  -- could be stricter..

hIsSeekable :: Handle -> IO Bool
hIsSeekable handle =
    withHandle_ "hIsSeekable" handle $ \ handle_@Handle__{..} -> do
    case haType of 
      ClosedHandle         -> ioe_closedHandle
      SemiClosedHandle     -> ioe_closedHandle
      AppendHandle         -> return False
      _                    -> IODevice.isSeekable haDevice

-- -----------------------------------------------------------------------------
-- Changing echo status (Non-standard GHC extensions)

-- | Set the echoing status of a handle connected to a terminal.

hSetEcho :: Handle -> Bool -> IO ()
hSetEcho handle on = do
    isT   <- hIsTerminalDevice handle
    if not isT
     then return ()
     else
      withHandle_ "hSetEcho" handle $ \ Handle__{..} -> do
      case haType of 
         ClosedHandle -> ioe_closedHandle
         _            -> IODevice.setEcho haDevice on

-- | Get the echoing status of a handle connected to a terminal.

hGetEcho :: Handle -> IO Bool
hGetEcho handle = do
    isT   <- hIsTerminalDevice handle
    if not isT
     then return False
     else
       withHandle_ "hGetEcho" handle $ \ Handle__{..} -> do
       case haType of 
         ClosedHandle -> ioe_closedHandle
         _            -> IODevice.getEcho haDevice

-- | Is the handle connected to a terminal?

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice handle = do
    withHandle_ "hIsTerminalDevice" handle $ \ Handle__{..} -> do
     case haType of 
       ClosedHandle -> ioe_closedHandle
       _            -> IODevice.isTerminal haDevice

-- -----------------------------------------------------------------------------
-- hSetBinaryMode

-- | Select binary mode ('True') or text mode ('False') on a open handle.
-- (See also 'openBinaryFile'.)
--
-- This has the same effect as calling 'hSetEncoding' with 'char8', together
-- with 'hSetNewlineMode' with 'noNewlineTranslation'.
--
hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode handle bin =
  withAllHandles__ "hSetBinaryMode" handle $ \ h_@Handle__{..} ->
    do 
         flushCharBuffer h_
         closeTextCodecs h_

         mb_te <- if bin then return Nothing
                         else fmap Just getLocaleEncoding

         openTextEncoding mb_te haType $ \ mb_encoder mb_decoder -> do

         -- should match the default newline mode, whatever that is
         let nl    | bin       = noNewlineTranslation
                   | otherwise = nativeNewlineMode

         bbuf <- readIORef haByteBuffer
         ref <- newIORef (error "codec_state", bbuf)

         return Handle__{ haLastDecode = ref,
                          haEncoder  = mb_encoder, 
                          haDecoder  = mb_decoder,
                          haCodec    = mb_te,
                          haInputNL  = inputNL nl,
                          haOutputNL = outputNL nl, .. }
  
-- -----------------------------------------------------------------------------
-- hSetNewlineMode

-- | Set the 'NewlineMode' on the specified 'Handle'.  All buffered
-- data is flushed first.
hSetNewlineMode :: Handle -> NewlineMode -> IO ()
hSetNewlineMode handle NewlineMode{ inputNL=i, outputNL=o } =
  withAllHandles__ "hSetNewlineMode" handle $ \h_@Handle__{..} ->
    do
         flushBuffer h_
         return h_{ haInputNL=i, haOutputNL=o }

-- -----------------------------------------------------------------------------
-- Duplicating a Handle

-- | Returns a duplicate of the original handle, with its own buffer.
-- The two Handles will share a file pointer, however.  The original
-- handle's buffer is flushed, including discarding any input data,
-- before the handle is duplicated.

hDuplicate :: Handle -> IO Handle
hDuplicate h@(FileHandle path m) = do
  withHandle_' "hDuplicate" h m $ \h_ ->
      dupHandle path h Nothing h_ (Just handleFinalizer)
hDuplicate h@(DuplexHandle path r w) = do
  write_side@(FileHandle _ write_m) <- 
     withHandle_' "hDuplicate" h w $ \h_ ->
        dupHandle path h Nothing h_ (Just handleFinalizer)
  read_side@(FileHandle _ read_m) <- 
    withHandle_' "hDuplicate" h r $ \h_ ->
        dupHandle path h (Just write_m) h_  Nothing
  return (DuplexHandle path read_m write_m)

dupHandle :: FilePath
          -> Handle
          -> Maybe (MVar Handle__)
          -> Handle__
          -> Maybe HandleFinalizer
          -> IO Handle
dupHandle filepath h other_side h_@Handle__{..} mb_finalizer = do
  -- flush the buffer first, so we don't have to copy its contents
  flushBuffer h_
  case other_side of
    Nothing -> do
       new_dev <- IODevice.dup haDevice
       dupHandle_ new_dev filepath other_side h_ mb_finalizer
    Just r  -> 
       withHandle_' "dupHandle" h r $ \Handle__{haDevice=dev} -> do
         dupHandle_ dev filepath other_side h_ mb_finalizer

dupHandle_ :: (IODevice dev, BufferedIO dev, Typeable dev) => dev
           -> FilePath
           -> Maybe (MVar Handle__)
           -> Handle__
           -> Maybe HandleFinalizer
           -> IO Handle
dupHandle_ new_dev filepath other_side h_@Handle__{..} mb_finalizer = do
   -- XXX wrong!
  mb_codec <- if isJust haEncoder then fmap Just getLocaleEncoding else return Nothing
  mkHandle new_dev filepath haType True{-buffered-} mb_codec
      NewlineMode { inputNL = haInputNL, outputNL = haOutputNL }
      mb_finalizer other_side

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
hDuplicateTo h1@(FileHandle path m1) h2@(FileHandle _ m2)  = do
 withHandle__' "hDuplicateTo" h2 m2 $ \h2_ -> do
   _ <- hClose_help h2_
   withHandle_' "hDuplicateTo" h1 m1 $ \h1_ -> do
     dupHandleTo path h1 Nothing h2_ h1_ (Just handleFinalizer)
hDuplicateTo h1@(DuplexHandle path r1 w1) h2@(DuplexHandle _ r2 w2)  = do
 withHandle__' "hDuplicateTo" h2 w2  $ \w2_ -> do
   _ <- hClose_help w2_
   withHandle_' "hDuplicateTo" h1 w1 $ \w1_ -> do
     dupHandleTo path h1 Nothing w2_ w1_ (Just handleFinalizer)
 withHandle__' "hDuplicateTo" h2 r2  $ \r2_ -> do
   _ <- hClose_help r2_
   withHandle_' "hDuplicateTo" h1 r1 $ \r1_ -> do
     dupHandleTo path h1 (Just w1) r2_ r1_ Nothing
hDuplicateTo h1 _ = 
  ioe_dupHandlesNotCompatible h1


ioe_dupHandlesNotCompatible :: Handle -> IO a
ioe_dupHandlesNotCompatible h =
   ioException (IOError (Just h) IllegalOperation "hDuplicateTo" 
                "handles are incompatible" Nothing Nothing)

dupHandleTo :: FilePath 
            -> Handle
            -> Maybe (MVar Handle__)
            -> Handle__
            -> Handle__
            -> Maybe HandleFinalizer
            -> IO Handle__
dupHandleTo filepath h other_side 
            hto_@Handle__{haDevice=devTo,..}
            h_@Handle__{haDevice=dev} mb_finalizer = do
  flushBuffer h_
  case cast devTo of
    Nothing   -> ioe_dupHandlesNotCompatible h
    Just dev' -> do 
      _ <- IODevice.dup2 dev dev'
      FileHandle _ m <- dupHandle_ dev' filepath other_side h_ mb_finalizer
      takeMVar m

-- ---------------------------------------------------------------------------
-- showing Handles.
--
-- | 'hShow' is in the 'IO' monad, and gives more comprehensive output
-- than the (pure) instance of 'Show' for 'Handle'.

hShow :: Handle -> IO String
hShow h@(FileHandle path _) = showHandle' path False h
hShow h@(DuplexHandle path _ _) = showHandle' path True h

showHandle' :: String -> Bool -> Handle -> IO String
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
             showString "buffering=" . showBufMode (unsafePerformIO (readIORef (haCharBuffer hdl_))) (haBufferMode hdl_) . showString "}" )
      ) "")
   where

    showHdl :: HandleType -> ShowS -> ShowS
    showHdl ht cont = 
       case ht of
        ClosedHandle  -> shows ht . showString "}"
        _ -> cont

    showBufMode :: Buffer e -> BufferMode -> ShowS
    showBufMode buf bmo =
      case bmo of
        NoBuffering   -> showString "none"
        LineBuffering -> showString "line"
        BlockBuffering (Just n) -> showString "block " . showParen True (shows n)
        BlockBuffering Nothing  -> showString "block " . showParen True (shows def)
      where
       def :: Int 
       def = bufSize buf


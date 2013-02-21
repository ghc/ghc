{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , RecordWildCards
           , BangPatterns
           , PatternGuards
           , NondecreasingIndentation
           , RankNTypes
  #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Handle.Internals
-- Copyright   :  (c) The University of Glasgow, 1994-2001
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- This module defines the basic operations on I\/O \"handles\".  All
-- of the operations defined here are independent of the underlying
-- device.
--
-----------------------------------------------------------------------------

-- #hide
module GHC.IO.Handle.Internals (
  withHandle, withHandle', withHandle_,
  withHandle__', withHandle_', withAllHandles__,
  wantWritableHandle, wantReadableHandle, wantReadableHandle_, 
  wantSeekableHandle,

  mkHandle, mkFileHandle, mkDuplexHandle,
  openTextEncoding, closeTextCodecs, initBufferState,
  dEFAULT_CHAR_BUFFER_SIZE,

  flushBuffer, flushWriteBuffer, flushCharReadBuffer,
  flushCharBuffer, flushByteReadBuffer, flushByteWriteBuffer,

  readTextDevice, writeCharBuffer, readTextDeviceNonBlocking,
  decodeByteBuf,

  augmentIOError,
  ioe_closedHandle, ioe_EOF, ioe_notReadable, ioe_notWritable,
  ioe_finalizedHandle, ioe_bufsiz,

  hClose_help, hLookAhead_,

  HandleFinalizer, handleFinalizer,

  debugIO,
 ) where

import GHC.IO
import GHC.IO.IOMode
import GHC.IO.Encoding as Encoding
import GHC.IO.Handle.Types
import GHC.IO.Buffer
import GHC.IO.BufferedIO (BufferedIO)
import GHC.IO.Exception
import GHC.IO.Device (IODevice, SeekMode(..))
import qualified GHC.IO.Device as IODevice
import qualified GHC.IO.BufferedIO as Buffered

import GHC.Conc.Sync
import GHC.Real
import GHC.Base
import GHC.Exception
import GHC.Num          ( Num(..) )
import GHC.Show
import GHC.IORef
import GHC.MVar
import Data.Typeable
import Control.Monad
import Data.Maybe
import Foreign.Safe
import System.Posix.Internals hiding (FD)

import Foreign.C

c_DEBUG_DUMP :: Bool
c_DEBUG_DUMP = False

-- ---------------------------------------------------------------------------
-- Creating a new handle

type HandleFinalizer = FilePath -> MVar Handle__ -> IO ()

newFileHandle :: FilePath -> Maybe HandleFinalizer -> Handle__ -> IO Handle
newFileHandle filepath mb_finalizer hc = do
  m <- newMVar hc
  case mb_finalizer of
    Just finalizer -> addMVarFinalizer m (finalizer filepath m)
    Nothing        -> return ()
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
original handle is always replaced.
-}

{-# INLINE withHandle #-}
withHandle :: String -> Handle -> (Handle__ -> IO (Handle__,a)) -> IO a
withHandle fun h@(FileHandle _ m)     act = withHandle' fun h m act
withHandle fun h@(DuplexHandle _ m _) act = withHandle' fun h m act

withHandle' :: String -> Handle -> MVar Handle__
   -> (Handle__ -> IO (Handle__,a)) -> IO a
withHandle' fun h m act =
 mask_ $ do
   (h',v)  <- do_operation fun h act m
   checkHandleInvariants h'
   putMVar m h'
   return v

{-# INLINE withHandle_ #-}
withHandle_ :: String -> Handle -> (Handle__ -> IO a) -> IO a
withHandle_ fun h@(FileHandle _ m)     act = withHandle_' fun h m act
withHandle_ fun h@(DuplexHandle _ m _) act = withHandle_' fun h m act

withHandle_' :: String -> Handle -> MVar Handle__ -> (Handle__ -> IO a) -> IO a
withHandle_' fun h m act = withHandle' fun h m $ \h_ -> do
                              a <- act h_
                              return (h_,a)

withAllHandles__ :: String -> Handle -> (Handle__ -> IO Handle__) -> IO ()
withAllHandles__ fun h@(FileHandle _ m)     act = withHandle__' fun h m act
withAllHandles__ fun h@(DuplexHandle _ r w) act = do
  withHandle__' fun h r act
  withHandle__' fun h w act

withHandle__' :: String -> Handle -> MVar Handle__ -> (Handle__ -> IO Handle__)
              -> IO ()
withHandle__' fun h m act =
 mask_ $ do
   h'  <- do_operation fun h act m
   checkHandleInvariants h'
   putMVar m h'
   return ()

do_operation :: String -> Handle -> (Handle__ -> IO a) -> MVar Handle__ -> IO a
do_operation fun h act m = do
  h_ <- takeMVar m
  checkHandleInvariants h_
  act h_ `catchException` handler h_
  where
    handler h_ e = do
      putMVar m h_
      case () of
        _ | Just ioe <- fromException e ->
            ioError (augmentIOError ioe fun h)
        _ | Just async_ex <- fromException e -> do -- see Note [async]
            let _ = async_ex :: SomeAsyncException
            t <- myThreadId
            throwTo t e
            do_operation fun h act m
        _otherwise ->
            throwIO e

-- Note [async]
--
-- If an asynchronous exception is raised during an I/O operation,
-- normally it is fine to just re-throw the exception synchronously.
-- However, if we are inside an unsafePerformIO or an
-- unsafeInterleaveIO, this would replace the enclosing thunk with the
-- exception raised, which is wrong (#3997).  We have to release the
-- lock on the Handle, but what do we replace the thunk with?  What
-- should happen when the thunk is subsequently demanded again?
--
-- The only sensible choice we have is to re-do the IO operation on
-- resumption, but then we have to be careful in the IO library that
-- this is always safe to do.  In particular we should
--
--    never perform any side-effects before an interruptible operation
--
-- because the interruptible operation may raise an asynchronous
-- exception, which may cause the operation and its side effects to be
-- subsequently performed again.
--
-- Re-doing the IO operation is achieved by:
--   - using throwTo to re-throw the asynchronous exception asynchronously
--     in the current thread
--   - on resumption, it will be as if throwTo returns.  In that case, we
--     recursively invoke the original operation (see do_operation above).
--
-- Interruptible operations in the I/O library are:
--    - threadWaitRead/threadWaitWrite
--    - fillReadBuffer/flushWriteBuffer
--    - readTextDevice/writeTextDevice

augmentIOError :: IOException -> String -> Handle -> IOException
augmentIOError ioe@IOError{ ioe_filename = fp } fun h
  = ioe { ioe_handle = Just h, ioe_location = fun, ioe_filename = filepath }
  where filepath
          | Just _ <- fp = fp
          | otherwise = case h of
                          FileHandle path _     -> Just path
                          DuplexHandle path _ _ -> Just path

-- ---------------------------------------------------------------------------
-- Wrapper for write operations.

wantWritableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantWritableHandle fun h@(FileHandle _ m) act
  = wantWritableHandle' fun h m act
wantWritableHandle fun h@(DuplexHandle _ _ m) act
  = wantWritableHandle' fun h m act
    -- we know it's not a ReadHandle or ReadWriteHandle, but we have to
    -- check for ClosedHandle/SemiClosedHandle. (#4808)

wantWritableHandle'
        :: String -> Handle -> MVar Handle__
        -> (Handle__ -> IO a) -> IO a
wantWritableHandle' fun h m act
   = withHandle_' fun h m (checkWritableHandle act)

checkWritableHandle :: (Handle__ -> IO a) -> Handle__ -> IO a
checkWritableHandle act h_@Handle__{..}
  = case haType of
      ClosedHandle         -> ioe_closedHandle
      SemiClosedHandle     -> ioe_closedHandle
      ReadHandle           -> ioe_notWritable
      ReadWriteHandle      -> do
        buf <- readIORef haCharBuffer
        when (not (isWriteBuffer buf)) $ do
           flushCharReadBuffer h_
           flushByteReadBuffer h_
           buf <- readIORef haCharBuffer
           writeIORef haCharBuffer buf{ bufState = WriteBuffer }
           buf <- readIORef haByteBuffer
           buf' <- Buffered.emptyWriteBuffer haDevice buf
           writeIORef haByteBuffer buf'
        act h_
      _other               -> act h_

-- ---------------------------------------------------------------------------
-- Wrapper for read operations.

wantReadableHandle :: String -> Handle -> (Handle__ -> IO (Handle__,a)) -> IO a
wantReadableHandle fun h act = withHandle fun h (checkReadableHandle act)

wantReadableHandle_ :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantReadableHandle_ fun h@(FileHandle  _ m)   act
  = wantReadableHandle' fun h m act
wantReadableHandle_ fun h@(DuplexHandle _ m _) act
  = wantReadableHandle' fun h m act
    -- we know it's not a WriteHandle or ReadWriteHandle, but we have to
    -- check for ClosedHandle/SemiClosedHandle. (#4808)

wantReadableHandle'
        :: String -> Handle -> MVar Handle__
        -> (Handle__ -> IO a) -> IO a
wantReadableHandle' fun h m act
  = withHandle_' fun h m (checkReadableHandle act)

checkReadableHandle :: (Handle__ -> IO a) -> Handle__ -> IO a
checkReadableHandle act h_@Handle__{..} =
    case haType of
      ClosedHandle         -> ioe_closedHandle
      SemiClosedHandle     -> ioe_closedHandle
      AppendHandle         -> ioe_notReadable
      WriteHandle          -> ioe_notReadable
      ReadWriteHandle      -> do
          -- a read/write handle and we want to read from it.  We must
          -- flush all buffered write data first.
          bbuf <- readIORef haByteBuffer
          when (isWriteBuffer bbuf) $ do
             when (not (isEmptyBuffer bbuf)) $ flushByteWriteBuffer h_
             cbuf' <- readIORef haCharBuffer
             writeIORef haCharBuffer cbuf'{ bufState = ReadBuffer }
             bbuf <- readIORef haByteBuffer
             writeIORef haByteBuffer bbuf{ bufState = ReadBuffer }
          act h_
      _other               -> act h_

-- ---------------------------------------------------------------------------
-- Wrapper for seek operations.

wantSeekableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantSeekableHandle fun h@(DuplexHandle _ _ _) _act =
  ioException (IOError (Just h) IllegalOperation fun
                   "handle is not seekable" Nothing Nothing)
wantSeekableHandle fun h@(FileHandle _ m) act =
  withHandle_' fun h m (checkSeekableHandle act)

checkSeekableHandle :: (Handle__ -> IO a) -> Handle__ -> IO a
checkSeekableHandle act handle_@Handle__{haDevice=dev} =
    case haType handle_ of
      ClosedHandle      -> ioe_closedHandle
      SemiClosedHandle  -> ioe_closedHandle
      AppendHandle      -> ioe_notSeekable
      _ -> do b <- IODevice.isSeekable dev
              if b then act handle_
                   else ioe_notSeekable

-- -----------------------------------------------------------------------------
-- Handy IOErrors

ioe_closedHandle, ioe_EOF,
  ioe_notReadable, ioe_notWritable, ioe_cannotFlushNotSeekable,
  ioe_notSeekable :: IO a

ioe_closedHandle = ioException
   (IOError Nothing IllegalOperation ""
        "handle is closed" Nothing Nothing)
ioe_EOF = ioException
   (IOError Nothing EOF "" "" Nothing Nothing)
ioe_notReadable = ioException
   (IOError Nothing IllegalOperation ""
        "handle is not open for reading" Nothing Nothing)
ioe_notWritable = ioException
   (IOError Nothing IllegalOperation ""
        "handle is not open for writing" Nothing Nothing)
ioe_notSeekable = ioException
   (IOError Nothing IllegalOperation ""
        "handle is not seekable" Nothing Nothing)
ioe_cannotFlushNotSeekable = ioException
   (IOError Nothing IllegalOperation ""
      "cannot flush the read buffer: underlying device is not seekable"
        Nothing Nothing)

ioe_finalizedHandle :: FilePath -> Handle__
ioe_finalizedHandle fp = throw
   (IOError Nothing IllegalOperation ""
        "handle is finalized" Nothing (Just fp))

ioe_bufsiz :: Int -> IO a
ioe_bufsiz n = ioException
   (IOError Nothing InvalidArgument "hSetBuffering"
        ("illegal buffer size " ++ showsPrec 9 n []) Nothing Nothing)
                                -- 9 => should be parens'ified.

-- ---------------------------------------------------------------------------
-- Wrapper for Handle encoding/decoding.

-- The interface for TextEncoding changed so that a TextEncoding doesn't raise
-- an exception if it encounters an invalid sequnce. Furthermore, encoding
-- returns a reason as to why encoding stopped, letting us know if it was due
-- to input/output underflow or an invalid sequence.
--
-- This code adapts this elaborated interface back to the original TextEncoding
-- interface.
--
-- FIXME: it is possible that Handle code using the haDecoder/haEncoder fields
-- could be made clearer by using the 'encode' interface directly. I have not
-- looked into this.

streamEncode :: BufferCodec from to state
             -> Buffer from -> Buffer to
             -> IO (Buffer from, Buffer to)
streamEncode codec from to = go (from, to)
  where 
    go (from, to) = do
      (why, from', to') <- encode codec from to
      -- When we are dealing with Handles, we don't care about input/output
      -- underflow particularly, and we want to delay errors about invalid
      -- sequences as far as possible.
      case why of
        Encoding.InvalidSequence | bufL from == bufL from' -> recover codec from' to' >>= go
        _ -> return (from', to')

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
--
-- [later; 8/2010] However, a program like this can yield a strange
-- error message:
--
--   main = writeFile "out" loop
--   loop = let x = x in x
--
-- because the main thread and the Handle are both unreachable at the
-- same time, the Handle may get finalized before the main thread
-- receives the NonTermination exception, and the exception handler
-- will then report an error.  We'd rather this was not an error and
-- the program just prints "<<loop>>".

handleFinalizer :: FilePath -> MVar Handle__ -> IO ()
handleFinalizer fp m = do
  handle_ <- takeMVar m
  (handle_', _) <- hClose_help handle_
  putMVar m handle_'
  return ()

-- ---------------------------------------------------------------------------
-- Allocating buffers

-- using an 8k char buffer instead of 32k improved performance for a
-- basic "cat" program by ~30% for me.  --SDM
dEFAULT_CHAR_BUFFER_SIZE :: Int
dEFAULT_CHAR_BUFFER_SIZE = 2048 -- 8k/sizeof(HsChar)

getCharBuffer :: IODevice dev => dev -> BufferState
              -> IO (IORef CharBuffer, BufferMode)
getCharBuffer dev state = do
  buffer <- newCharBuffer dEFAULT_CHAR_BUFFER_SIZE state
  ioref  <- newIORef buffer
  is_tty <- IODevice.isTerminal dev

  let buffer_mode 
         | is_tty    = LineBuffering 
         | otherwise = BlockBuffering Nothing

  return (ioref, buffer_mode)

mkUnBuffer :: BufferState -> IO (IORef CharBuffer, BufferMode)
mkUnBuffer state = do
  buffer <- newCharBuffer dEFAULT_CHAR_BUFFER_SIZE state
              --  See [note Buffer Sizing], GHC.IO.Handle.Types
  ref <- newIORef buffer
  return (ref, NoBuffering)

-- -----------------------------------------------------------------------------
-- Flushing buffers

-- | syncs the file with the buffer, including moving the
-- file pointer backwards in the case of a read buffer.  This can fail
-- on a non-seekable read Handle.
flushBuffer :: Handle__ -> IO ()
flushBuffer h_@Handle__{..} = do
  buf <- readIORef haCharBuffer
  case bufState buf of
    ReadBuffer  -> do
        flushCharReadBuffer h_
        flushByteReadBuffer h_
    WriteBuffer -> do
        flushByteWriteBuffer h_

-- | flushes the Char buffer only.  Works on all Handles.
flushCharBuffer :: Handle__ -> IO ()
flushCharBuffer h_@Handle__{..} = do
  cbuf <- readIORef haCharBuffer
  case bufState cbuf of
    ReadBuffer  -> do
        flushCharReadBuffer h_
    WriteBuffer ->
        when (not (isEmptyBuffer cbuf)) $
           error "internal IO library error: Char buffer non-empty"

-- -----------------------------------------------------------------------------
-- Writing data (flushing write buffers)

-- flushWriteBuffer flushes the buffer iff it contains pending write
-- data.  Flushes both the Char and the byte buffer, leaving both
-- empty.
flushWriteBuffer :: Handle__ -> IO ()
flushWriteBuffer h_@Handle__{..} = do
  buf <- readIORef haByteBuffer
  when (isWriteBuffer buf) $ flushByteWriteBuffer h_

flushByteWriteBuffer :: Handle__ -> IO ()
flushByteWriteBuffer h_@Handle__{..} = do
  bbuf <- readIORef haByteBuffer
  when (not (isEmptyBuffer bbuf)) $ do
    bbuf' <- Buffered.flushWriteBuffer haDevice bbuf
    writeIORef haByteBuffer bbuf'

-- write the contents of the CharBuffer to the Handle__.
-- The data will be encoded and pushed to the byte buffer,
-- flushing if the buffer becomes full.
writeCharBuffer :: Handle__ -> CharBuffer -> IO ()
writeCharBuffer h_@Handle__{..} !cbuf = do
  --
  bbuf <- readIORef haByteBuffer

  debugIO ("writeCharBuffer: cbuf=" ++ summaryBuffer cbuf ++
        " bbuf=" ++ summaryBuffer bbuf)

  (cbuf',bbuf') <- case haEncoder of
    Nothing      -> latin1_encode cbuf bbuf
    Just encoder -> (streamEncode encoder) cbuf bbuf

  debugIO ("writeCharBuffer after encoding: cbuf=" ++ summaryBuffer cbuf' ++
        " bbuf=" ++ summaryBuffer bbuf')

          -- flush if the write buffer is full
  if isFullBuffer bbuf'
          --  or we made no progress
     || not (isEmptyBuffer cbuf') && bufL cbuf' == bufL cbuf
          -- or the byte buffer has more elements than the user wanted buffered
     || (case haBufferMode of
          BlockBuffering (Just s) -> bufferElems bbuf' >= s
          NoBuffering -> True
          _other -> False)
    then do
      bbuf'' <- Buffered.flushWriteBuffer haDevice bbuf'
      writeIORef haByteBuffer bbuf''
    else
      writeIORef haByteBuffer bbuf'

  if not (isEmptyBuffer cbuf')
     then writeCharBuffer h_ cbuf'
     else return ()

-- -----------------------------------------------------------------------------
-- Flushing read buffers

-- It is always possible to flush the Char buffer back to the byte buffer.
flushCharReadBuffer :: Handle__ -> IO ()
flushCharReadBuffer Handle__{..} = do
  cbuf <- readIORef haCharBuffer
  if isWriteBuffer cbuf || isEmptyBuffer cbuf then return () else do

  -- haLastDecode is the byte buffer just before we did our last batch of
  -- decoding.  We're going to re-decode the bytes up to the current char,
  -- to find out where we should revert the byte buffer to.
  (codec_state, bbuf0) <- readIORef haLastDecode

  cbuf0 <- readIORef haCharBuffer
  writeIORef haCharBuffer cbuf0{ bufL=0, bufR=0 }

  -- if we haven't used any characters from the char buffer, then just
  -- re-install the old byte buffer.
  if bufL cbuf0 == 0
     then do writeIORef haByteBuffer bbuf0
             return ()
     else do

  case haDecoder of
    Nothing -> do
      writeIORef haByteBuffer bbuf0 { bufL = bufL bbuf0 + bufL cbuf0 }
      -- no decoder: the number of bytes to decode is the same as the
      -- number of chars we have used up.

    Just decoder -> do
      debugIO ("flushCharReadBuffer re-decode, bbuf=" ++ summaryBuffer bbuf0 ++
               " cbuf=" ++ summaryBuffer cbuf0)

      -- restore the codec state
      setState decoder codec_state
    
      (bbuf1,cbuf1) <- (streamEncode decoder) bbuf0
                               cbuf0{ bufL=0, bufR=0, bufSize = bufL cbuf0 }
    
      debugIO ("finished, bbuf=" ++ summaryBuffer bbuf1 ++
               " cbuf=" ++ summaryBuffer cbuf1)

      writeIORef haByteBuffer bbuf1


-- When flushing the byte read buffer, we seek backwards by the number
-- of characters in the buffer.  The file descriptor must therefore be
-- seekable: attempting to flush the read buffer on an unseekable
-- handle is not allowed.

flushByteReadBuffer :: Handle__ -> IO ()
flushByteReadBuffer h_@Handle__{..} = do
  bbuf <- readIORef haByteBuffer

  if isEmptyBuffer bbuf then return () else do

  seekable <- IODevice.isSeekable haDevice
  when (not seekable) $ ioe_cannotFlushNotSeekable

  let seek = negate (bufR bbuf - bufL bbuf)

  debugIO ("flushByteReadBuffer: new file offset = " ++ show seek)
  IODevice.seek haDevice RelativeSeek (fromIntegral seek)

  writeIORef haByteBuffer bbuf{ bufL=0, bufR=0 }

-- ----------------------------------------------------------------------------
-- Making Handles

mkHandle :: (IODevice dev, BufferedIO dev, Typeable dev) => dev
            -> FilePath
            -> HandleType
            -> Bool                     -- buffered?
            -> Maybe TextEncoding
            -> NewlineMode
            -> Maybe HandleFinalizer
            -> Maybe (MVar Handle__)
            -> IO Handle

mkHandle dev filepath ha_type buffered mb_codec nl finalizer other_side = do
   openTextEncoding mb_codec ha_type $ \ mb_encoder mb_decoder -> do

   let buf_state = initBufferState ha_type
   bbuf <- Buffered.newBuffer dev buf_state
   bbufref <- newIORef bbuf
   last_decode <- newIORef (error "codec_state", bbuf)

   (cbufref,bmode) <- 
         if buffered then getCharBuffer dev buf_state
                     else mkUnBuffer buf_state

   spares <- newIORef BufferListNil
   newFileHandle filepath finalizer
            (Handle__ { haDevice = dev,
                        haType = ha_type,
                        haBufferMode = bmode,
                        haByteBuffer = bbufref,
                        haLastDecode = last_decode,
                        haCharBuffer = cbufref,
                        haBuffers = spares,
                        haEncoder = mb_encoder,
                        haDecoder = mb_decoder,
                        haCodec = mb_codec,
                        haInputNL = inputNL nl,
                        haOutputNL = outputNL nl,
                        haOtherSide = other_side
                      })

-- | makes a new 'Handle'
mkFileHandle :: (IODevice dev, BufferedIO dev, Typeable dev)
             => dev -- ^ the underlying IO device, which must support 
                    -- 'IODevice', 'BufferedIO' and 'Typeable'
             -> FilePath
                    -- ^ a string describing the 'Handle', e.g. the file
                    -- path for a file.  Used in error messages.
             -> IOMode
                    -- The mode in which the 'Handle' is to be used
             -> Maybe TextEncoding
                    -- Create the 'Handle' with no text encoding?
             -> NewlineMode
                    -- Translate newlines?
             -> IO Handle
mkFileHandle dev filepath iomode mb_codec tr_newlines = do
   mkHandle dev filepath (ioModeToHandleType iomode) True{-buffered-} mb_codec
            tr_newlines
            (Just handleFinalizer) Nothing{-other_side-}

-- | like 'mkFileHandle', except that a 'Handle' is created with two
-- independent buffers, one for reading and one for writing.  Used for
-- full-duplex streams, such as network sockets.
mkDuplexHandle :: (IODevice dev, BufferedIO dev, Typeable dev) => dev
               -> FilePath -> Maybe TextEncoding -> NewlineMode -> IO Handle
mkDuplexHandle dev filepath mb_codec tr_newlines = do

  write_side@(FileHandle _ write_m) <- 
       mkHandle dev filepath WriteHandle True mb_codec
                        tr_newlines
                        (Just handleFinalizer)
                        Nothing -- no othersie

  read_side@(FileHandle _ read_m) <- 
      mkHandle dev filepath ReadHandle True mb_codec
                        tr_newlines
                        Nothing -- no finalizer
                        (Just write_m)

  return (DuplexHandle filepath read_m write_m)

ioModeToHandleType :: IOMode -> HandleType
ioModeToHandleType ReadMode      = ReadHandle
ioModeToHandleType WriteMode     = WriteHandle
ioModeToHandleType ReadWriteMode = ReadWriteHandle
ioModeToHandleType AppendMode    = AppendHandle

initBufferState :: HandleType -> BufferState
initBufferState ReadHandle = ReadBuffer
initBufferState _          = WriteBuffer

openTextEncoding
   :: Maybe TextEncoding
   -> HandleType
   -> (forall es ds . Maybe (TextEncoder es) -> Maybe (TextDecoder ds) -> IO a)
   -> IO a

openTextEncoding Nothing   ha_type cont = cont Nothing Nothing
openTextEncoding (Just TextEncoding{..}) ha_type cont = do
    mb_decoder <- if isReadableHandleType ha_type then do
                     decoder <- mkTextDecoder
                     return (Just decoder)
                  else
                     return Nothing
    mb_encoder <- if isWritableHandleType ha_type then do
                     encoder <- mkTextEncoder
                     return (Just encoder)
                  else 
                     return Nothing
    cont mb_encoder mb_decoder

closeTextCodecs :: Handle__ -> IO ()
closeTextCodecs Handle__{..} = do
  case haDecoder of Nothing -> return (); Just d -> Encoding.close d
  case haEncoder of Nothing -> return (); Just d -> Encoding.close d

-- ---------------------------------------------------------------------------
-- closing Handles

-- hClose_help is also called by lazyRead (in GHC.IO.Handle.Text) when
-- EOF is read or an IO error occurs on a lazy stream.  The
-- semi-closed Handle is then closed immediately.  We have to be
-- careful with DuplexHandles though: we have to leave the closing to
-- the finalizer in that case, because the write side may still be in
-- use.
hClose_help :: Handle__ -> IO (Handle__, Maybe SomeException)
hClose_help handle_ =
  case haType handle_ of 
      ClosedHandle -> return (handle_,Nothing)
      _ -> do mb_exc1 <- trymaybe $ flushWriteBuffer handle_ -- interruptible
                    -- it is important that hClose doesn't fail and
                    -- leave the Handle open (#3128), so we catch
                    -- exceptions when flushing the buffer.
              (h_, mb_exc2) <- hClose_handle_ handle_
              return (h_, if isJust mb_exc1 then mb_exc1 else mb_exc2)


trymaybe :: IO () -> IO (Maybe SomeException)
trymaybe io = (do io; return Nothing) `catchException` \e -> return (Just e)

hClose_handle_ :: Handle__ -> IO (Handle__, Maybe SomeException)
hClose_handle_ h_@Handle__{..} = do

    -- close the file descriptor, but not when this is the read
    -- side of a duplex handle.
    -- If an exception is raised by the close(), we want to continue
    -- to close the handle and release the lock if it has one, then 
    -- we return the exception to the caller of hClose_help which can
    -- raise it if necessary.
    maybe_exception <- 
      case haOtherSide of
        Nothing -> trymaybe $ IODevice.close haDevice
        Just _  -> return Nothing

    -- free the spare buffers
    writeIORef haBuffers BufferListNil
    writeIORef haCharBuffer noCharBuffer
    writeIORef haByteBuffer noByteBuffer
  
    -- release our encoder/decoder
    closeTextCodecs h_

    -- we must set the fd to -1, because the finalizer is going
    -- to run eventually and try to close/unlock it.
    -- ToDo: necessary?  the handle will be marked ClosedHandle
    -- XXX GHC won't let us use record update here, hence wildcards
    return (Handle__{ haType = ClosedHandle, .. }, maybe_exception)

{-# NOINLINE noCharBuffer #-}
noCharBuffer :: CharBuffer
noCharBuffer = unsafePerformIO $ newCharBuffer 1 ReadBuffer

{-# NOINLINE noByteBuffer #-}
noByteBuffer :: Buffer Word8
noByteBuffer = unsafePerformIO $ newByteBuffer 1 ReadBuffer

-- ---------------------------------------------------------------------------
-- Looking ahead

hLookAhead_ :: Handle__ -> IO Char
hLookAhead_ handle_@Handle__{..} = do
    buf <- readIORef haCharBuffer
  
    -- fill up the read buffer if necessary
    new_buf <- if isEmptyBuffer buf
                  then readTextDevice handle_ buf
                  else return buf
    writeIORef haCharBuffer new_buf
  
    peekCharBuf (bufRaw buf) (bufL buf)

-- ---------------------------------------------------------------------------
-- debugging

debugIO :: String -> IO ()
debugIO s
 | c_DEBUG_DUMP
    = do _ <- withCStringLen (s ++ "\n") $
                  \(p, len) -> c_write 1 (castPtr p) (fromIntegral len)
         return ()
 | otherwise = return ()

-- ----------------------------------------------------------------------------
-- Text input/output

-- Read characters into the provided buffer.  Return when any
-- characters are available; raise an exception if the end of 
-- file is reached.
readTextDevice :: Handle__ -> CharBuffer -> IO CharBuffer
readTextDevice h_@Handle__{..} cbuf = do
  --
  bbuf0 <- readIORef haByteBuffer

  debugIO ("readTextDevice: cbuf=" ++ summaryBuffer cbuf ++ 
        " bbuf=" ++ summaryBuffer bbuf0)

  bbuf1 <- if not (isEmptyBuffer bbuf0)
              then return bbuf0
              else do
                   (r,bbuf1) <- Buffered.fillReadBuffer haDevice bbuf0
                   if r == 0 then ioe_EOF else do  -- raise EOF
                   return bbuf1

  debugIO ("readTextDevice after reading: bbuf=" ++ summaryBuffer bbuf1)

  (bbuf2,cbuf') <- 
      case haDecoder of
          Nothing      -> do
               writeIORef haLastDecode (error "codec_state", bbuf1)
               latin1_decode bbuf1 cbuf
          Just decoder -> do
               state <- getState decoder
               writeIORef haLastDecode (state, bbuf1)
               (streamEncode decoder) bbuf1 cbuf

  debugIO ("readTextDevice after decoding: cbuf=" ++ summaryBuffer cbuf' ++ 
        " bbuf=" ++ summaryBuffer bbuf2)

  writeIORef haByteBuffer bbuf2
  if bufR cbuf' == bufR cbuf -- no new characters
     then readTextDevice' h_ bbuf2 cbuf -- we need more bytes to make a Char
     else return cbuf'

-- we have an incomplete byte sequence at the end of the buffer: try to
-- read more bytes.
readTextDevice' :: Handle__ -> Buffer Word8 -> CharBuffer -> IO CharBuffer
readTextDevice' h_@Handle__{..} bbuf0 cbuf0 = do
  --
  -- copy the partial sequence to the beginning of the buffer, so we have
  -- room to read more bytes.
  bbuf1 <- slideContents bbuf0

  -- readTextDevice only calls us if we got some bytes but not some characters.
  -- This can't occur if haDecoder is Nothing because latin1_decode accepts all bytes.
  let Just decoder = haDecoder
  
  (r,bbuf2) <- Buffered.fillReadBuffer haDevice bbuf1
  if r == 0
   then do
     (bbuf3, cbuf1) <- recover decoder bbuf2 cbuf0
     writeIORef haByteBuffer bbuf3
     -- We should recursively invoke readTextDevice after recovery,
     -- if recovery did not add at least one new character to the buffer:
     --  1. If we were using IgnoreCodingFailure it might be the case that
     --     cbuf1 is the same length as cbuf0 and we need to raise ioe_EOF
     --  2. If we were using TransliterateCodingFailure we might have *mutated*
     --     the byte buffer without changing the pointers into either buffer.
     --     We need to try and decode it again - it might just go through this time.
     if bufR cbuf1 == bufR cbuf0
      then readTextDevice h_ cbuf1
      else return cbuf1
   else do
    debugIO ("readTextDevice' after reading: bbuf=" ++ summaryBuffer bbuf2)
  
    (bbuf3,cbuf1) <- do
       state <- getState decoder
       writeIORef haLastDecode (state, bbuf2)
       (streamEncode decoder) bbuf2 cbuf0
  
    debugIO ("readTextDevice' after decoding: cbuf=" ++ summaryBuffer cbuf1 ++ 
          " bbuf=" ++ summaryBuffer bbuf3)
  
    writeIORef haByteBuffer bbuf3
    if bufR cbuf0 == bufR cbuf1
       then readTextDevice' h_ bbuf3 cbuf1
       else return cbuf1

-- Read characters into the provided buffer.  Do not block;
-- return zero characters instead.  Raises an exception on end-of-file.
readTextDeviceNonBlocking :: Handle__ -> CharBuffer -> IO CharBuffer
readTextDeviceNonBlocking h_@Handle__{..} cbuf = do
  --
  bbuf0 <- readIORef haByteBuffer
  when (isEmptyBuffer bbuf0) $ do
     (r,bbuf1) <- Buffered.fillReadBuffer0 haDevice bbuf0
     if isNothing r then ioe_EOF else do  -- raise EOF
     writeIORef haByteBuffer bbuf1

  decodeByteBuf h_ cbuf

-- Decode bytes from the byte buffer into the supplied CharBuffer.
decodeByteBuf :: Handle__ -> CharBuffer -> IO CharBuffer
decodeByteBuf h_@Handle__{..} cbuf = do
  --
  bbuf0 <- readIORef haByteBuffer

  (bbuf2,cbuf') <-
      case haDecoder of
          Nothing      -> do
               writeIORef haLastDecode (error "codec_state", bbuf0)
               latin1_decode bbuf0 cbuf
          Just decoder -> do
               state <- getState decoder
               writeIORef haLastDecode (state, bbuf0)
               (streamEncode decoder) bbuf0 cbuf

  writeIORef haByteBuffer bbuf2
  return cbuf'


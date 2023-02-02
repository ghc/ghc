{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Handle.FD
-- Copyright   :  (c) The University of Glasgow, 1994-2008
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Handle operations implemented by file descriptors (FDs)
--
-----------------------------------------------------------------------------

module GHC.IO.Handle.FD ( 
  stdin, stdout, stderr,
  openFile, withFile,
  openBinaryFile, withBinaryFile,
  openFileBlocking, withFileBlocking,
  mkHandleFromFD, fdToHandle, fdToHandle', handleToFd
 ) where

import GHC.Base
import GHC.Show
import Data.Maybe
import Data.Typeable
import Foreign.C.Types
import GHC.MVar
import GHC.IO
import GHC.IO.Encoding
import GHC.IO.Device as IODevice
import GHC.IO.Exception
import GHC.IO.IOMode
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import qualified GHC.IO.FD as FD
import qualified System.Posix.Internals as Posix

-- ---------------------------------------------------------------------------
-- Standard Handles

-- Three handles are allocated during program initialisation.  The first
-- two manage input or output from the Haskell program's standard input
-- or output channel respectively.  The third manages output to the
-- standard error channel. These handles are initially open.

-- | A handle managing input from the Haskell program's standard input channel.
stdin :: Handle
{-# NOINLINE stdin #-}
stdin = unsafePerformIO $ do
   -- ToDo: acquire lock
   setBinaryMode FD.stdin
   enc <- getLocaleEncoding
   mkHandle FD.stdin "<stdin>" ReadHandle True (Just enc)
                nativeNewlineMode{-translate newlines-}
                (Just stdHandleFinalizer) Nothing

-- | A handle managing output to the Haskell program's standard output channel.
stdout :: Handle
{-# NOINLINE stdout #-}
stdout = unsafePerformIO $ do
   -- ToDo: acquire lock
   setBinaryMode FD.stdout
   enc <- getLocaleEncoding
   mkHandle FD.stdout "<stdout>" WriteHandle True (Just enc)
                nativeNewlineMode{-translate newlines-}
                (Just stdHandleFinalizer) Nothing

-- | A handle managing output to the Haskell program's standard error channel.
stderr :: Handle
{-# NOINLINE stderr #-}
stderr = unsafePerformIO $ do
    -- ToDo: acquire lock
   setBinaryMode FD.stderr
   enc <- getLocaleEncoding
   mkHandle FD.stderr "<stderr>" WriteHandle False{-stderr is unbuffered-} 
                (Just enc)
                nativeNewlineMode{-translate newlines-}
                (Just stdHandleFinalizer) Nothing

stdHandleFinalizer :: FilePath -> MVar Handle__ -> IO ()
stdHandleFinalizer fp m = do
  h_ <- takeMVar m
  flushWriteBuffer h_
  case haType h_ of 
      ClosedHandle -> return ()
      _other       -> closeTextCodecs h_
  putMVar m (ioe_finalizedHandle fp)

-- We have to put the FDs into binary mode on Windows to avoid the newline
-- translation that the CRT IO library does.
setBinaryMode :: FD.FD -> IO ()
#if defined(mingw32_HOST_OS)
setBinaryMode fd = do _ <- setmode (FD.fdFD fd) True
                      return ()
#else
setBinaryMode _ = return ()
#endif

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "__hscore_setmode"
  setmode :: CInt -> Bool -> IO CInt
#endif

-- ---------------------------------------------------------------------------
-- Opening and Closing Files

addFilePathToIOError :: String -> FilePath -> IOException -> IOException
addFilePathToIOError fun fp ioe
  = ioe{ ioe_location = fun, ioe_filename = Just fp }

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
--  * 'System.IO.Error.isAlreadyInUseError' if the file is already open and
--    cannot be reopened;
--
--  * 'System.IO.Error.isDoesNotExistError' if the file does not exist or
--    (on POSIX systems) is a FIFO without a reader and 'WriteMode' was
--    requested; or
--
--  * 'System.IO.Error.isPermissionError' if the user does not have permission
--     to open the file.
--
-- On POSIX systems, 'openFile' is an /interruptible operation/ as
-- described in "Control.Exception".
--
-- Note: if you will be working with files containing binary data, you'll want to
-- be using 'openBinaryFile'.
openFile :: FilePath -> IOMode -> IO Handle
openFile fp im = 
  catchException
    (openFile' fp im dEFAULT_OPEN_IN_BINARY_MODE True)
    (\e -> ioError (addFilePathToIOError "openFile" fp e))

-- | @'withFile' name mode act@ opens a file like 'openFile' and passes
-- the resulting handle to the computation @act@.  The handle will be
-- closed on exit from 'withFile', whether by normal termination or by
-- raising an exception.  If closing the handle raises an exception, then
-- this exception will be raised by 'withFile' rather than any exception
-- raised by @act@.
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile fp im act =
  catchException
    (withFile' fp im dEFAULT_OPEN_IN_BINARY_MODE True act)
    (\e -> ioError (addFilePathToIOError "withFile" fp e))

-- | Like 'openFile', but opens the file in ordinary blocking mode.
-- This can be useful for opening a FIFO for writing: if we open in
-- non-blocking mode then the open will fail if there are no readers,
-- whereas a blocking open will block until a reader appear.
--
-- Note: when blocking happens, an OS thread becomes tied up with the
-- processing, so the program must have at least another OS thread if
-- it wants to unblock itself. By corollary, a non-threaded runtime
-- will need a process-external trigger in order to become unblocked.
--
-- On POSIX systems, 'openFileBlocking' is an /interruptible operation/ as
-- described in "Control.Exception".
--
-- @since 4.4.0.0
openFileBlocking :: FilePath -> IOMode -> IO Handle
openFileBlocking fp im =
  catchException
    (openFile' fp im dEFAULT_OPEN_IN_BINARY_MODE False)
    (\e -> ioError (addFilePathToIOError "openFileBlocking" fp e))

-- | @'withFileBlocking' name mode act@ opens a file like 'openFileBlocking'
-- and passes the resulting handle to the computation @act@.  The handle will
-- be closed on exit from 'withFileBlocking', whether by normal termination or
-- by raising an exception.  If closing the handle raises an exception, then
-- this exception will be raised by 'withFile' rather than any exception raised
-- by @act@.
withFileBlocking :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileBlocking fp im act =
  catchException
    (withFile' fp im dEFAULT_OPEN_IN_BINARY_MODE False act)
    (\e -> ioError (addFilePathToIOError "withFileBlocking" fp e))

-- | Like 'openFile', but open the file in binary mode.
-- On Windows, reading a file in text mode (which is the default)
-- will translate CRLF to LF, and writing will translate LF to CRLF.
-- This is usually what you want with text files.  With binary files
-- this is undesirable; also, as usual under Microsoft operating systems,
-- text mode treats control-Z as EOF.  Binary mode turns off all special
-- treatment of end-of-line and end-of-file characters.
-- (See also 'System.IO.hSetBinaryMode'.)

-- On POSIX systems, 'openBinaryFile' is an /interruptible operation/ as
-- described in "Control.Exception".
openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile fp m =
  catchException
    (openFile' fp m True True)
    (\e -> ioError (addFilePathToIOError "openBinaryFile" fp e))

-- | A version of `openBinaryFile` that takes an action to perform
-- with the handle. If an exception occurs in the action, then
-- the file will be closed automatically. The action /should/
-- close the file when finished with it so the file does not remain
-- open until the garbage collector collects the handle.
withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile fp im act =
  catchException
    (withFile' fp im True True act)
    (\e -> ioError (addFilePathToIOError "withBinaryFile" fp e))

-- | Open a file and perform an action with it. If the action throws an
-- exception, then the file will be closed. If the last argument is 'True',
-- then the file will be closed on successful completion as well. We use this to
-- implement both the `withFile` family of functions (via `withFile'`) and the
-- `openFile` family (via `openFile'`).
withOpenFile' :: String -> IOMode -> Bool -> Bool -> (Handle -> IO r) -> Bool -> IO r
withOpenFile' filepath iomode binary non_blocking act close_finally =
  -- first open the file to get an FD
  FD.openFileWith filepath iomode non_blocking (\fd fd_type -> do

      mb_codec <- if binary then return Nothing else fmap Just getLocaleEncoding

      -- Then use it to make a Handle. If this fails, openFileWith
      -- will take care of closing the file.
      mkHandleFromFDNoFinalizer fd fd_type filepath iomode
                       False {- do not *set* non-blocking mode -}
                       mb_codec)

    -- Add a finalizer to the handle. This is done under a mask,
    -- so there are no asynchronous exceptions, and (satisfying
    -- the conditions of openFileWith), addHandleFinalizer
    -- cannot throw a synchronous exception.
    (\restore hndl -> do
        addHandleFinalizer hndl handleFinalizer
        r <- restore (act hndl) `onException` hClose_impl hndl
        when close_finally $ hClose_impl hndl
        pure r
        )

        -- ASSERT: if we just created the file, then fdToHandle' won't fail
        -- (so we don't need to worry about removing the newly created file
        --  in the event of an error).

-- | Open a file and perform an action with it. When the action
-- completes or throws/receives an exception, the file will be closed.
withFile' :: String -> IOMode -> Bool -> Bool -> (Handle -> IO r) -> IO r
withFile' filepath iomode binary non_blocking act =
  withOpenFile' filepath iomode binary non_blocking act True

openFile' :: String -> IOMode -> Bool -> Bool -> IO Handle
openFile' filepath iomode binary non_blocking =
  withOpenFile' filepath iomode binary non_blocking pure False

-- ---------------------------------------------------------------------------
-- Converting file descriptors from/to Handles

mkHandleFromFDNoFinalizer
   :: FD.FD
   -> IODeviceType
   -> FilePath  -- a string describing this file descriptor (e.g. the filename)
   -> IOMode
   -> Bool      --  *set* non-blocking mode on the FD
   -> Maybe TextEncoding
   -> IO Handle

mkHandleFromFDNoFinalizer fd0 fd_type filepath iomode set_non_blocking mb_codec
  = do
#if !defined(mingw32_HOST_OS)
    -- turn on non-blocking mode
    fd <- if set_non_blocking 
             then FD.setNonBlockingMode fd0 True
             else return fd0
#else
    let _ = set_non_blocking -- warning suppression
    fd <- return fd0
#endif

    let nl | isJust mb_codec = nativeNewlineMode
           | otherwise       = noNewlineTranslation

    case fd_type of
        Directory -> 
           ioException (IOError Nothing InappropriateType "openFile"
                           "is a directory" Nothing Nothing)

        Stream
           -- only *Streams* can be DuplexHandles.  Other read/write
           -- Handles must share a buffer.
           | ReadWriteMode <- iomode -> 
                mkDuplexHandleNoFinalizer fd filepath mb_codec nl

        _other -> 
           mkFileHandleNoFinalizer fd filepath iomode mb_codec nl

mkHandleFromFD
   :: FD.FD
   -> IODeviceType
   -> FilePath  -- a string describing this file descriptor (e.g. the filename)
   -> IOMode
   -> Bool      --  *set* non-blocking mode on the FD
   -> Maybe TextEncoding
   -> IO Handle
mkHandleFromFD fd0 fd_type filepath iomode set_non_blocking mb_codec = do
  h <- mkHandleFromFDNoFinalizer fd0 fd_type filepath iomode
                                 set_non_blocking mb_codec
  addHandleFinalizer h handleFinalizer
  pure h

-- | Old API kept to avoid breaking clients
fdToHandle' :: CInt
            -> Maybe IODeviceType
            -> Bool -- is_socket on Win, non-blocking on Unix
            -> FilePath
            -> IOMode
            -> Bool -- binary
            -> IO Handle
fdToHandle' fdint mb_type is_socket filepath iomode binary = do
  let mb_stat = case mb_type of
                        Nothing          -> Nothing
                          -- mkFD will do the stat:
                        Just RegularFile -> Nothing
                          -- no stat required for streams etc.:
                        Just other       -> Just (other,0,0)
  (fd,fd_type) <- FD.mkFD fdint iomode mb_stat
                       is_socket
                       is_socket
  enc <- if binary then return Nothing else fmap Just getLocaleEncoding
  mkHandleFromFD fd fd_type filepath iomode is_socket enc


-- | Turn an existing file descriptor into a Handle.  This is used by
-- various external libraries to make Handles.
--
-- Makes a binary Handle.  This is for historical reasons; it should
-- probably be a text Handle with the default encoding and newline
-- translation instead.
fdToHandle :: Posix.FD -> IO Handle
fdToHandle fdint = do
   iomode <- Posix.fdGetMode fdint
   (fd,fd_type) <- FD.mkFD fdint iomode Nothing
            False{-is_socket-} 
              -- NB. the is_socket flag is False, meaning that:
              --  on Windows we're guessing this is not a socket (XXX)
            False{-is_nonblock-}
              -- file descriptors that we get from external sources are
              -- not put into non-blocking mode, because that would affect
              -- other users of the file descriptor
   let fd_str = "<file descriptor: " ++ show fd ++ ">"
   mkHandleFromFD fd fd_type fd_str iomode False{-non-block-} 
                  Nothing -- bin mode

-- | Turn an existing Handle into a file descriptor. This function throws an
-- IOError if the Handle does not reference a file descriptor.
handleToFd :: Handle -> IO FD.FD
handleToFd h = case h of
  FileHandle _ mv -> do
    Handle__{haDevice = dev} <- readMVar mv
    case cast dev of
      Just fd -> return fd
      Nothing -> throwErr "not a file descriptor"
  DuplexHandle{} -> throwErr "not a file handle"
  where
    throwErr msg = ioException $ IOError (Just h)
      InappropriateType "handleToFd" msg Nothing Nothing


-- ---------------------------------------------------------------------------
-- Are files opened by default in text or binary mode, if the user doesn't
-- specify?

dEFAULT_OPEN_IN_BINARY_MODE :: Bool
dEFAULT_OPEN_IN_BINARY_MODE = False

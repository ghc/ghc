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
  openFile, openBinaryFile, openFileBlocking,
  mkHandleFromFD, fdToHandle, fdToHandle',
  isEOF
 ) where

import GHC.Base
import GHC.Show
import Data.Maybe
import Foreign.C.Types
import GHC.MVar
import GHC.IO
import GHC.IO.Encoding
import GHC.IO.Device as IODevice
import GHC.IO.Exception
import GHC.IO.IOMode
import GHC.IO.Handle
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
#ifdef mingw32_HOST_OS
setBinaryMode fd = do _ <- setmode (FD.fdFD fd) True
                      return ()
#else
setBinaryMode _ = return ()
#endif

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "__hscore_setmode"
  setmode :: CInt -> Bool -> IO CInt
#endif

-- ---------------------------------------------------------------------------
-- isEOF

-- | The computation 'isEOF' is identical to 'hIsEOF',
-- except that it works only on 'stdin'.

isEOF :: IO Bool
isEOF = hIsEOF stdin

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
  catchException
    (openFile' fp im dEFAULT_OPEN_IN_BINARY_MODE True)
    (\e -> ioError (addFilePathToIOError "openFile" fp e))

-- | Like 'openFile', but opens the file in ordinary blocking mode.
-- This can be useful for opening a FIFO for writing: if we open in
-- non-blocking mode then the open will fail if there are no readers,
-- whereas a blocking open will block until a reader appear.
--
-- @since 4.4.0.0
openFileBlocking :: FilePath -> IOMode -> IO Handle
openFileBlocking fp im =
  catchException
    (openFile' fp im dEFAULT_OPEN_IN_BINARY_MODE False)
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
  catchException
    (openFile' fp m True True)
    (\e -> ioError (addFilePathToIOError "openBinaryFile" fp e))

openFile' :: String -> IOMode -> Bool -> Bool -> IO Handle
openFile' filepath iomode binary non_blocking = do
  -- first open the file to get an FD
  (fd, fd_type) <- FD.openFile filepath iomode non_blocking

  mb_codec <- if binary then return Nothing else fmap Just getLocaleEncoding

  -- then use it to make a Handle
  mkHandleFromFD fd fd_type filepath iomode
                   False {- do not *set* non-blocking mode -}
                   mb_codec
            `onException` IODevice.close fd
        -- NB. don't forget to close the FD if mkHandleFromFD fails, otherwise
        -- this FD leaks.
        -- ASSERT: if we just created the file, then fdToHandle' won't fail
        -- (so we don't need to worry about removing the newly created file
        --  in the event of an error).


-- ---------------------------------------------------------------------------
-- Converting file descriptors to Handles

mkHandleFromFD
   :: FD.FD
   -> IODeviceType
   -> FilePath  -- a string describing this file descriptor (e.g. the filename)
   -> IOMode
   -> Bool      --  *set* non-blocking mode on the FD
   -> Maybe TextEncoding
   -> IO Handle

mkHandleFromFD fd0 fd_type filepath iomode set_non_blocking mb_codec
  = do
#ifndef mingw32_HOST_OS
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
                mkDuplexHandle fd filepath mb_codec nl
                   

        _other -> 
           mkFileHandle fd filepath iomode mb_codec nl

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

-- ---------------------------------------------------------------------------
-- Are files opened by default in text or binary mode, if the user doesn't
-- specify?

dEFAULT_OPEN_IN_BINARY_MODE :: Bool
dEFAULT_OPEN_IN_BINARY_MODE = False

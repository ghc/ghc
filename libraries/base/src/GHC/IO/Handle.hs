{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

-- |
--
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

module GHC.IO.Handle
    (-- * Portable operations
     Handle,
     BufferMode(..),
     mkFileHandle,
     mkDuplexHandle,
     hFileSize,
     hSetFileSize,
     hIsEOF,
     isEOF,
     hLookAhead,
     hSetBuffering,
     hSetBinaryMode,
     hSetEncoding,
     hGetEncoding,
     hFlush,
     hFlushAll,
     hDuplicate,
     hDuplicateTo,
     hClose,
     hClose_help,
     LockMode(..),
     hLock,
     hTryLock,
     HandlePosition,
     HandlePosn(..),
     hGetPosn,
     hSetPosn,
     SeekMode(..),
     hSeek,
     hTell,
     hIsOpen,
     hIsClosed,
     hIsReadable,
     hIsWritable,
     hGetBuffering,
     hIsSeekable,
     hSetEcho,
     hGetEcho,
     hIsTerminalDevice,
     hSetNewlineMode,
     Newline(..),
     NewlineMode(..),
     nativeNewline,
     noNewlineTranslation,
     universalNewlineMode,
     nativeNewlineMode,
     hShow,
     hWaitForInput,
     hGetChar,
     hGetLine,
     hGetContents,
     hGetContents',
     hPutChar,
     hPutStr,
     hGetBuf,
     hGetBufNonBlocking,
     hPutBuf,
     hPutBufNonBlocking,

     -- * Obtaining file descriptors and Windows handles
     withReadingFileDescriptor,
     withWritingFileDescriptor,
     withReadingWindowsHandle,
     withWritingWindowsHandle

     -- ** Caveats
     -- $with-ref-caveats
) where

import GHC.Internal.IO.Handle

import GHC.Internal.Control.Monad (return)
import GHC.Internal.Control.Exception (mask)
import GHC.Internal.Data.Function ((.), ($))
import GHC.Internal.Data.Functor (fmap)
#if defined(mingw32_HOST_OS)
import GHC.Internal.Data.Bool (otherwise)
#endif
import GHC.Internal.Data.Maybe (Maybe (Nothing), maybe)
#if defined(mingw32_HOST_OS)
import GHC.Internal.Data.Maybe (Maybe (Just))
#endif
import GHC.Internal.Data.List ((++))
import GHC.Internal.Data.String (String)
import GHC.Internal.Data.Typeable (Typeable, cast)
import GHC.Internal.System.IO (IO)
import GHC.Internal.IO.FD (fdFD)
#if defined(mingw32_HOST_OS)
import GHC.Internal.IO.Windows.Handle
       (
           NativeHandle,
           ConsoleHandle,
           IoHandle,
           toHANDLE
       )
#endif
import GHC.Internal.IO.Handle.Types (Handle__)
import GHC.Internal.IO.Handle.Internals
       (
           wantReadableHandle_,
           wantWritableHandle,
           flushBuffer
       )
import GHC.Internal.IO.Exception
       (
           IOErrorType (IllegalOperation),
           IOException (IOError),
           ioException
       )
import GHC.Internal.Foreign.Ptr (Ptr)
import GHC.Internal.Foreign.C.Types (CInt)

-- * Obtaining file descriptors and Windows handles

{-|
    Obtains from a handle an underlying operating-system reference for reading
    or writing and executes a user-provided action on it. The Haskell-side
    buffers of the handle are flushed before this action is run. While this
    action is executed, further operations on the handle are blocked to a degree
    that interference with this action is prevented.

    See [below](#with-ref-caveats) for caveats regarding this operation.
-}
withRef :: (Handle -> (Handle__ -> IO a) -> IO a)
           -- ^ Obtaining of an appropriately prepared handle side from a handle
        -> (forall d. Typeable d => d -> IO r)
           -- ^ Conversion of a device into an operating-system reference
        -> Handle
           -- ^ The handle to use
        -> (r -> IO a)
           -- ^ The action to execute on the operating-system reference
        -> IO a
withRef withHandleSide getRef handle act
    = mask $ \ withOriginalMaskingState ->
      withHandleSide handle $ \ handleSide -> do
          ref <- getRef handleSide
          flushBuffer handleSide
          withOriginalMaskingState $ act ref
{-
    The public operations that use 'withRef' provide 'withHandleSide' arguments
    that perform masking. Still, we have to use 'mask' here, in order do obtain
    the operation that restores the original masking state. The user-provided
    action should be executed with this original masking state, as there is no
    inherent reason to generally perform it with masking in place. The masking
    that the 'withHandleSide' arguments perform is only for safely accessing
    internal handle data and thus constitutes an implementation detail; it has
    nothing to do with the user-provided action.
-}
{-
    The order of actions in 'withRef' is such that any exception from 'getRef'
    is thrown before the flushing of the Haskell-side buffers.
-}

{-|
    Yields the result of another operation if that operation succeeded, and
    otherwise throws an exception that signals that the other operation failed
    because a certain I/O subsystem is not in use.
-}
requiringSubsystem :: String
                      -- ^ The name of the required subsystem
                   -> Maybe a
                      -- ^ The result of the other operation if it succeeded
                   -> IO a
requiringSubsystem subsystemName
    = maybe (ioException subsystemRequired) return
    where

    subsystemRequired :: IOException
    subsystemRequired = IOError Nothing
                                IllegalOperation
                                ""
                                (subsystemName ++ " I/O subsystem required")
                                Nothing
                                Nothing

{-|
    Obtains the POSIX file descriptor of a device if the POSIX I/O subsystem is
    in use, and throws an exception otherwise.
-}
getFileDescriptor :: Typeable d => d -> IO CInt
getFileDescriptor = requiringSubsystem "POSIX" . fmap fdFD . cast

{-|
    Obtains the Windows handle of a device if the Windows I/O subsystem is in
    use, and throws an exception otherwise.
-}
getWindowsHandle :: Typeable d => d -> IO (Ptr ())
getWindowsHandle = requiringSubsystem "native" . toMaybeWindowsHandle where

    toMaybeWindowsHandle :: Typeable d => d -> Maybe (Ptr ())
#if defined(mingw32_HOST_OS)
    toMaybeWindowsHandle dev
        | Just nativeHandle <- cast dev :: Maybe (IoHandle NativeHandle)
            = Just (toHANDLE nativeHandle)
        | Just consoleHandle <- cast dev :: Maybe (IoHandle ConsoleHandle)
            = Just (toHANDLE consoleHandle)
        | otherwise
            = Nothing
    {-
        This is inspired by the implementation of
        'System.Win32.Types.withHandleToHANDLENative'.
    -}
#else
    toMaybeWindowsHandle _ = Nothing
#endif

{-|
    Obtains from a handle a POSIX file descriptor for reading and executes a
    user-provided action on it. The Haskell-side buffers of the handle are
    flushed before this action is run. While this action is executed, further
    operations on the handle are blocked to a degree that interference with this
    action is prevented.

    If the I/O subsystem in use is not the POSIX one, an exception is thrown.

    See [below](#with-ref-caveats) for caveats regarding this operation.
-}
withReadingFileDescriptor :: Handle -> (CInt -> IO a) -> IO a
withReadingFileDescriptor
    = withRef (wantReadableHandle_ "withReadingFileDescriptor")
              getFileDescriptor

{-|
    Obtains from a handle a POSIX file descriptor for writing and executes a
    user-provided action on it. The Haskell-side buffers of the handle are
    flushed before this action is run. While this action is executed, further
    operations on the handle are blocked to a degree that interference with this
    action is prevented.

    If the I/O subsystem in use is not the POSIX one, an exception is thrown.

    See [below](#with-ref-caveats) for caveats regarding this operation.
-}
withWritingFileDescriptor :: Handle -> (CInt -> IO a) -> IO a
withWritingFileDescriptor
    = withRef (wantWritableHandle "withWritingFileDescriptor")
              getFileDescriptor

{-|
    Obtains from a Haskell handle a Windows handle for reading and executes a
    user-provided action on it. The Haskell-side buffers of the Haskell handle
    are flushed before this action is run. While this action is executed,
    further operations on the handle are blocked to a degree that interference
    with this action is prevented.

    If the I/O subsystem in use is not the Windows one, an exception is thrown.

    See [below](#with-ref-caveats) for caveats regarding this operation.
-}
withReadingWindowsHandle :: Handle -> (Ptr () -> IO a) -> IO a
withReadingWindowsHandle
    = withRef (wantReadableHandle_ "withReadingWindowsHandle")
              getWindowsHandle

{-|
    Obtains from a Haskell handle a Windows handle for writing and executes a
    user-provided action on it. The Haskell-side buffers of the Haskell handle
    are flushed before this action is run. While this action is executed,
    further operations on the handle are blocked to a degree that interference
    with this action is prevented.

    If the I/O subsystem in use is not the Windows one, an exception is thrown.

    See [below](#with-ref-caveats) for caveats regarding this operation.
-}
withWritingWindowsHandle :: Handle -> (Ptr () -> IO a) -> IO a
withWritingWindowsHandle
    = withRef (wantWritableHandle "withWritingWindowsHandle")
              getWindowsHandle

-- ** Caveats

{-$with-ref-caveats
    #with-ref-caveats#There are the following caveats regarding each of the
    above operations:

      * The flushing of buffers can fail if the given handle is readable but not
        seekable.

      * If the operation is performed as part of an action executed by
        'unsafePerformIO', 'unsafeInterleaveIO', or one of their “dupable”
        variants and the user-provided action receives an asychnchronous
        exception and does not catch it, then the following happens:

          - Before the overall computation is suspended, the blocking of handle
            operations is removed.

          - When the computation is later resumed due to another evaluation
            attempt, the blocking of handle operations is reinstantiated, the
            Haskell-side buffers are flushed again, and the user-provided action
            is run from the beginning.

        Repeating the previously executed part of the user-provided action
        cannot be avoided apparently. See the @[async]@ note in the source code
        of "GHC.Internal.IO.Handle.Internals" for further explanation.
-}

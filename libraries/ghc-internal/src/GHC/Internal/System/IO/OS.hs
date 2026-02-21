{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

{-|
    This module bridges between Haskell handles and underlying operating-system
    features.
-}
module GHC.Internal.System.IO.OS
(
    -- * Obtaining file descriptors and Windows handles
    withFileDescriptorReadingBiased,
    withFileDescriptorWritingBiased,
    withWindowsHandleReadingBiased,
    withWindowsHandleWritingBiased,
    withFileDescriptorReadingBiasedRaw,
    withFileDescriptorWritingBiasedRaw,
    withWindowsHandleReadingBiasedRaw,
    withWindowsHandleWritingBiasedRaw

    -- ** Caveats
    -- $with-ref-caveats
)
where

#if defined(mingw32_HOST_OS)
import GHC.Internal.Base (otherwise)
#endif
import GHC.Internal.Control.Monad (return)
import GHC.Internal.Control.Concurrent.MVar (MVar)
import GHC.Internal.Control.Exception (mask)
import GHC.Internal.Data.Function (const, (.), ($))
import GHC.Internal.Data.Functor (fmap)
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
import GHC.Internal.IO.Handle.Types
       (
           Handle (FileHandle, DuplexHandle),
           Handle__ (Handle__, haDevice)
       )
import GHC.Internal.IO.Handle.Internals (withHandle_', flushBuffer)
import GHC.Internal.IO.Exception
       (
           IOErrorType (InappropriateType),
           IOException (IOError),
           ioException
       )
import GHC.Internal.Foreign.Ptr (Ptr)
import GHC.Internal.Foreign.C.Types (CInt)

-- * Obtaining POSIX file descriptors and Windows handles

{-|
    Executes a user-provided action on an operating-system handle that underlies
    a Haskell handle. Before the user-provided action is run, user-defined
    preparation based on the handle state that contains the operating-system
    handle is performed. While the user-provided action is executed, further
    operations on the Haskell handle are blocked to a degree that interference
    with this action is prevented.

    See [below](#with-ref-caveats) for caveats regarding this operation.
-}
withOSHandle :: String
                -- ^ The name of the overall operation
             -> (Handle -> MVar Handle__)
                {-^
                    Obtaining of the handle state variable that holds the
                    operating-system handle
                -}
             -> (forall d. Typeable d => d -> IO a)
                -- ^ Conversion of a device into an operating-system handle
             -> (Handle__ -> IO ())
                -- ^ The preparation
             -> Handle
                -- ^ The Haskell handle to use
             -> (a -> IO r)
                -- ^ The action to execute on the operating-system handle
             -> IO r
withOSHandle opName handleStateVar getOSHandle prepare handle act
    = mask $ \ withOriginalMaskingState ->
      withHandleState $ \ handleState@Handle__ {haDevice = dev} -> do
          osHandle <- getOSHandle dev
          prepare handleState
          withOriginalMaskingState $ act osHandle
      where

      withHandleState = withHandle_' opName handle (handleStateVar handle)
{-
    The 'withHandle_'' operation, which we use here, already performs masking.
    Still, we have to employ 'mask', in order do obtain the operation that
    restores the original masking state. The user-provided action should be
    executed with this original masking state, as there is no inherent reason to
    generally perform it with masking in place. The masking that 'withHandle_''
    performs is only for safely accessing handle state and thus constitutes an
    implementation detail; it has nothing to do with the user-provided action.
-}
{-
    The order of actions in 'withOSHandle' is such that any exception from
    'getOSHandle' is thrown before the user-defined preparation is performed.
-}

{-|
    Obtains the handle state variable that underlies a handle or specifically
    the handle state variable for reading if the handle uses different state
    variables for reading and writing.
-}
handleStateVarReadingBiased :: Handle -> MVar Handle__
handleStateVarReadingBiased (FileHandle _ var)            = var
handleStateVarReadingBiased (DuplexHandle _ readingVar _) = readingVar

{-|
    Obtains the handle state variable that underlies a handle or specifically
    the handle state variable for writing if the handle uses different state
    variables for reading and writing.
-}
handleStateVarWritingBiased :: Handle -> MVar Handle__
handleStateVarWritingBiased (FileHandle _ var)            = var
handleStateVarWritingBiased (DuplexHandle _ _ writingVar) = writingVar

{-|
    Yields the result of another operation if that operation succeeded, and
    otherwise throws an exception that signals that the other operation failed
    because some Haskell handle does not use an operating-system handle of a
    required type.
-}
requiringOSHandleOfType :: String
                           -- ^ The name of the operating-system handle type
                        -> Maybe a
                           {-^
                               The result of the other operation if it succeeded
                           -}
                        -> IO a
requiringOSHandleOfType osHandleTypeName
    = maybe (ioException osHandleOfTypeRequired) return
    where

    osHandleOfTypeRequired :: IOException
    osHandleOfTypeRequired
        = IOError Nothing
                  InappropriateType
                  ""
                  ("handle does not use " ++ osHandleTypeName ++ "s")
                  Nothing
                  Nothing

{-|
    Obtains the POSIX file descriptor of a device if the device contains one,
    and throws an exception otherwise.
-}
getFileDescriptor :: Typeable d => d -> IO CInt
getFileDescriptor = requiringOSHandleOfType "POSIX file descriptor" .
                    fmap fdFD . cast

{-|
    Obtains the Windows handle of a device if the device contains one, and
    throws an exception otherwise.
-}
getWindowsHandle :: Typeable d => d -> IO (Ptr ())
getWindowsHandle = requiringOSHandleOfType "Windows handle" .
                   toMaybeWindowsHandle
    where

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
    Executes a user-provided action on the POSIX file descriptor that underlies
    a handle or specifically on the POSIX file descriptor for reading if the
    handle uses different file descriptors for reading and writing. The
    Haskell-managed buffers related to the file descriptor are flushed before
    the user-provided action is run. While this action is executed, further
    operations on the handle are blocked to a degree that interference with this
    action is prevented.

    If the handle does not use POSIX file descriptors, an exception is thrown.

    See [below](#with-ref-caveats) for caveats regarding this operation.
-}
withFileDescriptorReadingBiased :: Handle -> (CInt -> IO r) -> IO r
withFileDescriptorReadingBiased = withOSHandle "withFileDescriptorReadingBiased"
                                               handleStateVarReadingBiased
                                               getFileDescriptor
                                               flushBuffer

{-|
    Executes a user-provided action on the POSIX file descriptor that underlies
    a handle or specifically on the POSIX file descriptor for writing if the
    handle uses different file descriptors for reading and writing. The
    Haskell-managed buffers related to the file descriptor are flushed before
    the user-provided action is run. While this action is executed, further
    operations on the handle are blocked to a degree that interference with this
    action is prevented.

    If the handle does not use POSIX file descriptors, an exception is thrown.

    See [below](#with-ref-caveats) for caveats regarding this operation.
-}
withFileDescriptorWritingBiased :: Handle -> (CInt -> IO r) -> IO r
withFileDescriptorWritingBiased = withOSHandle "withFileDescriptorWritingBiased"
                                               handleStateVarWritingBiased
                                               getFileDescriptor
                                               flushBuffer

{-|
    Executes a user-provided action on the Windows handle that underlies a
    Haskell handle or specifically on the Windows handle for reading if the
    Haskell handle uses different Windows handles for reading and writing. The
    Haskell-managed buffers related to the Windows handle are flushed before the
    user-provided action is run. While this action is executed, further
    operations on the Haskell handle are blocked to a degree that interference
    with this action is prevented.

    If the Haskell handle does not use Windows handles, an exception is thrown.

    See [below](#with-ref-caveats) for caveats regarding this operation.
-}
withWindowsHandleReadingBiased :: Handle -> (Ptr () -> IO r) -> IO r
withWindowsHandleReadingBiased = withOSHandle "withWindowsHandleReadingBiased"
                                              handleStateVarReadingBiased
                                              getWindowsHandle
                                              flushBuffer

{-|
    Executes a user-provided action on the Windows handle that underlies a
    Haskell handle or specifically on the Windows handle for writing if the
    Haskell handle uses different Windows handles for reading and writing. The
    Haskell-managed buffers related to the Windows handle are flushed before the
    user-provided action is run. While this action is executed, further
    operations on the Haskell handle are blocked to a degree that interference
    with this action is prevented.

    If the Haskell handle does not use Windows handles, an exception is thrown.

    See [below](#with-ref-caveats) for caveats regarding this operation.
-}
withWindowsHandleWritingBiased :: Handle -> (Ptr () -> IO r) -> IO r
withWindowsHandleWritingBiased = withOSHandle "withWindowsHandleWritingBiased"
                                              handleStateVarWritingBiased
                                              getWindowsHandle
                                              flushBuffer

{-|
    Like 'withFileDescriptorReadingBiased' except that Haskell-managed buffers
    are not flushed.
-}
withFileDescriptorReadingBiasedRaw :: Handle -> (CInt -> IO r) -> IO r
withFileDescriptorReadingBiasedRaw
    = withOSHandle "withFileDescriptorReadingBiasedRaw"
                   handleStateVarReadingBiased
                   getFileDescriptor
                   (const $ return ())

{-|
    Like 'withFileDescriptorWritingBiased' except that Haskell-managed buffers
    are not flushed.
-}
withFileDescriptorWritingBiasedRaw :: Handle -> (CInt -> IO r) -> IO r
withFileDescriptorWritingBiasedRaw
    = withOSHandle "withFileDescriptorWritingBiasedRaw"
                   handleStateVarWritingBiased
                   getFileDescriptor
                   (const $ return ())

{-|
    Like 'withWindowsHandleReadingBiased' except that Haskell-managed buffers
    are not flushed.
-}
withWindowsHandleReadingBiasedRaw :: Handle -> (Ptr () -> IO r) -> IO r
withWindowsHandleReadingBiasedRaw
    = withOSHandle "withWindowsHandleReadingBiasedRaw"
                   handleStateVarReadingBiased
                   getWindowsHandle
                   (const $ return ())

{-|
    Like 'withWindowsHandleWritingBiased' except that Haskell-managed buffers
    are not flushed.
-}
withWindowsHandleWritingBiasedRaw :: Handle -> (Ptr () -> IO r) -> IO r
withWindowsHandleWritingBiasedRaw
    = withOSHandle "withWindowsHandleWritingBiasedRaw"
                   handleStateVarWritingBiased
                   getWindowsHandle
                   (const $ return ())

-- ** Caveats

{-$with-ref-caveats
    #with-ref-caveats#This subsection is just a dummy, whose purpose is to serve
    as the target of the hyperlinks above. The real documentation of the caveats
    is in the /Caveats/ subsection in the @base@ module @System.IO.OS@, which
    re-exports the above operations.
-}

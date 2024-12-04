{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.IO.SubSystem
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- The 'IoSubSystem' control interface.  These methods can be used to disambiguate
-- between the two operations.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

module GHC.Internal.IO.SubSystem (
  withIoSubSystem,
  withIoSubSystem',
  whenIoSubSystem,
  ioSubSystem,
  IoSubSystem(..),
  conditional,
  (<!>),
  isWindowsNativeIO
 ) where

import GHC.Internal.Base
import GHC.Internal.Show
import GHC.Internal.Enum

#if defined(mingw32_HOST_OS)
import GHC.Internal.IO.Unsafe
import GHC.Internal.Foreign.Ptr
import GHC.Internal.Foreign.Storable
import GHC.Internal.Foreign.C.Types
import GHC.Internal.Foreign.Marshal.Utils
#endif

infixl 7 <!>

-- | The I/O SubSystem to use in the program.
--
-- @since base-4.9.0.0
data IoSubSystem
  = IoPOSIX   -- ^ Use a POSIX I/O Sub-System
  | IoNative  -- ^ Use platform native Sub-System. For unix OSes this is the
              --   same as IoPOSIX, but on Windows this means use the Windows
              --   native APIs for I/O, including IOCP and RIO.
  deriving (Eq)

-- N.B. These are currently unused by GHC but is needed for stability of @base@.
deriving instance Enum IoSubSystem
deriving instance Show IoSubSystem

-- | Conditionally execute an action depending on the configured I/O subsystem.
-- On POSIX systems always execute the first action.
-- On Windows execute the second action if WINIO as active, otherwise fall back to
-- the first action.
conditional :: a -> a -> a
#if defined(mingw32_HOST_OS)
conditional posix windows =
  case ioSubSystem of
    IoPOSIX -> posix
    IoNative -> windows
#else
conditional posix _       = posix
#endif

-- | Infix version of `conditional`.
-- posix <!> windows == conditional posix windows
(<!>) :: a -> a -> a
(<!>) = conditional

isWindowsNativeIO :: Bool
isWindowsNativeIO = False <!> True

-- | The 'IoSubSystem' in use.
--
-- This is needed to optimize support for different IO Managers on Windows.
-- GHC supports both the new WinIO manager as well as the old MIO (threaded),
-- and ancient win32-legacy (non-threaded) ones. The WinIO manager uses native
-- Win32 HANDLEs, whereas the other two use posix style FDs (via translation
-- layers).
--
-- In many places in the I\/O base library code, for correctness or performance
-- on Windows, we have to take different code paths depending on which style of
-- IO manager is in use. The IO manager is set on RTS startup (and the default
-- choice can be overridden using RTS flags). On Windows this value is obtained
-- by reading a global variable that is set by the RTS IOManager on startup.
--
-- On non-Windows systems this value is always 'IoPOSIX'.
--
ioSubSystem :: IoSubSystem
#if defined(mingw32_HOST_OS)
{-# INLINE ioSubSystem #-}
ioSubSystem =
  case toBool ioManagerIsWin32NativeCBool of
    False -> IoPOSIX
    True  -> IoNative

{-# NOINLINE ioManagerIsWin32NativeCBool #-}
ioManagerIsWin32NativeCBool :: CBool
ioManagerIsWin32NativeCBool =
  unsafeDupablePerformIO $ peek ioManagerIsWin32NativePtr

foreign import ccall "&rts_IOManagerIsWin32Native"
  ioManagerIsWin32NativePtr :: Ptr CBool
#else
ioSubSystem = IoPOSIX
#endif

withIoSubSystem :: (IoSubSystem -> IO a) -> IO a
withIoSubSystem f = f ioSubSystem

withIoSubSystem' :: (IoSubSystem -> a) -> a
withIoSubSystem' f = f ioSubSystem

whenIoSubSystem :: IoSubSystem -> IO () -> IO ()
whenIoSubSystem m f = do let sub = ioSubSystem
                         when (sub == m) f


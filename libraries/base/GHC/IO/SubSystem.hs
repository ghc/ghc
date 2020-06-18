{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.SubSystem
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- The SubSystem control interface.  These methods can be used to disambiguate
-- between the two operations.
--
-----------------------------------------------------------------------------

module GHC.IO.SubSystem (
  withIoSubSystem,
  withIoSubSystem',
  whenIoSubSystem,
  ioSubSystem,
  IoSubSystem(..),
  conditional,
  (<!>),
  isWindowsNativeIO
 ) where

import GHC.Base
import GHC.RTS.Flags

#if defined(mingw32_HOST_OS)
import GHC.IO.Unsafe
#endif

infixl 7 <!>

-- | Conditionally execute an action depending on the configured I/O subsystem.
-- On POSIX systems always execute the first action.
-- On windows execute the second action if WINIO as active, otherwise fall back to
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

ioSubSystem :: IoSubSystem
#if defined(mingw32_HOST_OS)
{-# NOINLINE ioSubSystem #-}
ioSubSystem = unsafeDupablePerformIO getIoManagerFlag
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


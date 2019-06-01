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
  setIoSubSystem,
  getIoSubSystem,
  withIoSubSystem,
  withIoSubSystem',
  whenIoSubSystem,
  IoSubSystem(..),
  conditional,
  (<!>),
  isWindowsNativeIO
 ) where

import GHC.Base

import GHC.IO.Unsafe
import GHC.IORef
import GHC.RTS.Flags

infixl 7 <!>

-- | Conditionally execute an action depending on the configured I/O subsystem.
-- If POSIX then execute first action, if Windows then execute second.
-- On POSIX systems but NATIVE and POSIX will execute the first action.
conditional :: a -> a -> a
conditional posix windows = withIoSubSystem' sub
  where
    sub = \s -> case s of
                  IoPOSIX -> posix
#if defined(mingw32_HOST_OS)
                  IoNative -> windows
#else
                  IoNative -> posix
#endif

-- | Infix version of `conditional`.
(<!>) :: a -> a -> a
(<!>) = conditional

isWindowsNativeIO :: Bool
isWindowsNativeIO = False <!> True

ioSubSystem :: IORef IoSubSystem
ioSubSystem = unsafePerformIO sub
  where
    sub = do misc <- getMiscFlags
             newIORef (ioManager misc)

setIoSubSystem :: IoSubSystem -> IO ()
setIoSubSystem = writeIORef ioSubSystem

getIoSubSystem :: IO IoSubSystem
getIoSubSystem = readIORef ioSubSystem

withIoSubSystem :: (IoSubSystem -> IO a) -> IO a
withIoSubSystem f = do sub <- getIoSubSystem
                       f sub

withIoSubSystem' :: (IoSubSystem -> a) -> a
withIoSubSystem' f = unsafePerformIO inner
  where inner = do sub <- getIoSubSystem
                   return (f sub)

whenIoSubSystem :: IoSubSystem -> IO () -> IO ()
whenIoSubSystem m f = do sub <- getIoSubSystem
                         when (sub == m) f


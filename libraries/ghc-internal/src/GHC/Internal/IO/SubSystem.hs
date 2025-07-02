{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MagicHash          #-}

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
  isWindowsNativeIO,

  -- * I\/O manager features
  -- | Different I\/O manager implementations support different features, and
  -- some I\/O code within ghc-internal or base must be conditional on these
  -- features. This API is used to dynamically test for features supported by
  -- the I\/O manager currently in use.
  --
  -- The I\/O manager is selected on RTS startup and is not changed thereafter.
  -- Thus these feature tests are stable, but must be made at runtime.
  --
  -- Historically such code was conditional on hard-coded assumptions about
  -- which I\/O manager is available for particular platforms or RTS ways.
  -- For example, historically a lot of code has been conditional on whether
  -- the RTS way was threaded or non-threaded, as a proxy for whether the I\/O
  -- manager is in the RTS or in Haskell, because historically each impled the
  -- other. This can instead now be done conditionally on the I\/O manager
  -- feature 'iomgrInRTS'.
  iomgrInRTS,
  iomgrUsesHANDLEs,
 ) where

import GHC.Internal.Base
import GHC.Internal.Enum
import GHC.Internal.Num
import GHC.Internal.Prim (Word#, and#, indexWordOffAddr#, uncheckedShiftRL#)
import GHC.Internal.Ptr (Ptr(..))
import GHC.Internal.Show

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
conditional posix windows =
  case ioSubSystem of
    IoPOSIX -> posix
    IoNative -> windows

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
ioSubSystem = if iomgrUsesHANDLEs then IoNative else IoPOSIX

withIoSubSystem :: (IoSubSystem -> IO a) -> IO a
withIoSubSystem f = f ioSubSystem

withIoSubSystem' :: (IoSubSystem -> a) -> a
withIoSubSystem' f = f ioSubSystem

whenIoSubSystem :: IoSubSystem -> IO () -> IO ()
whenIoSubSystem m f = do let sub = ioSubSystem
                         when (sub == m) f


-- | The available I\/O manager features we can dynamically test for.
--
-- This set is likely to be extended in future, as the I\/O functionality is
-- extended.
--
data IOManagerFeature =
       IOMgrInRTS
     | IOMgrUsesHANDLEs

ioManagerFeatureBitmask :: IOManagerFeature -> Word#
ioManagerFeatureBitmask IOMgrInRTS       = 1## `uncheckedShiftRL#` 0#
ioManagerFeatureBitmask IOMgrUsesHANDLEs = 1## `uncheckedShiftRL#` 1#

-- | This means that the I\/O manager is implemented within the RTS. The
-- inverse is that it is implemented in Haskell. Interaction with an in-RTS
-- I\/O manager should be via the appropriate primops, while interaction
-- with in-Haskell I\/O managers can be done by normal library calls.
--
-- In principle this is independent of the RTS threaded\/non-threaded way:
-- neither implies the other. This is intended to allow for using in-RTS
-- I\/O managers in the threaded or non-threaded RTS.
--
iomgrInRTS :: Bool
iomgrInRTS = ioManagerFeature IOMgrInRTS

-- | This means that the I\/O manager uses the Win32 API's HANDLE type to
-- refer to open files. The inverse is that it uses Posix style fds.
-- This is always false on Poisx platforms, while on Windows it is true
-- for some but not all of the Windows I\/O manager implementations.
--
iomgrUsesHANDLEs :: Bool
#if defined(mingw32_HOST_OS)
iomgrUsesHANDLEs = ioManagerFeature IOMgrUsesHANDLEs
#else
iomgrUsesHANDLEs = False -- this is just an optimisation
#endif

-- | Test for availablity of a feature of the current I\/O manager.
--
-- The I\/O manager is selected on startup and not changed thereafter. Thus
-- these feature tests are stable, but must be made at runtime.
--
ioManagerFeature :: IOManagerFeature -> Bool
ioManagerFeature feature =
  case ioManagerFeaturesPtr of
    Ptr ioManagerFeaturesAddr# ->
      case and# (ioManagerFeatureBitmask feature)
                (indexWordOffAddr# ioManagerFeaturesAddr# 0#) of
        0## -> False
        _   -> True

foreign import ccall unsafe "&rts_IOManagerFeatures"
  ioManagerFeaturesPtr :: Ptr Int

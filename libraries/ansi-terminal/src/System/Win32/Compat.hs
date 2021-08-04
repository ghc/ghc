#include "Common-Safe-Haskell.hs"
{-# OPTIONS_HADDOCK hide #-}

{-| The Win32 library ships with GHC. Win32-2.1 first shipped with GHC 6.6
(released October 2006). Win32-2.5.4.1 first shipped with GHC 8.2.1 (released
July 2017), replacing Win32-2.3.1.1.

The ansi-terminal library makes use of functionality in Win32-2.1 and other
functionality first added to Win32-2.5.0.0 or Win32-2.5.1.0 (from ansi-terminal
itself).

This module provides functions available in those later versions of Win32 to a
wider range of compilers, reducing the use of CPP pragmas in other modules.
-}
module System.Win32.Compat
  (
    BOOL
  , DWORD
  , ErrCode
  , HANDLE
  , LPCTSTR
  , LPDWORD
  , SHORT                -- from Win32-2.5.0.0
  , TCHAR
  , UINT
  , WORD
  , failIfFalse_
  , getLastError
  , iNVALID_HANDLE_VALUE
  , nullHANDLE
  , withHandleToHANDLE   -- from Win32-2.5.1.0
  , withTString
  ) where

#if !MIN_VERSION_Win32(2,5,0)
import Foreign.C.Types (CShort (..))
#endif

#if !MIN_VERSION_Win32(2,5,1)
import Control.Concurrent.MVar (readMVar)
import Control.Exception (bracket)
import Foreign.C.Types (CInt (..))
import Foreign.StablePtr (StablePtr, freeStablePtr, newStablePtr)
import Data.Typeable (cast)
import GHC.IO.FD (FD(..)) -- A wrapper around an Int32
import GHC.IO.Handle.Types (Handle (..), Handle__ (..))
#endif

import System.Win32.Types (BOOL, DWORD, ErrCode, HANDLE, LPCTSTR, LPDWORD,
  TCHAR, UINT, WORD, failIfFalse_, getLastError, iNVALID_HANDLE_VALUE,
  nullHANDLE, withTString)

#if MIN_VERSION_Win32(2,5,0)
import System.Win32.Types (SHORT)
#endif

#if MIN_VERSION_Win32(2,5,1)
import System.Win32.Types (withHandleToHANDLE)
#endif

#if !MIN_VERSION_Win32(2,5,0)
type SHORT = CShort
#endif

#if !MIN_VERSION_Win32(2,5,1)

#if defined(i386_HOST_ARCH)
#define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
#define WINDOWS_CCONV ccall
#else
#error Unknown mingw32 arch
#endif

-- | This bit is all highly dubious. The problem is that we want to output ANSI
-- to arbitrary Handles rather than forcing people to use stdout.  However, the
-- Windows ANSI emulator needs a Windows HANDLE to work it's magic, so we need
-- to be able to extract one of those from the Haskell Handle.
--
-- This code accomplishes this, albeit at the cost of only being compatible with
-- GHC.
withHandleToHANDLE :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLE haskell_handle action =
  -- Create a stable pointer to the Handle. This prevents the garbage collector
  -- getting to it while we are doing horrible manipulations with it, and hence
  -- stops it being finalized (and closed).
  withStablePtr haskell_handle $ const $ do
    -- Grab the write handle variable from the Handle
    let write_handle_mvar = case haskell_handle of
          FileHandle _ handle_mvar     -> handle_mvar
          DuplexHandle _ _ handle_mvar -> handle_mvar -- This is "write" MVar,
          -- we could also take the "read" one

    -- Get the FD from the algebraic data type
    Just fd <- fmap (\(Handle__ { haDevice = dev }) ->
      fmap fdFD (cast dev)) $ readMVar write_handle_mvar

    -- Finally, turn that (C-land) FD into a HANDLE using msvcrt
    windows_handle <- cget_osfhandle fd

    -- Do what the user originally wanted
    action windows_handle

-- This essential function comes from the C runtime system. It is certainly
-- provided by msvcrt, and also seems to be provided by the mingw C library -
-- hurrah!
foreign import WINDOWS_CCONV unsafe "_get_osfhandle"
  cget_osfhandle :: CInt -> IO HANDLE

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr value = bracket (newStablePtr value) freeStablePtr
#endif

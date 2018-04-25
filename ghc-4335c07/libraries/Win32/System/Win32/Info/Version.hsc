{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.Info.Version
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Version information about your computer.
-}
module System.Win32.Info.Version
  ( -- * Version Info
    OSVERSIONINFOEX(..), POSVERSIONINFOEX, LPOSVERSIONINFOEX
  , ProductType(..)
  , getVersionEx, c_GetVersionEx
  
    -- * Verify OS version
  , isVistaOrLater, is7OrLater
  ) where
import Foreign.Ptr           ( Ptr, plusPtr )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Storable      ( Storable(..) )
import System.Win32.String   ( withTString, peekTString )
import System.Win32.Types    ( BOOL, BYTE, failIfFalse_ )
import System.Win32.Word     ( WORD, DWORD )

#include <windows.h>
#include "alignment.h"
##include "windows_cconv.h"

----------------------------------------------------------------
-- Version Info
----------------------------------------------------------------
getVersionEx :: IO OSVERSIONINFOEX
getVersionEx =
  alloca $ \buf -> do
    (#poke OSVERSIONINFOEXW, dwOSVersionInfoSize) buf
      $ sizeOf (undefined::OSVERSIONINFOEX)
    failIfFalse_ "GetVersionEx"
      $ c_GetVersionEx buf
    peek buf

data ProductType = VerUnknow BYTE | VerNTWorkStation | VerNTDomainControler | VerNTServer
    deriving (Show,Eq)

instance Storable ProductType where
    sizeOf    _ = sizeOf    (undefined::BYTE)
    alignment _ = alignment (undefined::BYTE)
    poke buf v = pokeByteOff buf 0 $ case v of
        VerUnknow w          -> w
        VerNTWorkStation     -> #const VER_NT_WORKSTATION
        VerNTDomainControler -> #const VER_NT_DOMAIN_CONTROLLER
        VerNTServer          -> #const VER_NT_SERVER
    peek buf = do
        v <- peekByteOff buf 0
        return $ case v of
            (#const VER_NT_WORKSTATION)       -> VerNTWorkStation
            (#const VER_NT_DOMAIN_CONTROLLER) -> VerNTDomainControler
            (#const VER_NT_SERVER)            -> VerNTServer
            w                                 -> VerUnknow w

type POSVERSIONINFOEX = Ptr OSVERSIONINFOEX
type LPOSVERSIONINFOEX = Ptr OSVERSIONINFOEX

data OSVERSIONINFOEX = OSVERSIONINFOEX
     { dwMajorVersion    :: DWORD
     , dwMinorVersion    :: DWORD
     , dwBuildNumber     :: DWORD
     , dwPlatformId      :: DWORD
     , szCSDVersion      :: String
     , wServicePackMajor :: WORD
     , wServicePackMinor :: WORD
     , wSuiteMask        :: WORD
     , wProductType      :: ProductType
     } deriving Show

instance Storable OSVERSIONINFOEX where
    sizeOf = const #{size struct _OSVERSIONINFOEXW}
    alignment _ = #alignment OSVERSIONINFOEX
    poke buf info = do
        (#poke OSVERSIONINFOEXW, dwOSVersionInfoSize) buf (sizeOf info)
        (#poke OSVERSIONINFOEXW, dwMajorVersion) buf (dwMajorVersion info)
        (#poke OSVERSIONINFOEXW, dwMinorVersion) buf (dwMinorVersion info)
        (#poke OSVERSIONINFOEXW, dwBuildNumber)  buf (dwBuildNumber info)
        (#poke OSVERSIONINFOEXW, dwPlatformId) buf (dwPlatformId info)
        withTString (szCSDVersion info) $ \szCSDVersion' ->
          (#poke OSVERSIONINFOEXW, szCSDVersion) buf szCSDVersion'
        (#poke OSVERSIONINFOEXW, wServicePackMajor) buf (wServicePackMajor info)
        (#poke OSVERSIONINFOEXW, wServicePackMinor) buf (wServicePackMinor info)
        (#poke OSVERSIONINFOEXW, wSuiteMask)   buf (wSuiteMask info)
        (#poke OSVERSIONINFOEXW, wProductType) buf (wProductType info)
        (#poke OSVERSIONINFOEXW, wReserved)    buf (0::BYTE)

    peek buf = do
        majorVersion     <- (#peek OSVERSIONINFOEXW, dwMajorVersion) buf
        minorVersion     <- (#peek OSVERSIONINFOEXW, dwMinorVersion) buf
        buildNumber      <- (#peek OSVERSIONINFOEXW, dwBuildNumber) buf
        platformId       <- (#peek OSVERSIONINFOEXW, dwPlatformId) buf
        cSDVersion       <- peekTString $ (#ptr OSVERSIONINFOEXW, szCSDVersion) buf
        servicePackMajor <- (#peek OSVERSIONINFOEXW, wServicePackMajor) buf
        servicePackMinor <- (#peek OSVERSIONINFOEXW, wServicePackMinor) buf
        suiteMask        <- (#peek OSVERSIONINFOEXW, wSuiteMask) buf
        productType      <- (#peek OSVERSIONINFOEXW, wProductType) buf
        return $ OSVERSIONINFOEX majorVersion minorVersion
                                 buildNumber platformId cSDVersion
                                 servicePackMajor servicePackMinor
                                 suiteMask productType

foreign import WINDOWS_CCONV unsafe "windows.h GetVersionExW"
  c_GetVersionEx :: LPOSVERSIONINFOEX -> IO BOOL

----------------------------------------------------------------
-- Verify OS version
----------------------------------------------------------------
-- See: http://msdn.microsoft.com/en-us/library/windows/desktop/ms724833(v=vs.85).aspx

isVistaOrLater, is7OrLater :: IO Bool
isVistaOrLater = do
  ver <- getVersionEx
  return $ 6 <= dwMajorVersion ver

is7OrLater = do
  ver <- getVersionEx
  return $  6 <= dwMajorVersion ver
         && 1 <= dwMinorVersion ver

{-
We don't use VerifyVersionInfo function to above functions.

Because VerifyVersionInfo is more difficult than GetVersionEx and accessing field in Haskell.

-- | See: http://support.microsoft.com/kb/225013/
-- http://msdn.microsoft.com/en-us/library/windows/desktop/ms725491(v=vs.85).aspx

bIsWindowsVersionOK :: DWORD -> DWORD -> WORD -> IO BOOL
bIsWindowsVersionOK dwMajor dwMinor dwSPMajor =
  alloca $ \buf -> do
    zeroMemory buf
      (#{size OSVERSIONINFOEXW}::DWORD)
    (#poke OSVERSIONINFOEXW, dwOSVersionInfoSize) buf
      (#{size OSVERSIONINFOEXW}::DWORD)
    (#poke OSVERSIONINFOEXW, dwMajorVersion)    buf dwMajor
    (#poke OSVERSIONINFOEXW, dwMinorVersion)    buf dwMinor
    (#poke OSVERSIONINFOEXW, wServicePackMajor) buf dwSPMajor
    --  Set up the condition mask.
    let dwlConditionMask = 0
        flag =    #const VER_MAJORVERSION
             .|.  #const VER_MINORVERSION
             .|.  #const VER_SERVICEPACKMAJOR
    dwlConditionMask'   <- vER_SET_CONDITION dwlConditionMask   #{const VER_MAJORVERSION} #{const VER_GREATER_EQUAL}
    dwlConditionMask''  <- vER_SET_CONDITION dwlConditionMask'  #{const VER_MINORVERSION} #{const VER_MINORVERSION}
    dwlConditionMask''' <- vER_SET_CONDITION dwlConditionMask'' #{const VER_SERVICEPACKMAJOR} #{const VER_SERVICEPACKMAJOR}
    verifyVersionInfo buf flag dwlConditionMask'''

type ULONGLONG = DWORDLONG

foreign import capi unsafe "windows.h VER_SET_CONDITION"
  vER_SET_CONDITION :: ULONGLONG -> DWORD -> BYTE -> IO ULONGLONG

foreign import WINDOWS_CCONV unsafe "windows.h VerifyVersionInfoW"
  verifyVersionInfo :: LPOSVERSIONINFOEX -> DWORD -> DWORDLONG -> IO BOOL
-}

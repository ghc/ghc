{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.Info.Computer
   Copyright   :  2012-2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Information about your computer.
-}
module System.Win32.Info.Computer
  ( -- * Environment Strings
    expandEnvironmentStrings, c_ExpandEnvironmentStrings

    -- * Computer Name
  , getComputerName, setComputerName
  , c_GetComputerName, c_SetComputerName

    -- * System metrics
  , getSystemMetrics
  , sM_CMONITORS
  , sM_IMMENABLED
  , sM_MOUSEWHEELPRESENT
  , sM_REMOTESESSION
  , sM_SAMEDISPLAYFORMAT
  , sM_XVIRTUALSCREEN
  , sM_YVIRTUALSCREEN
  , sM_SERVERR2
  , sM_MEDIACENTER
  , sM_STARTER
  , sM_TABLETPC

  -- * User name
  , getUserName, c_GetUserName

  -- * Version Info
  , OSVERSIONINFOEX(..), POSVERSIONINFOEX, LPOSVERSIONINFOEX
  , ProductType(..)
  , getVersionEx, c_GetVersionEx

  -- * Processor features
  , ProcessorFeature
  , isProcessorFeaturePresent
  , pF_3DNOW_INSTRUCTIONS_AVAILABLE
  , pF_COMPARE_EXCHANGE_DOUBLE
  , pF_FLOATING_POINT_EMULATED
  , pF_FLOATING_POINT_PRECISION_ERRATA
  , pF_MMX_INSTRUCTIONS_AVAILABLE
  , pF_PAE_ENABLED
  , pF_RDTSC_INSTRUCTION_AVAILABLE
  , pF_XMMI_INSTRUCTIONS_AVAILABLE
  , pF_XMMI64_INSTRUCTIONS_AVAILABLE
  ) where

import Foreign.Marshal.Utils ( with )
import Foreign.Storable      ( Storable(..) )
import System.Win32.Info     ( SMSetting )
import System.Win32.Info.Version
import System.Win32.String   ( LPCTSTR, LPTSTR, withTString, withTStringBuffer
                             , peekTString, peekTStringLen )
import System.Win32.Types    ( BOOL, failIfFalse_ )
import System.Win32.Utils    ( tryWithoutNull )
import System.Win32.Word     ( DWORD, LPDWORD )

#include <windows.h>
#include <lmcons.h>
#include "alignment.h"
##include "windows_cconv.h"

----------------------------------------------------------------
-- Environment Strings
----------------------------------------------------------------
expandEnvironmentStrings :: String -> IO String
expandEnvironmentStrings name =
  withTString name $ \ c_name ->
    tryWithoutNull (unwords ["ExpandEnvironmentStrings", name])
      (\buf len -> c_ExpandEnvironmentStrings c_name buf len) 512

foreign import WINDOWS_CCONV unsafe "windows.h ExpandEnvironmentStringsW"
  c_ExpandEnvironmentStrings :: LPCTSTR -> LPTSTR -> DWORD -> IO DWORD

----------------------------------------------------------------
-- Computer Name
----------------------------------------------------------------

getComputerName :: IO String
getComputerName =
  withTStringBuffer  maxLength  $ \buf ->
  with (fromIntegral maxLength) $ \len -> do
      failIfFalse_ "GetComputerName"
        $ c_GetComputerName buf len
      len' <- peek len
      peekTStringLen (buf, (fromIntegral len'))
  where
    maxLength = #const MAX_COMPUTERNAME_LENGTH

foreign import WINDOWS_CCONV unsafe "GetComputerNameW"
  c_GetComputerName :: LPTSTR -> LPDWORD -> IO Bool

setComputerName :: String -> IO ()
setComputerName name =
  withTString name $ \buf ->
      failIfFalse_ (unwords ["SetComputerName", name])
        $ c_SetComputerName buf

foreign import WINDOWS_CCONV unsafe "SetComputerNameW"
  c_SetComputerName :: LPTSTR -> IO Bool
{-
type COMPUTER_NAME_FORMAT = UINT
{enum COMPUTER_NAME_FORMAT,
 , computerNameNetBIOS                   = ComputerNameNetBIOS
 , computerNameDnsHostname               = ComputerNameDnsHostname
 , computerNameDnsDomain                 = ComputerNameDnsDomain
 , computerNameDnsFullyQualified         = ComputerNameDnsFullyQualified
 , computerNamePhysicalNetBIOS           = ComputerNamePhysicalNetBIOS
 , computerNamePhysicalDnsHostname       = ComputerNamePhysicalDnsHostname
 , computerNamePhysicalDnsDomain         = ComputerNamePhysicalDnsFullyQualified
 , computerNamePhysicalDnsFullyQualified = ComputerNamePhysicalDnsFullyQualified
 , computerNameMax                       = ComputerNameMax
 }
-}

----------------------------------------------------------------
-- Hardware Profiles
----------------------------------------------------------------
{-
-- TODO: Deside HW_PROFILE_INFO type design

type LPHW_PROFILE_INFO = Ptr HW_PROFILE_INFO

data HW_PROFILE_INFO = HW_PROFILE_INFO
     { dwDockInfo       :: DWORD
     , szHwProfileGuid  :: String -- Should we use GUID type instead of String?
     , szHwProfileName  :: String
     } deriving Show

instance Storable HW_PROFILE_INFO where
    sizeOf = const #{size HW_PROFILE_INFOW}
    alignment _ = #alignment HW_PROFILE_INFOW
    poke buf info = do
        (#poke HW_PROFILE_INFOW, dwDockInfo) buf (dwDockInfo info)
        withTString (szHwProfileGuid info) $ \szHwProfileGuid' ->
          (#poke HW_PROFILE_INFOW, szHwProfileGuid) buf szHwProfileGuid'
        withTString (szHwProfileName info) $ \szHwProfileName' ->
          (#poke HW_PROFILE_INFOW, szHwProfileName) buf szHwProfileName'

    peek buf = do
        dockInfo       <- (#peek HW_PROFILE_INFOW, dwDockInfo) buf
        hwProfileGuid  <- peekTString $ (#ptr HW_PROFILE_INFOW, szHwProfileGuid) buf
        hwProfileName  <- peekTString $ (#ptr HW_PROFILE_INFOW, szHwProfileName) buf
        return $ HW_PROFILE_INFO dockInfo hwProfileGuid hwProfileName

getCurrentHwProfile :: IO HW_PROFILE_INFO
getCurrentHwProfile =
  alloca $ \buf -> do
    failIfFalse_ "GetCurrentHwProfile"
      $ c_GetCurrentHwProfile buf
    peek buf

foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentHwProfileW"
  c_GetCurrentHwProfile :: LPHW_PROFILE_INFO -> IO Bool
-}

----------------------------------------------------------------
-- System metrics
----------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h GetSystemMetrics"
  getSystemMetrics :: SMSetting -> IO Int

#{enum SMSetting,
 , sM_CMONITORS           = SM_CMONITORS
 , sM_IMMENABLED          = SM_IMMENABLED
 , sM_MOUSEWHEELPRESENT   = SM_MOUSEWHEELPRESENT
 , sM_REMOTESESSION       = SM_REMOTESESSION
 , sM_SAMEDISPLAYFORMAT   = SM_SAMEDISPLAYFORMAT
 , sM_XVIRTUALSCREEN      = SM_XVIRTUALSCREEN
 , sM_YVIRTUALSCREEN      = SM_YVIRTUALSCREEN
 , sM_SERVERR2            = SM_SERVERR2
 , sM_MEDIACENTER         = SM_MEDIACENTER
 , sM_STARTER             = SM_STARTER
 , sM_TABLETPC            = SM_TABLETPC
 }

----------------------------------------------------------------
-- User name
----------------------------------------------------------------

-- | Get user name. See: <https://github.com/haskell/win32/issues/8>, <http://lpaste.net/41521>
getUserName :: IO String
getUserName =
  withTStringBuffer  maxLength  $ \buf ->
  with (fromIntegral maxLength) $ \len -> do
      failIfFalse_ "GetComputerName"
        $ c_GetUserName buf len
      -- GetUserNameW includes NUL charactor.
      peekTString buf
  where
    -- This requires Lmcons.h
    maxLength = #const UNLEN

foreign import WINDOWS_CCONV unsafe "windows.h GetUserNameW"
  c_GetUserName :: LPTSTR -> LPDWORD -> IO Bool

----------------------------------------------------------------
-- Processor features
----------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h IsProcessorFeaturePresent"
  isProcessorFeaturePresent :: ProcessorFeature -> IO BOOL

type ProcessorFeature   = DWORD

#{enum ProcessorFeature,
 , pF_3DNOW_INSTRUCTIONS_AVAILABLE     = PF_3DNOW_INSTRUCTIONS_AVAILABLE
 , pF_COMPARE_EXCHANGE_DOUBLE          = PF_COMPARE_EXCHANGE_DOUBLE
 , pF_FLOATING_POINT_EMULATED          = PF_FLOATING_POINT_EMULATED
 , pF_FLOATING_POINT_PRECISION_ERRATA  = PF_FLOATING_POINT_PRECISION_ERRATA
 , pF_MMX_INSTRUCTIONS_AVAILABLE       = PF_MMX_INSTRUCTIONS_AVAILABLE
 , pF_PAE_ENABLED                      = PF_PAE_ENABLED
 , pF_RDTSC_INSTRUCTION_AVAILABLE      = PF_RDTSC_INSTRUCTION_AVAILABLE
 , pF_XMMI_INSTRUCTIONS_AVAILABLE      = PF_XMMI_INSTRUCTIONS_AVAILABLE
 , pF_XMMI64_INSTRUCTIONS_AVAILABLE    = PF_XMMI64_INSTRUCTIONS_AVAILABLE
 }

{-
 , pF_CHANNELS_ENABLED                 = PF_CHANNELS_ENABLED
 , pF_NX_ENABLED                       = PF_NX_ENABLED
 , pF_COMPARE_EXCHANGE128              = PF_COMPARE_EXCHANGE128
 , pF_COMPARE64_EXCHANGE128            = PF_COMPARE64_EXCHANGE128
 , pF_SECOND_LEVEL_ADDRESS_TRANSLATION = PF_SECOND_LEVEL_ADDRESS_TRANSLATION
 , pF_SSE3_INSTRUCTIONS_AVAILABLE      = PF_SSE3_INSTRUCTIONS_AVAILABLE
 , pF_VIRT_FIRMWARE_ENABLED            = PF_VIRT_FIRMWARE_ENABLED
 , pF_XSAVE_ENABLED                    = PF_XSAVE_ENABLED
-}

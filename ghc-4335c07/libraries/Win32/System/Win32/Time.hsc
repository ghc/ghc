#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Time
-- Copyright   :  (c) Esa Ilari Vuokko, 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32 Time API.
--
-----------------------------------------------------------------------------
module System.Win32.Time where

import System.Win32.Types   ( DWORD, WORD, LONG, BOOL, failIf, failIf_, HANDLE
                            , peekTStringLen, LCID, LPTSTR, LPCTSTR, DDWORD
                            , LARGE_INTEGER, ddwordToDwords, dwordsToDdword )

import Control.Monad    ( when, liftM3, liftM )
import Data.Word        ( Word8 )
import Foreign          ( Storable(sizeOf, alignment, peekByteOff, peek,
                                   pokeByteOff, poke)
                        , Ptr, nullPtr, castPtr, plusPtr, advancePtr
                        , with, alloca, allocaBytes, copyArray )
import Foreign.C        ( CInt(..), CWchar(..)
                        , peekCWString, withCWStringLen, withCWString )
import Foreign.Marshal.Utils (maybeWith)

##include "windows_cconv.h"
#include <windows.h>
#include "alignment.h"

----------------------------------------------------------------
-- data types
----------------------------------------------------------------

newtype FILETIME = FILETIME DDWORD deriving (Show, Eq, Ord)

data SYSTEMTIME = SYSTEMTIME {
    wYear, wMonth, wDayOfWeek, wDay, wHour, wMinute, wSecond, wMilliseconds :: WORD }
    deriving (Show, Eq, Ord)

data TIME_ZONE_INFORMATION = TIME_ZONE_INFORMATION
    { tziBias :: LONG
    , tziStandardName :: String
    , tziStandardDate :: SYSTEMTIME
    , tziStandardBias :: LONG
    , tziDaylightName :: String
    , tziDaylightDate :: SYSTEMTIME
    , tziDaylightBias :: LONG
    } deriving (Show,Eq,Ord)

data TimeZoneId = TzIdUnknown | TzIdStandard | TzIdDaylight
    deriving (Show, Eq, Ord)

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance Storable FILETIME where
    sizeOf = const (#size FILETIME)
    alignment _ = #alignment FILETIME
    poke buf (FILETIME n) = do
        (#poke FILETIME, dwLowDateTime) buf low
        (#poke FILETIME, dwHighDateTime) buf hi
        where (hi,low) = ddwordToDwords n
    peek buf = do
        low <- (#peek FILETIME, dwLowDateTime) buf
        hi <- (#peek FILETIME, dwHighDateTime) buf
        return $ FILETIME $ dwordsToDdword (hi,low)

instance Storable SYSTEMTIME where
    sizeOf _ = #size SYSTEMTIME
    alignment _ = #alignment SYSTEMTIME
    poke buf st = do
         (#poke SYSTEMTIME, wYear)          buf (wYear st)
         (#poke SYSTEMTIME, wMonth)         buf (wMonth st)
         (#poke SYSTEMTIME, wDayOfWeek)     buf (wDayOfWeek st)
         (#poke SYSTEMTIME, wDay)           buf (wDay st)
         (#poke SYSTEMTIME, wHour)          buf (wHour st)
         (#poke SYSTEMTIME, wMinute)        buf (wMinute st)
         (#poke SYSTEMTIME, wSecond)        buf (wSecond st)
         (#poke SYSTEMTIME, wMilliseconds)  buf (wMilliseconds st)
    peek buf = do
        year    <- (#peek SYSTEMTIME, wYear)        buf
        month   <- (#peek SYSTEMTIME, wMonth)       buf
        dow     <- (#peek SYSTEMTIME, wDayOfWeek)   buf
        day     <- (#peek SYSTEMTIME, wDay)         buf
        hour    <- (#peek SYSTEMTIME, wHour)        buf
        mins    <- (#peek SYSTEMTIME, wMinute)      buf
        sec     <- (#peek SYSTEMTIME, wSecond)      buf
        ms      <- (#peek SYSTEMTIME, wMilliseconds) buf
        return $ SYSTEMTIME year month dow day hour mins sec ms

instance Storable TIME_ZONE_INFORMATION where
    sizeOf _ = (#size TIME_ZONE_INFORMATION)
    alignment _ = #alignment TIME_ZONE_INFORMATION
    poke buf tzi = do
        (#poke TIME_ZONE_INFORMATION, Bias) buf (tziBias tzi)
        (#poke TIME_ZONE_INFORMATION, StandardDate) buf (tziStandardDate tzi)
        (#poke TIME_ZONE_INFORMATION, StandardBias) buf (tziStandardBias tzi)
        (#poke TIME_ZONE_INFORMATION, DaylightDate) buf (tziDaylightDate tzi)
        (#poke TIME_ZONE_INFORMATION, DaylightBias) buf (tziDaylightBias tzi)
        write buf (#offset TIME_ZONE_INFORMATION, StandardName) (tziStandardName tzi)
        write buf (#offset TIME_ZONE_INFORMATION, DaylightName) (tziDaylightName tzi)
        where
            write buf_ offset str = withCWStringLen str $ \(c_str,len) -> do
                when (len>31) $ fail "Storable TIME_ZONE_INFORMATION.poke: Too long string."
                let len'  = len * sizeOf (undefined :: CWchar)
                    start = (advancePtr (castPtr buf_) offset)
                    end   = advancePtr start len'
                copyArray start (castPtr c_str :: Ptr Word8) len'
                poke (castPtr end) (0 :: CWchar)

    peek buf = do
        bias <- (#peek TIME_ZONE_INFORMATION, Bias)         buf
        sdat <- (#peek TIME_ZONE_INFORMATION, StandardDate) buf
        sbia <- (#peek TIME_ZONE_INFORMATION, StandardBias) buf
        ddat <- (#peek TIME_ZONE_INFORMATION, DaylightDate) buf
        dbia <- (#peek TIME_ZONE_INFORMATION, DaylightBias) buf
        snam <- peekCWString (plusPtr buf (#offset TIME_ZONE_INFORMATION, StandardName))
        dnam <- peekCWString (plusPtr buf (#offset TIME_ZONE_INFORMATION, DaylightName))
        return $ TIME_ZONE_INFORMATION bias snam sdat sbia dnam ddat dbia

foreign import WINDOWS_CCONV "windows.h GetSystemTime"
    c_GetSystemTime :: Ptr SYSTEMTIME -> IO ()
getSystemTime :: IO SYSTEMTIME
getSystemTime = alloca $ \res -> do
    c_GetSystemTime res
    peek res

foreign import WINDOWS_CCONV "windows.h SetSystemTime"
    c_SetSystemTime :: Ptr SYSTEMTIME -> IO BOOL
setSystemTime :: SYSTEMTIME -> IO ()
setSystemTime st = with st $ \c_st -> failIf_ not "setSystemTime: SetSystemTime" $
    c_SetSystemTime c_st

foreign import WINDOWS_CCONV "windows.h GetSystemTimeAsFileTime"
    c_GetSystemTimeAsFileTime :: Ptr FILETIME -> IO ()
getSystemTimeAsFileTime :: IO FILETIME
getSystemTimeAsFileTime = alloca $ \ret -> do
    c_GetSystemTimeAsFileTime ret
    peek ret

foreign import WINDOWS_CCONV "windows.h GetLocalTime"
    c_GetLocalTime :: Ptr SYSTEMTIME -> IO ()
getLocalTime :: IO SYSTEMTIME
getLocalTime = alloca $ \res -> do
    c_GetLocalTime res
    peek res

foreign import WINDOWS_CCONV "windows.h SetLocalTime"
    c_SetLocalTime :: Ptr SYSTEMTIME -> IO BOOL
setLocalTime :: SYSTEMTIME -> IO ()
setLocalTime st = with st $ \c_st -> failIf_ not "setLocalTime: SetLocalTime" $
    c_SetLocalTime c_st

foreign import WINDOWS_CCONV "windows.h GetSystemTimeAdjustment"
    c_GetSystemTimeAdjustment :: Ptr DWORD -> Ptr DWORD -> Ptr BOOL -> IO BOOL
getSystemTimeAdjustment :: IO (Maybe (Int, Int))
getSystemTimeAdjustment = alloca $ \ta -> alloca $ \ti -> alloca $ \enabled -> do
    failIf_ not "getSystemTimeAdjustment: GetSystemTimeAdjustment" $
        c_GetSystemTimeAdjustment ta ti enabled
    enabled' <- peek enabled
    if enabled'
        then do
            ta' <- peek ta
            ti' <- peek ti
            return $ Just (fromIntegral ta', fromIntegral ti')
        else return Nothing

foreign import WINDOWS_CCONV "windows.h GetTickCount" getTickCount :: IO DWORD

foreign import WINDOWS_CCONV "windows.h SetSystemTimeAdjustment"
    c_SetSystemTimeAdjustment :: DWORD -> BOOL -> IO BOOL
setSystemTimeAdjustment :: Maybe Int -> IO ()
setSystemTimeAdjustment ta =
    failIf_ not "setSystemTimeAjustment: SetSystemTimeAdjustment" $
        c_SetSystemTimeAdjustment time disabled
    where
        (time,disabled) = case ta of
            Nothing -> (0,True)
            Just x  -> (fromIntegral x,False)

foreign import WINDOWS_CCONV "windows.h GetTimeZoneInformation"
    c_GetTimeZoneInformation :: Ptr TIME_ZONE_INFORMATION -> IO DWORD
getTimeZoneInformation :: IO (TimeZoneId, TIME_ZONE_INFORMATION)
getTimeZoneInformation = alloca $ \tzi -> do
    tz <- failIf (==(#const TIME_ZONE_ID_INVALID)) "getTimeZoneInformation: GetTimeZoneInformation" $
        c_GetTimeZoneInformation tzi
    tzi' <- peek tzi
    return . flip (,) tzi' $ case tz of
        (#const TIME_ZONE_ID_UNKNOWN)   -> TzIdUnknown
        (#const TIME_ZONE_ID_STANDARD)  -> TzIdStandard
        (#const TIME_ZONE_ID_DAYLIGHT)  -> TzIdDaylight
        _                               -> TzIdUnknown   -- to remove warning

foreign import WINDOWS_CCONV "windows.h SystemTimeToFileTime"
    c_SystemTimeToFileTime :: Ptr SYSTEMTIME -> Ptr FILETIME -> IO BOOL
systemTimeToFileTime :: SYSTEMTIME -> IO FILETIME
systemTimeToFileTime s = with s $ \c_s -> alloca $ \ret -> do
    failIf_ not "systemTimeToFileTime: SystemTimeToFileTime" $
        c_SystemTimeToFileTime c_s ret
    peek ret

foreign import WINDOWS_CCONV "windows.h FileTimeToSystemTime"
    c_FileTimeToSystemTime :: Ptr FILETIME -> Ptr SYSTEMTIME -> IO BOOL
fileTimeToSystemTime :: FILETIME -> IO SYSTEMTIME
fileTimeToSystemTime s = with s $ \c_s -> alloca $ \ret -> do
    failIf_ not "fileTimeToSystemTime: FileTimeToSystemTime" $
        c_FileTimeToSystemTime c_s ret
    peek ret

foreign import WINDOWS_CCONV "windows.h GetFileTime"
    c_GetFileTime :: HANDLE -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> IO BOOL
getFileTime :: HANDLE -> IO (FILETIME,FILETIME,FILETIME)
getFileTime h = alloca $ \crt -> alloca $ \acc -> alloca $ \wrt -> do
    failIf_ not "getFileTime: GetFileTime" $ c_GetFileTime h crt acc wrt
    liftM3 (,,) (peek crt) (peek acc) (peek wrt)

foreign import WINDOWS_CCONV "windows.h SetFileTime"
    c_SetFileTime :: HANDLE -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> IO BOOL
setFileTime :: HANDLE -> FILETIME -> FILETIME -> FILETIME -> IO ()
setFileTime h crt acc wrt = with crt $
    \c_crt -> with acc $
    \c_acc -> with wrt $
    \c_wrt -> do
      failIf_ not "setFileTime: SetFileTime" $ c_SetFileTime h c_crt c_acc c_wrt

foreign import WINDOWS_CCONV "windows.h FileTimeToLocalFileTime"
    c_FileTimeToLocalFileTime :: Ptr FILETIME -> Ptr FILETIME -> IO BOOL
fileTimeToLocalFileTime :: FILETIME -> IO FILETIME
fileTimeToLocalFileTime ft = with ft $ \c_ft -> alloca $ \res -> do
    failIf_ not "fileTimeToLocalFileTime: FileTimeToLocalFileTime"
        $ c_FileTimeToLocalFileTime c_ft res
    peek res

foreign import WINDOWS_CCONV "windows.h LocalFileTimeToFileTime"
    c_LocalFileTimeToFileTime :: Ptr FILETIME -> Ptr FILETIME -> IO BOOL
localFileTimeToFileTime :: FILETIME -> IO FILETIME
localFileTimeToFileTime ft = with ft $ \c_ft -> alloca $ \res -> do
    failIf_ not "localFileTimeToFileTime: LocalFileTimeToFileTime"
        $ c_LocalFileTimeToFileTime c_ft res
    peek res

{-
-- Windows XP SP1
foreign import WINDOWS_CCONV "windows.h GetSystemTimes"
    c_GetSystemTimes :: Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> IO BOOL
getSystemTimes :: IO (FILETIME,FILETIME,FILETIME)
getSystemTimes = alloca $ \idle -> alloca $ \kernel -> alloca $ \user -> do
    failIf not "getSystemTimes: GetSystemTimes" $ c_GetSystemTimes idle kernel user
    liftM3 (,,) (peek idle) (peek kernel) (peek user)
-}

{-
-- Windows XP
foreign import WINDOWS_CCONV "windows.h SystemTimeToTzSpecificLocalTime"
    c_SystemTimeToTzSpecificLocalTime :: Ptr TIME_ZONE_INFORMATION -> Ptr SYSTEMTIME -> Ptr SYSTEMTIME -> IO BOOL
systemTimeToTzSpecificLocalTime :: TIME_ZONE_INFORMATION -> SYSTEMTIME -> IO SYSTEMTIME
systemTimeToTzSpecificLocalTime tzi st = with tzi $ \tzi -> with st $ \st -> alloca $ \res -> do
    failIf not "systemTimeToTzSpecificLocalTime: SystemTimeToTzSpecificLocalTime" $
        c_SystemTimeToTzSpecificLocalTime tzi st res
    peek res

foreign import WINDOWS_CCONV "windows.h TzSpecificLocalTimeToSystemTime"
    c_TzSpecificLocalTimeToSystemTime :: Ptr TIME_ZONE_INFORMATION -> Ptr SYSTEMTIME -> Ptr SYSTEMTIME -> IO BOOL
tzSpecificLocalTimeToSystemTime :: TIME_ZONE_INFORMATION -> SYSTEMTIME -> IO SYSTEMTIME
tzSpecificLocalTimeToSystemTime tzi st = with tzi $ \tzi -> with st $ \st -> alloca $ \res -> do
    failIf not "tzSpecificLocalTimeToSystemTime: TzSpecificLocalTimeToSystemTime" $
        c_TzSpecificLocalTimeToSystemTime tzi st res
    peek res
-}

foreign import WINDOWS_CCONV "windows.h QueryPerformanceFrequency"
    c_QueryPerformanceFrequency :: Ptr LARGE_INTEGER -> IO BOOL
queryPerformanceFrequency :: IO Integer
queryPerformanceFrequency = alloca $ \res -> do
    failIf_ not "queryPerformanceFrequency: QueryPerformanceFrequency" $
        c_QueryPerformanceFrequency res
    liftM fromIntegral $ peek res

foreign import WINDOWS_CCONV "windows.h QueryPerformanceCounter"
    c_QueryPerformanceCounter:: Ptr LARGE_INTEGER -> IO BOOL
queryPerformanceCounter:: IO Integer
queryPerformanceCounter= alloca $ \res -> do
    failIf_ not "queryPerformanceCounter: QueryPerformanceCounter" $
        c_QueryPerformanceCounter res
    liftM fromIntegral $ peek res

type GetTimeFormatFlags = DWORD
#{enum GetTimeFormatFlags,
    , lOCALE_NOUSEROVERRIDE = LOCALE_NOUSEROVERRIDE
    , lOCALE_USE_CP_ACP     = LOCALE_USE_CP_ACP
    , tIME_NOMINUTESORSECONDS = TIME_NOMINUTESORSECONDS
    , tIME_NOSECONDS        = TIME_NOSECONDS
    , tIME_NOTIMEMARKER     = TIME_NOTIMEMARKER
    , tIME_FORCE24HOURFORMAT= TIME_FORCE24HOURFORMAT
    }

foreign import WINDOWS_CCONV "windows.h GetTimeFormatW"
    c_GetTimeFormat :: LCID -> GetTimeFormatFlags -> Ptr SYSTEMTIME -> LPCTSTR -> LPTSTR -> CInt -> IO CInt
getTimeFormat :: LCID -> GetTimeFormatFlags -> Maybe SYSTEMTIME -> Maybe String -> IO String
getTimeFormat locale flags st fmt =
    maybeWith with st $ \c_st ->
    maybeWith withCWString fmt $ \c_fmt -> do
        size <- c_GetTimeFormat locale flags c_st c_fmt nullPtr 0
        allocaBytes ((fromIntegral size) * (sizeOf (undefined::CWchar))) $ \out -> do
            size' <- failIf (==0) "getTimeFormat: GetTimeFormat" $
                c_GetTimeFormat locale flags c_st c_fmt (castPtr out) size
            peekTStringLen (out,fromIntegral size')

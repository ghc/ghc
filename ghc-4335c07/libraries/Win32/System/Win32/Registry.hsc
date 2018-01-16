#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Registry
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for accessing the Win32 registry.
--
-----------------------------------------------------------------------------

module System.Win32.Registry
                ( module System.Win32.Registry
                ) where
{- What's really on offer:
        (
          regCloseKey        -- :: HKEY -> IO ()
        , regConnectRegistry -- :: Maybe String -> HKEY -> IO HKEY
        , regCreateKey       -- :: HKEY -> String -> IO HKEY
        , regCreateKeyEx     -- :: HKEY -> String -> Maybe String
                             -- -> RegCreateOptions -> REGSAM
                             -- -> Maybe LPSECURITY_ATTRIBUTES
                             -- -> IO (HKEY, Bool)
        , regDeleteKey       -- :: HKEY -> String -> IO ()
        , regDeleteValue     -- :: HKEY -> String -> IO ()
        , regEnumKeys        -- :: HKEY -> IO [String]
        , regEnumKey         -- :: HKEY -> DWORD -> Addr -> DWORD -> IO String
        , regEnumKeyValue    -- :: HKEY -> DWORD -> Addr -> DWORD -> Addr -> DWORD -> IO String
        , regFlushKey        -- :: HKEY -> IO ()
        , regLoadKey         -- :: HKEY -> String -> String -> IO ()
        , regNotifyChangeKeyValue -- :: HKEY -> Bool -> RegNotifyOptions
                                  -- -> HANDLE -> Bool -> IO ()
        , regOpenKey         -- :: HKEY -> String -> IO HKEY
        , regOpenKeyEx       -- :: HKEY -> String -> REGSAM -> IO HKEY
        , regQueryInfoKey    -- :: HKEY -> IO RegInfoKey
        , regQueryValue      -- :: HKEY -> String -> IO String
        , regQueryValueKey   -- :: HKEY -> String -> IO String
        , regQueryValueEx    -- :: HKEY -> String -> Addr -> Int -> IO RegValueType
        , regReplaceKey      -- :: HKEY -> Maybe String -> String -> String -> IO ()
        , regRestoreKey      -- :: HKEY -> String -> RegRestoreFlags -> IO ()
        , regSaveKey         -- :: HKEY -> String -> Maybe LPSECURITY_ATTRIBUTES -> IO ()
        , regSetValue        -- :: HKEY -> String -> String -> IO ()
        , regSetValueEx      -- :: HKEY -> String -> RegValueType -> LPTSTR -> Int -> IO ()
        , regSetStringValue  -- :: HKEY -> String -> String -> IO ()
        , regUnloadKey       -- :: HKEY -> String -> IO ()
        ) where
-}

{-
 Registry API omissions:

   RegQueryMultipleValues()
   RegEnumKeyEx()

-}

import Data.Word (Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, free, mallocBytes)
import Foreign.Marshal.Array (allocaArray0)
import Foreign.Marshal.Utils (maybeWith, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, peekByteOff, peekElemOff, sizeOf)
import System.IO.Unsafe (unsafePerformIO)
import System.Win32.File (LPSECURITY_ATTRIBUTES)
import System.Win32.Time (FILETIME)
import System.Win32.Types (DWORD, ErrCode, HKEY, LPCTSTR, PKEY, withTString)
import System.Win32.Types (HANDLE, LONG, LPBYTE, newForeignHANDLE, peekTString)
import System.Win32.Types (LPTSTR, TCHAR, failUnlessSuccess)
import System.Win32.Types (castUINTPtrToPtr, failUnlessSuccessOr, maybePtr)

##include "windows_cconv.h"

#include <windows.h>

#{enum HKEY, (unsafePerformIO . newForeignHANDLE . castUINTPtrToPtr)
 , hKEY_CLASSES_ROOT    = (UINT_PTR)HKEY_CLASSES_ROOT
 , hKEY_CURRENT_CONFIG  = (UINT_PTR)HKEY_CURRENT_CONFIG
 , hKEY_CURRENT_USER    = (UINT_PTR)HKEY_CURRENT_USER
 , hKEY_LOCAL_MACHINE   = (UINT_PTR)HKEY_LOCAL_MACHINE
 , hKEY_USERS           = (UINT_PTR)HKEY_USERS
 }
-- , PKEYERFORMANCE_DATA  NT only
-- , HKEY_DYN_DATA     95/98 only

regCloseKey :: HKEY -> IO ()
regCloseKey key =
  withForeignPtr key $ \ p_key ->
  failUnlessSuccess "RegCloseKey" $ c_RegCloseKey p_key
foreign import WINDOWS_CCONV unsafe "windows.h RegCloseKey"
  c_RegCloseKey :: PKEY -> IO ErrCode

-- Connects to a predefined registry handle on another computer.

regConnectRegistry :: Maybe String -> HKEY -> IO HKEY
regConnectRegistry mb_machine key =
  withForeignPtr key $ \ p_key ->
  maybeWith withTString mb_machine $ \ c_machine ->
  alloca $ \ p_out_key -> do
  failUnlessSuccess "RegConnectRegistry" $
    c_RegConnectRegistry c_machine p_key p_out_key
  p_new_key <- peek p_out_key
  newForeignHANDLE p_new_key
foreign import WINDOWS_CCONV unsafe "windows.h RegConnectRegistryW"
  c_RegConnectRegistry :: LPCTSTR -> PKEY -> Ptr PKEY -> IO ErrCode

regCreateKey :: HKEY -> String -> IO HKEY
regCreateKey key subkey =
  withForeignPtr key $ \ p_key ->
  withTString subkey $ \ c_subkey ->
  alloca $ \ p_out_key -> do
  failUnlessSuccess "RegCreateKey" $
    c_RegCreateKey p_key c_subkey p_out_key
  p_new_key <- peek p_out_key
  newForeignHANDLE p_new_key
foreign import WINDOWS_CCONV unsafe "windows.h RegCreateKeyW"
  c_RegCreateKey :: PKEY -> LPCTSTR -> Ptr PKEY -> IO ErrCode

type RegCreateOptions = DWORD

#{enum RegCreateOptions,
 , rEG_OPTION_NON_VOLATILE      = REG_OPTION_NON_VOLATILE
 , rEG_OPTION_VOLATILE          = REG_OPTION_VOLATILE
 }

type REGSAM = #{type REGSAM}

#{enum REGSAM,
 , kEY_ALL_ACCESS       = KEY_ALL_ACCESS
 , kEY_CREATE_LINK      = KEY_CREATE_LINK
 , kEY_CREATE_SUB_KEY   = KEY_CREATE_SUB_KEY
 , kEY_ENUMERATE_SUB_KEYS = KEY_ENUMERATE_SUB_KEYS
 , kEY_EXECUTE          = KEY_EXECUTE
 , kEY_NOTIFY           = KEY_NOTIFY
 , kEY_QUERY_VALUE      = KEY_QUERY_VALUE
 , kEY_READ             = KEY_READ
 , kEY_SET_VALUE        = KEY_SET_VALUE
 , kEY_WRITE            = KEY_WRITE
 }

regCreateKeyEx :: HKEY -> String -> Maybe String -> RegCreateOptions -> REGSAM -> Maybe LPSECURITY_ATTRIBUTES -> IO (HKEY, Bool)
regCreateKeyEx key subkey cls opts sam mb_attr =
  withForeignPtr key $ \ p_key ->
  withTString subkey $ \ c_subkey ->
  maybeWith withTString cls $ \ c_cls ->
  alloca $ \ p_res ->
  alloca $ \ p_disp -> do
  failUnlessSuccess "RegCreateKeyEx" $
    c_RegCreateKeyEx p_key c_subkey 0 c_cls opts sam (maybePtr mb_attr) p_res p_disp
  p_out_key <- peek p_res
  out_key <- newForeignHANDLE p_out_key
  disp <- peek p_disp
  return (out_key, disp == #{const REG_CREATED_NEW_KEY})
foreign import WINDOWS_CCONV unsafe "windows.h RegCreateKeyExW"
  c_RegCreateKeyEx :: PKEY -> LPCTSTR -> DWORD -> LPCTSTR -> RegCreateOptions -> REGSAM -> LPSECURITY_ATTRIBUTES -> Ptr PKEY -> Ptr DWORD -> IO ErrCode

regDeleteKey :: HKEY -> String -> IO ()
regDeleteKey key subkey =
  withForeignPtr key $ \ p_key ->
  withTString subkey $ \ c_subkey ->
  failUnlessSuccess "RegDeleteKey" $ c_RegDeleteKey p_key c_subkey
foreign import WINDOWS_CCONV unsafe "windows.h RegDeleteKeyW"
  c_RegDeleteKey :: PKEY -> LPCTSTR -> IO ErrCode

regDeleteValue :: HKEY -> String -> IO ()
regDeleteValue key name =
  withForeignPtr key $ \ p_key ->
  withTString name $ \ c_name ->
  failUnlessSuccess "RegDeleteValue" $ c_RegDeleteValue p_key c_name
foreign import WINDOWS_CCONV unsafe "windows.h RegDeleteValueW"
  c_RegDeleteValue :: PKEY -> LPCTSTR -> IO ErrCode

-- XXX Not 100% sure this is right, but I think it is.
-- Surely this function already exists somewhere?
mallocWideChars :: Int -> IO (Ptr a)
mallocWideChars i = mallocBytes (4 * i)

regEnumKeys :: HKEY -> IO [String]
regEnumKeys hkey = do
   hinfo <- regQueryInfoKey hkey
   let buflen = 1+max_subkey_len hinfo
   buf   <- mallocWideChars (fromIntegral buflen)
   ls    <- go 0 buf buflen
   free buf
   return ls
 where
   go n buf buflen = do
      (v,flg)  <- regEnumKey hkey n buf buflen
      if flg /= 0
       then return []
       else do
         vs <- go (n+1) buf buflen
         return (v:vs)

regEnumKeyVals :: HKEY -> IO [(String,String,RegValueType)]
regEnumKeyVals hkey = do
   hinfo <- regQueryInfoKey hkey
   let nmlen  = 1+max_value_name_len hinfo  -- add spc for terminating NUL.
   let vallen = 1+max_value_len hinfo
   nmbuf  <- mallocWideChars (fromIntegral nmlen)
   valbuf <- mallocWideChars (fromIntegral vallen)
   ls     <- go 0 nmbuf nmlen valbuf vallen
   free nmbuf
   free valbuf
   return ls
 where
   go n nmbuf nmlen valbuf vallen = do
      (ty,nm,flg) <- regEnumValue hkey n nmbuf nmlen valbuf vallen
      if flg /= 0
       then return []
       else do

        val <-
           case ty of
             x | x == rEG_SZ    -> peekTString (castPtr valbuf)
               | x == rEG_DWORD -> peekElemOff (castPtr valbuf) 0 >>= \ v -> return (show (v :: DWORD))
               | otherwise      -> return "<<unknown>>"

        vs <- go (n+1) nmbuf nmlen valbuf vallen
        return ((nm,val,ty):vs)

-- It's up to the programmer to ensure that a large enough
-- buffer is passed in here.

regEnumKey :: HKEY -> DWORD -> LPTSTR -> DWORD -> IO (String, Int)
regEnumKey key index c_name len =
  withForeignPtr key $ \ p_key -> do
  no_more <- failUnlessSuccessOr eRROR_NO_MORE_ITEMS "RegEnumKey" $
    c_RegEnumKey p_key index c_name len
  str <- peekTString c_name
  return (str, fromEnum no_more)
foreign import WINDOWS_CCONV unsafe "windows.h RegEnumKeyW"
  c_RegEnumKey :: PKEY -> DWORD -> LPTSTR -> DWORD -> IO ErrCode

regEnumValue :: HKEY -> DWORD -> LPTSTR -> DWORD -> LPBYTE -> DWORD -> IO (RegValueType, String, Int)
regEnumValue key index name name_len value value_len =
  withForeignPtr key $ \ p_key ->
  with name_len $ \ p_name_len ->
  with value_len $ \ p_value_len ->
  alloca $ \ p_reg_ty -> do
  no_more <- failUnlessSuccessOr eRROR_NO_MORE_ITEMS "RegEnumValue" $
    c_RegEnumValue p_key index name p_name_len nullPtr p_reg_ty value p_value_len
  reg_ty <- peek p_reg_ty
  str <- peekTString name
  return (reg_ty, str, fromEnum no_more)
foreign import WINDOWS_CCONV unsafe "windows.h RegEnumValueW"
  c_RegEnumValue :: PKEY -> DWORD -> LPTSTR -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> LPBYTE -> Ptr DWORD -> IO ErrCode

eRROR_NO_MORE_ITEMS :: ErrCode
eRROR_NO_MORE_ITEMS = #{const ERROR_NO_MORE_ITEMS}

regFlushKey :: HKEY -> IO ()
regFlushKey key =
  withForeignPtr key $ \ p_key ->
  failUnlessSuccess "RegFlushKey" $ c_RegFlushKey p_key
foreign import WINDOWS_CCONV unsafe "windows.h RegFlushKey"
  c_RegFlushKey :: PKEY -> IO ErrCode

-- ifdef FOR_WINDOWS_NT
-- RegGetKeySecurity :: HKEY -> SECURITY_INFORMATION -> IO SECURITY_DESCRIPTION

-- endif

regLoadKey :: HKEY -> String -> String -> IO ()
regLoadKey key subkey file =
  withForeignPtr key $ \ p_key ->
  withTString subkey $ \ c_subkey ->
  withTString file $ \ c_file ->
  failUnlessSuccess "RegLoadKey" $ c_RegLoadKey p_key c_subkey c_file
foreign import WINDOWS_CCONV unsafe "windows.h RegLoadKeyW"
  c_RegLoadKey :: PKEY -> LPCTSTR -> LPCTSTR -> IO ErrCode

-- ifdef FOR_WINDOWS_NT

type RegNotifyOptions = DWORD

#{enum RegNotifyOptions,
 , rEG_NOTIFY_CHANGE_NAME       = REG_NOTIFY_CHANGE_NAME
 , rEG_NOTIFY_CHANGE_ATTRIBUTES = REG_NOTIFY_CHANGE_ATTRIBUTES
 , rEG_NOTIFY_CHANGE_LAST_SET   = REG_NOTIFY_CHANGE_LAST_SET
 , rEG_NOTIFY_CHANGE_SECURITY   = REG_NOTIFY_CHANGE_SECURITY
 }

regNotifyChangeKeyValue :: HKEY -> Bool -> RegNotifyOptions -> HANDLE -> Bool -> IO ()
regNotifyChangeKeyValue key watch notifyFilter event async =
  withForeignPtr key $ \ p_key ->
  failUnlessSuccess "RegNotifyChangeKeyValue" $
    c_RegNotifyChangeKeyValue p_key watch notifyFilter event async
foreign import WINDOWS_CCONV unsafe "windows.h RegNotifyChangeKeyValue"
  c_RegNotifyChangeKeyValue :: PKEY -> Bool -> RegNotifyOptions -> HANDLE -> Bool -> IO ErrCode

-- endif

-- for Win 3.x compatibility, use RegOpenKeyEx instead.

regOpenKey :: HKEY -> String -> IO HKEY
regOpenKey key subkey =
  withForeignPtr key $ \ p_key ->
  withTString subkey $ \ c_subkey ->
  alloca $ \ p_res -> do
  failUnlessSuccess "RegOpenKey" $ c_RegOpenKey p_key c_subkey p_res
  p_res_key <- peek p_res
  newForeignHANDLE p_res_key
foreign import WINDOWS_CCONV unsafe "windows.h RegOpenKeyW"
  c_RegOpenKey :: PKEY -> LPCTSTR -> Ptr PKEY -> IO ErrCode

regOpenKeyEx :: HKEY -> String -> REGSAM -> IO HKEY
regOpenKeyEx key subkey sam =
  withForeignPtr key $ \ p_key ->
  withTString subkey $ \ c_subkey ->
  alloca $ \ p_res -> do
  failUnlessSuccess "RegOpenKeyEx" $ c_RegOpenKeyEx p_key c_subkey 0 sam p_res
  p_res_key <- peek p_res
  newForeignHANDLE p_res_key
foreign import WINDOWS_CCONV unsafe "windows.h RegOpenKeyExW"
  c_RegOpenKeyEx :: PKEY -> LPCTSTR -> DWORD -> REGSAM -> Ptr PKEY -> IO ErrCode

data RegInfoKey =
  RegInfoKey {
    class_string       :: String,
    class_id           :: Int,
    subkeys            :: Word32,
    max_subkey_len     :: Word32,
    max_class_len      :: Word32,
    values             :: Word32,
    max_value_name_len :: Word32,
    max_value_len      :: Word32,
    sec_len            :: Int,
    lastWrite_lo       :: Word32,
    lastWrite_hi       :: Word32
  }

regQueryInfoKey :: HKEY -> IO RegInfoKey
regQueryInfoKey key =
  withForeignPtr key $ \ p_key ->
  allocaBytes 100 $ \ c_class_string ->
  alloca $ \ p_class_id ->
  alloca $ \ p_subkeys ->
  alloca $ \ p_max_subkey_len ->
  alloca $ \ p_max_class_len ->
  alloca $ \ p_values ->
  alloca $ \ p_max_value_name_len ->
  alloca $ \ p_max_value_len ->
  alloca $ \ p_sec_len ->
  allocaBytes (#{size FILETIME}) $ \ p_lastWrite -> do
  failUnlessSuccess "RegQueryInfoKey" $
    c_RegQueryInfoKey p_key c_class_string p_class_id nullPtr p_subkeys
        p_max_subkey_len p_max_class_len p_values p_max_value_name_len
        p_max_value_len p_sec_len p_lastWrite
  class_string' <- peekTString c_class_string
  class_id' <- peek p_class_id
  subkeys' <- peek p_subkeys
  max_subkey_len' <- peek p_max_subkey_len
  max_class_len' <- peek p_max_class_len
  values' <- peek p_values
  max_value_name_len' <- peek p_max_value_name_len
  max_value_len' <- peek p_max_value_len
  sec_len' <- peek p_sec_len
  lastWrite_lo' <- #{peek FILETIME,dwLowDateTime} p_lastWrite
  lastWrite_hi' <- #{peek FILETIME,dwHighDateTime} p_lastWrite
  return $ RegInfoKey
    { class_string = class_string'
    , class_id = fromIntegral class_id'
    , subkeys = subkeys'
    , max_subkey_len = max_subkey_len'
    , max_class_len = max_class_len'
    , values = values'
    , max_value_name_len = max_value_name_len'
    , max_value_len = max_value_len'
    , sec_len = fromIntegral sec_len'
    , lastWrite_lo = lastWrite_lo'
    , lastWrite_hi = lastWrite_hi'
    }
foreign import WINDOWS_CCONV unsafe "windows.h RegQueryInfoKeyW"
  c_RegQueryInfoKey :: PKEY -> LPTSTR -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> Ptr FILETIME -> IO ErrCode

-- RegQueryMultipleValues :: HKEY -> IO ([VALENT],String)

-- RegQueryValue() isn't really that, it just allows you to
-- get at the default values of keys, so we provide our own
-- (and better!) version of it. If you want RegQueryValue()s
-- behaviour, use regQueryValueKey.

regQueryValueKey :: HKEY -> String -> IO String
regQueryValueKey key mb_subkey =
  withForeignPtr key $ \ p_key ->
  withTString mb_subkey $ \ c_subkey ->
  alloca $ \ p_value_len -> do
  failUnlessSuccess "RegQueryValue" $
    c_RegQueryValue p_key c_subkey nullPtr p_value_len
  value_len <- peek p_value_len
  allocaArray0 (fromIntegral value_len) $ \ c_value -> do
    failUnlessSuccess "RegQueryValue" $
      c_RegQueryValue p_key c_subkey c_value p_value_len
    peekTString c_value
foreign import WINDOWS_CCONV unsafe "windows.h RegQueryValueW"
  c_RegQueryValue :: PKEY -> LPCTSTR -> LPTSTR -> Ptr LONG -> IO ErrCode

regQueryValue :: HKEY -> String -> IO String
regQueryValue key mb_subkey =
  withForeignPtr key $ \ p_key ->
  withTString mb_subkey $ \ c_subkey ->
  alloca $ \ p_ty ->
  alloca $ \ p_value_len -> do
  failUnlessSuccess "RegQueryValue" $
    c_RegQueryValueEx p_key c_subkey nullPtr p_ty nullPtr p_value_len
  ty <- peek p_ty
  failUnlessSuccess "RegQueryValue" $ return (if ty == rEG_SZ then 0 else 1)
  value_len <- peek p_value_len
  allocaArray0 (fromIntegral value_len) $ \ c_value -> do
    failUnlessSuccess "RegQueryValue" $
      c_RegQueryValueEx p_key c_subkey nullPtr p_ty c_value p_value_len
    peekTString (castPtr c_value)

regQueryValueEx :: HKEY -> String -> LPBYTE -> Int -> IO RegValueType
regQueryValueEx key name value value_len =
  withForeignPtr key $ \ p_key ->
  withTString name $ \ c_name ->
  alloca $ \ p_ty ->
  with (fromIntegral value_len) $ \ p_value_len -> do
  failUnlessSuccess "RegQueryValueEx" $
    c_RegQueryValueEx p_key c_name nullPtr p_ty value p_value_len
  peek p_ty
foreign import WINDOWS_CCONV unsafe "windows.h RegQueryValueExW"
  c_RegQueryValueEx :: PKEY -> LPCTSTR -> Ptr DWORD -> Ptr DWORD -> LPBYTE -> Ptr DWORD -> IO ErrCode

regReplaceKey :: HKEY -> Maybe String -> String -> String -> IO ()
regReplaceKey key subkey newfile oldfile =
  withForeignPtr key $ \ p_key ->
  maybeWith withTString subkey $ \ c_subkey ->
  withTString newfile $ \ c_newfile ->
  withTString oldfile $ \ c_oldfile ->
  failUnlessSuccess "RegReplaceKey" $
    c_RegReplaceKey p_key c_subkey c_newfile c_oldfile
foreign import WINDOWS_CCONV unsafe "windows.h RegReplaceKeyW"
  c_RegReplaceKey :: PKEY -> LPCTSTR -> LPCTSTR -> LPCTSTR -> IO ErrCode

type RegRestoreFlags = DWORD

#{enum RegRestoreFlags,
 , rEG_WHOLE_HIVE_VOLATILE = REG_WHOLE_HIVE_VOLATILE
 , rEG_REFRESH_HIVE     = REG_REFRESH_HIVE
 , rEG_NO_LAZY_FLUSH    = REG_NO_LAZY_FLUSH
 }

regRestoreKey :: HKEY -> String -> RegRestoreFlags -> IO ()
regRestoreKey key file flags =
  withForeignPtr key $ \ p_key ->
  withTString file $ \ c_file ->
  failUnlessSuccess "RegRestoreKey" $ c_RegRestoreKey p_key c_file flags
foreign import WINDOWS_CCONV unsafe "windows.h RegRestoreKeyW"
  c_RegRestoreKey :: PKEY -> LPCTSTR -> RegRestoreFlags -> IO ErrCode

regSaveKey :: HKEY -> String -> Maybe LPSECURITY_ATTRIBUTES -> IO ()
regSaveKey key file mb_attr =
  withForeignPtr key $ \ p_key ->
  withTString file $ \ c_file ->
  failUnlessSuccess "RegSaveKey" $ c_RegSaveKey p_key c_file (maybePtr mb_attr)
foreign import WINDOWS_CCONV unsafe "windows.h RegSaveKeyW"
  c_RegSaveKey :: PKEY -> LPCTSTR -> LPSECURITY_ATTRIBUTES -> IO ErrCode

-- ifdef FOR_WINDOWS_NT

-- RegSetKeySecurity :: HKEY -> SECURITY_INFORMATION -> SECURITY_DESCRIPTOR -> IO ()

-- endif

-- 3.1 compat. - only allows storage of REG_SZ values.

regSetValue :: HKEY -> String -> String -> IO ()
regSetValue key subkey value =
  withForeignPtr key $ \ p_key ->
  withTString subkey $ \ c_subkey ->
  withTString value $ \ c_value ->
  failUnlessSuccess "RegSetValue" $
    c_RegSetValue p_key c_subkey rEG_SZ c_value 0 -- cbData is ignored, value needs to be null terminated.
foreign import WINDOWS_CCONV unsafe "windows.h RegSetValueW"
  c_RegSetValue :: PKEY -> LPCTSTR -> DWORD -> LPCTSTR -> Int -> IO ErrCode


type RegValueType = DWORD

#{enum RegValueType,
 , rEG_BINARY           = REG_BINARY
 , rEG_DWORD            = REG_DWORD
 , rEG_DWORD_LITTLE_ENDIAN = REG_DWORD_LITTLE_ENDIAN
 , rEG_DWORD_BIG_ENDIAN = REG_DWORD_BIG_ENDIAN
 , rEG_EXPAND_SZ        = REG_EXPAND_SZ
 , rEG_LINK             = REG_LINK
 , rEG_MULTI_SZ         = REG_MULTI_SZ
 , rEG_NONE             = REG_NONE
 , rEG_RESOURCE_LIST    = REG_RESOURCE_LIST
 , rEG_SZ               = REG_SZ
 }

-- regSetValueEx has a somewhat wieldly interface if all you want to do is
-- add a string value (a Common Thing to want to do), so we support this
-- specially:
regSetStringValue :: HKEY -> String -> String -> IO ()
regSetStringValue hk key val =
  withTString val $ \ v ->
  regSetValueEx hk key rEG_SZ v ((1+length val) * sizeOf (undefined::TCHAR))

regSetValueEx :: HKEY -> String -> RegValueType -> LPTSTR -> Int -> IO ()
regSetValueEx key subkey ty value value_len =
  withForeignPtr key $ \ p_key ->
  withTString subkey $ \ c_subkey ->
  failUnlessSuccess "RegSetValueEx" $
    c_RegSetValueEx p_key c_subkey 0 ty value value_len
foreign import WINDOWS_CCONV unsafe "windows.h RegSetValueExW"
  c_RegSetValueEx :: PKEY -> LPCTSTR -> DWORD -> RegValueType -> LPTSTR -> Int -> IO ErrCode

regUnLoadKey :: HKEY -> String -> IO ()
regUnLoadKey key subkey =
  withForeignPtr key $ \ p_key ->
  withTString subkey $ \ c_subkey ->
  failUnlessSuccess "RegUnLoadKey" $ c_RegUnLoadKey p_key c_subkey
foreign import WINDOWS_CCONV unsafe "windows.h RegUnLoadKeyW"
  c_RegUnLoadKey :: PKEY -> LPCTSTR -> IO ErrCode

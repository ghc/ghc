{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.HardLink
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Handling hard link using Win32 API. [NTFS only]

   Note: You should worry about file system type when use this module's function in your application:

     * NTFS only supprts this functionality.

     * ReFS doesn't support hard link currently.
-}
module System.Win32.HardLink
  ( module System.Win32.HardLink
  ) where
import System.Win32.File   ( LPSECURITY_ATTRIBUTES, failIfFalseWithRetry_ )
import System.Win32.String ( LPCTSTR, withTString )
import System.Win32.Types  ( BOOL, nullPtr )

#include "windows_cconv.h"

-- | NOTE: createHardLink is /flipped arguments/ to provide compatiblity for Unix.
-- 
-- If you want to create hard link by Windows way, use 'createHardLink'' instead.
createHardLink :: FilePath -- ^ Target file path
               -> FilePath -- ^ Hard link name
               -> IO ()
createHardLink = flip createHardLink'

createHardLink' :: FilePath -- ^ Hard link name
                -> FilePath -- ^ Target file path
                -> IO ()
createHardLink' link target =
   withTString target $ \c_target ->
   withTString link   $ \c_link ->
        failIfFalseWithRetry_ (unwords ["CreateHardLinkW",show link,show target]) $
          c_CreateHardLink c_link c_target nullPtr

foreign import WINDOWS_CCONV unsafe "windows.h CreateHardLinkW"
  c_CreateHardLink :: LPCTSTR -- ^ Hard link name
                   -> LPCTSTR -- ^ Target file path
                   -> LPSECURITY_ATTRIBUTES -- ^ This parameter is reserved. You should pass just /nullPtr/.
                   -> IO BOOL

{-
-- We plan to check file system type internally.

-- We are thinking about API design, currently...
data VolumeInformation = VolumeInformation
      { volumeName         :: String
      , volumeSerialNumber :: DWORD
      , maximumComponentLength :: DWORD
      , fileSystemFlags    :: DWORD
      , fileSystemName     :: String
      } deriving Show

getVolumeInformation :: Maybe String -> IO VolumeInformation
getVolumeInformation drive =
   maybeWith withTString drive $ \c_drive ->
   withTStringBufferLen 256    $ \(vnBuf, vnLen) ->
   alloca $ \serialNum ->
   alloca $ \maxLen ->
   alloca $ \fsFlags ->
   withTStringBufferLen 256 $ \(fsBuf, fsLen) -> do
       failIfFalse_ (unwords ["GetVolumeInformationW", drive]) $
         c_GetVolumeInformation c_drive vnBuf (fromIntegral vnLen)
                                serialNum maxLen fsFlags
                                fsBuf (fromIntegral fsLen)
       return VolumeInformation
         <*> peekTString vnBuf
         <*> peek serialNum
         <*> peek maxLen
         <*> peek fsFlags
         <*> peekTString fsBuf

-- Which is better?
getVolumeFileType :: String -> IO String
getVolumeFileType drive = fileSystemName <$> getVolumeInformation drive

getVolumeFileType :: String -> IO String
getVolumeFileType drive =
   withTString drive        $ \c_drive ->
   withTStringBufferLen 256 $ \(buf, len) -> do
       failIfFalse_ (unwords ["GetVolumeInformationW", drive]) $
         c_GetVolumeInformation c_drive nullPtr 0 nullPtr nullPtr nullPtr buf (fromIntegral len)
       peekTString buf

foreign import WINDOWS_CCONV unsafe "windows.h GetVolumeInformationW"
  c_GetVolumeInformation :: LPCTSTR -> LPTSTR -> DWORD -> LPDWORD -> LPDWORD -> LPDWORD -> LPTSTR -> DWORD -> IO BOOL
-}

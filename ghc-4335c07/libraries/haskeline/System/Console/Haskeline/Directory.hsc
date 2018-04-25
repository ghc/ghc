{- |
A Unicode-aware module for interacting with files.  We just need enough to support
filename completion.  In particular, these functions will silently handle all errors
(for example, file does not exist)
-}
module System.Console.Haskeline.Directory(
                    getDirectoryContents,
                    doesDirectoryExist,
                    getHomeDirectory
                    ) where

#ifdef MINGW

import Foreign
import Foreign.C
import System.Win32.Types
import qualified System.Directory

#include <windows.h>
#include <shlobj.h>
##include "windows_cconv.h"

foreign import WINDOWS_CCONV "FindFirstFileW" c_FindFirstFile
            :: LPCTSTR -> Ptr () -> IO HANDLE

foreign import WINDOWS_CCONV "FindNextFileW" c_FindNextFile
            :: HANDLE -> Ptr () -> IO Bool

foreign import WINDOWS_CCONV "FindClose" c_FindClose :: HANDLE -> IO BOOL

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents fp = allocaBytes (#size WIN32_FIND_DATA) $ \findP ->
    withCWString (fp ++ "\\*") $ \t_arr -> do
        h <- c_FindFirstFile t_arr findP
        if h == iNVALID_HANDLE_VALUE
            then return []
            else loop h findP
  where
    loop h findP = do
        f <- peekFileName findP
        isNext <- c_FindNextFile h findP
        if isNext
            then do {fs <- loop h findP; return (f:fs)}
            else c_FindClose h >> return [f]
    peekFileName = peekCWString . (#ptr WIN32_FIND_DATA, cFileName)

foreign import WINDOWS_CCONV "GetFileAttributesW" c_GetFileAttributes
            :: LPCTSTR -> IO DWORD

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist file = do
    attrs <- withCWString file c_GetFileAttributes
    return $ attrs /= (#const INVALID_FILE_ATTRIBUTES)
            && (attrs .&. (#const FILE_ATTRIBUTE_DIRECTORY)) /= 0

getHomeDirectory :: IO FilePath
getHomeDirectory = System.Directory.getHomeDirectory

#else

import System.Directory

#endif

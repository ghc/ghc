{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, CPP, ForeignFunctionInterface #-}

module Development.Shake.Internal.FileInfo(
    noFileHash, isNoFileHash,
    FileSize, ModTime, FileHash,
    getFileHash, getFileInfo
    ) where

#ifndef MIN_VERSION_unix
#define MIN_VERSION_unix(a,b,c) 0
#endif

#ifndef MIN_VERSION_time
#define MIN_VERSION_time(a,b,c) 0
#endif


import Data.Hashable
import Control.Exception.Extra
import Development.Shake.Classes
import Development.Shake.Internal.FileName
import qualified Data.ByteString.Lazy.Internal as LBS (defaultChunkSize)
import Data.List.Extra
import Data.Word
import Numeric
import System.IO
import Foreign

#if defined(PORTABLE)
import System.IO.Error
import System.Directory
import Data.Time

#elif defined(mingw32_HOST_OS)
import Development.Shake.Internal.Errors
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Foreign.C.String
import Data.Char

#else

#if MIN_VERSION_time(1,9,1)
import Data.Time.Clock
import Data.Fixed
#endif

import Development.Shake.Internal.Errors
import GHC.IO.Exception
import System.IO.Error
import System.Posix.Files.ByteString
#endif

-- A piece of file information, where 0 and 1 are special (see fileInfo* functions)
newtype FileInfo a = FileInfo Word32
    deriving (Typeable,Hashable,Binary,Storable,NFData)

noFileHash :: FileHash
noFileHash = FileInfo 1   -- Equal to nothing

isNoFileHash :: FileHash -> Bool
isNoFileHash (FileInfo i) = i == 1

fileInfo :: Word32 -> FileInfo a
fileInfo a = FileInfo $ if a > maxBound - 2 then a else a + 2

instance Show (FileInfo a) where
    show (FileInfo x)
        | x == 0 = "EQ"
        | x == 1 = "NEQ"
        | otherwise = "0x" ++ upper (showHex (x-2) "")

instance Eq (FileInfo a) where
    FileInfo a == FileInfo b
        | a == 0 || b == 0 = True
        | a == 1 || b == 1 = False
        | otherwise = a == b

data FileInfoHash; type FileHash = FileInfo FileInfoHash
data FileInfoMod ; type ModTime  = FileInfo FileInfoMod
data FileInfoSize; type FileSize = FileInfo FileInfoSize


getFileHash :: FileName -> IO FileHash
getFileHash x = withFile (fileNameToString x) ReadMode $ \h ->
    allocaBytes LBS.defaultChunkSize $ \ptr ->
        go h ptr (hash ())
    where
        go h ptr salt = do
            n <- hGetBufSome h ptr LBS.defaultChunkSize
            if n == 0 then
                pure $! fileInfo $ fromIntegral salt
            else
                go h ptr =<< hashPtrWithSalt ptr n salt



-- If the result isn't strict then we are referencing a much bigger structure,
-- and it causes a space leak I don't really understand on Linux when running
-- the 'tar' test, followed by the 'benchmark' test.
-- See this blog post: https://neilmitchell.blogspot.co.uk/2015/09/three-space-leaks.html
result :: Word32 -> Word32 -> IO (Maybe (ModTime, FileSize))
result x y = do
    x <- evaluate $ fileInfo x
    y <- evaluate $ fileInfo y
    pure $ Just (x, y)


-- | True = allow directory, False = disallow
getFileInfo :: Bool -> FileName -> IO (Maybe (ModTime, FileSize))

#if defined(PORTABLE)
-- Portable fallback
getFileInfo allowDir x = handleBool isDoesNotExistError (const $ pure Nothing) $ do
    let file = fileNameToString x
    time <- getModificationTime file
    size <- withFile file ReadMode hFileSize
    result (extractFileTime time) (fromIntegral size)

extractFileTime :: UTCTime -> Word32
extractFileTime = floor . fromRational . toRational . utctDayTime


#elif defined(mingw32_HOST_OS)
-- Directly against the Win32 API, twice as fast as the portable version
getFileInfo allowDir x = BS.useAsCString (fileNameToByteString x) $ \file ->
    alloca_WIN32_FILE_ATTRIBUTE_DATA $ \fad -> do
        res <- c_GetFileAttributesExA file 0 fad
        let peek = do
                code <- peekFileAttributes fad
                if not allowDir && testBit code 4 then
                    throwIO $ errorDirectoryNotFile $ fileNameToString x
                 else
                    join $ liftM2 result (peekLastWriteTimeLow fad) (peekFileSizeLow fad)
        if res then
            peek
         else if BS.any (>= chr 0x80) (fileNameToByteString x) then withCWString (fileNameToString x) $ \file -> do
            res <- c_GetFileAttributesExW file 0 fad
            if res then peek else pure Nothing
         else
            pure Nothing

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "Windows.h GetFileAttributesExA" c_GetFileAttributesExA :: CString  -> Int32 -> Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Bool
foreign import CALLCONV unsafe "Windows.h GetFileAttributesExW" c_GetFileAttributesExW :: CWString -> Int32 -> Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Bool

data WIN32_FILE_ATTRIBUTE_DATA

alloca_WIN32_FILE_ATTRIBUTE_DATA :: (Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO a) -> IO a
alloca_WIN32_FILE_ATTRIBUTE_DATA act = allocaBytes size_WIN32_FILE_ATTRIBUTE_DATA act
    where size_WIN32_FILE_ATTRIBUTE_DATA = 36

peekFileAttributes :: Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Word32
peekFileAttributes p = peekByteOff p index_WIN32_FILE_ATTRIBUTE_DATA_dwFileAttributes
    where index_WIN32_FILE_ATTRIBUTE_DATA_dwFileAttributes = 0

peekLastWriteTimeLow :: Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Word32
peekLastWriteTimeLow p = peekByteOff p index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime
    where index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime = 20

peekFileSizeLow :: Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Word32
peekFileSizeLow p = peekByteOff p index_WIN32_FILE_ATTRIBUTE_DATA_nFileSizeLow
    where index_WIN32_FILE_ATTRIBUTE_DATA_nFileSizeLow = 32


#else
-- Unix version
getFileInfo allowDir x = handleBool isDoesNotExistError' (const $ pure Nothing) $ do
    s <- getFileStatus $ fileNameToByteString x
    if not allowDir && isDirectory s then
        throwM $ errorDirectoryNotFile $ fileNameToString x
     else
        result (extractFileTime s) (fromIntegral $ fileSize s)
    where
        isDoesNotExistError' e =
            isDoesNotExistError e || ioeGetErrorType e == InappropriateType

extractFileTime :: FileStatus -> Word32
#if MIN_VERSION_unix(2,6,0)
#if MIN_VERSION_time(1,9,1)
extractFileTime = fromInteger . (\(MkFixed x) -> x) . nominalDiffTimeToSeconds . modificationTimeHiRes
#else
extractFileTime x = ceiling $ modificationTimeHiRes x * 1e4
#endif
#else
extractFileTime x = fromIntegral $ fromEnum $ modificationTime x
#endif

#endif

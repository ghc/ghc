{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Compat.Directory
-- Copyright   :  (c) The University of Glasgow 2001-2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions from System.Directory that aren't present in older versions
-- of that library.
--
-----------------------------------------------------------------------------

module Compat.Directory (
  	getAppUserDataDirectory,
  	copyFile,
  	findExecutable,
  	createDirectoryIfMissing
  ) where

#include "../../includes/ghcconfig.h"

import System.Environment (getEnv)
import System.Directory.Internals
#if __GLASGOW_HASKELL__ > 600
import Control.Exception	( bracket )
import Control.Monad		( when )
import Foreign.Marshal.Alloc	( allocaBytes )
import System.IO (IOMode(..), openBinaryFile, hGetBuf, hPutBuf, hClose)
import System.IO.Error		( try )
import GHC.IOBase ( IOException(..), IOErrorType(..) )
#else
import System.IO		( try )
#endif
#if __GLASGOW_HASKELL__ && defined(mingw32_HOST_OS)
import Foreign.Ptr
import Foreign.C
#endif
import System.Directory(doesFileExist, doesDirectoryExist, getPermissions, setPermissions, createDirectory)

getAppUserDataDirectory :: String -> IO FilePath
getAppUserDataDirectory appName = do
#if __GLASGOW_HASKELL__ && defined(mingw32_HOST_OS)
  allocaBytes long_path_size $ \pPath -> do
     r <- c_SHGetFolderPath nullPtr csidl_APPDATA nullPtr 0 pPath
     when (r<0) (raiseUnsupported "Compat.Directory.getAppUserDataDirectory")
     s <- peekCString pPath
     return (s++'\\':appName)
#else
  path <- getEnv "HOME"
  return (path++'/':'.':appName)
#endif

#if __GLASGOW_HASKELL__ && defined(mingw32_HOST_OS)
foreign import stdcall unsafe "directory.h __hscore_getFolderPath"
            c_SHGetFolderPath :: Ptr () 
                              -> CInt 
                              -> Ptr () 
                              -> CInt 
                              -> CString 
                              -> IO CInt

-- __compat_long_path_size defined in cbits/directory.c
foreign import ccall unsafe "directory.h __compat_long_path_size"
  long_path_size :: Int

foreign import ccall unsafe "directory.h __hscore_CSIDL_APPDATA"  csidl_APPDATA  :: CInt

raiseUnsupported loc = 
   ioError (IOError Nothing UnsupportedOperation loc "unsupported operation" Nothing)
#endif


copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFPath toFPath =
#if (!(defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 600))
	do readFile fromFPath >>= writeFile toFPath
	   try (getPermissions fromFPath >>= setPermissions toFPath)
	   return ()
#else
	(bracket (openBinaryFile fromFPath ReadMode) hClose $ \hFrom ->
	 bracket (openBinaryFile toFPath WriteMode) hClose $ \hTo ->
	 allocaBytes bufferSize $ \buffer -> do
		copyContents hFrom hTo buffer
		try (getPermissions fromFPath >>= setPermissions toFPath)
		return ()) `catch` (ioError . changeFunName)
	where
		bufferSize = 1024
		
		changeFunName (IOError h iot fun str mb_fp) = IOError h iot "copyFile" str mb_fp
		
		copyContents hFrom hTo buffer = do
			count <- hGetBuf hFrom buffer bufferSize
			when (count > 0) $ do
				hPutBuf hTo buffer count
				copyContents hFrom hTo buffer
#endif


findExecutable :: String -> IO (Maybe FilePath)
findExecutable binary = do
  path <- getEnv "PATH"
  search (parseSearchPath path)
  where
#ifdef mingw32_HOST_OS
    fileName = binary `joinFileExt` "exe"
#else
    fileName = binary
#endif

    search :: [FilePath] -> IO (Maybe FilePath)
    search [] = return Nothing
    search (d:ds) = do
	let path = d `joinFileName` fileName
	b <- doesFileExist path
	if b then return (Just path)
             else search ds

createDirectoryIfMissing :: Bool     -- ^ Create its parents too?
		         -> FilePath -- ^ The path to the directory you want to make
		         -> IO ()
createDirectoryIfMissing parents file = do
  b <- doesDirectoryExist file
  case (b,parents, file) of 
    (_,     _, "") -> return ()
    (True,  _,  _) -> return ()
    (_,  True,  _) -> mapM_ (createDirectoryIfMissing False) (tail (pathParents file))
    (_, False,  _) -> createDirectory file

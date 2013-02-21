{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Miscellaneous information about the system environment.
--
-----------------------------------------------------------------------------

module System.Environment
    (
      getArgs,
      getProgName,
      getExecutablePath,
      getEnv,
      lookupEnv,
      withArgs,
      withProgName,
#ifdef __GLASGOW_HASKELL__
      getEnvironment,
#endif
  ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import Foreign.Safe
import Foreign.C
import Control.Exception.Base   ( bracket )
-- import GHC.IO
import GHC.IO.Exception
import GHC.IO.Encoding (getFileSystemEncoding)
import qualified GHC.Foreign as GHC
import Data.List
#ifdef mingw32_HOST_OS
import GHC.Environment
import GHC.Windows
#else
import Control.Monad
#endif
#endif

#ifdef __HUGS__
import Hugs.System
#endif

import System.Environment.ExecutablePath

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

#ifdef __GLASGOW_HASKELL__
-- ---------------------------------------------------------------------------
-- getArgs, getProgName, getEnv

#ifdef mingw32_HOST_OS

-- Ignore the arguments to hs_init on Windows for the sake of Unicode compat

getWin32ProgArgv_certainly :: IO [String]
getWin32ProgArgv_certainly = do
        mb_argv <- getWin32ProgArgv
        case mb_argv of
          Nothing   -> fmap dropRTSArgs getFullArgs
          Just argv -> return argv

withWin32ProgArgv :: [String] -> IO a -> IO a
withWin32ProgArgv argv act = bracket begin setWin32ProgArgv (\_ -> act)
  where
    begin = do
          mb_old_argv <- getWin32ProgArgv
          setWin32ProgArgv (Just argv)
          return mb_old_argv

getWin32ProgArgv :: IO (Maybe [String])
getWin32ProgArgv = alloca $ \p_argc -> alloca $ \p_argv -> do
        c_getWin32ProgArgv p_argc p_argv
        argc <- peek p_argc
        argv_p <- peek p_argv
        if argv_p == nullPtr
         then return Nothing
         else do
          argv_ps <- peekArray (fromIntegral argc) argv_p
          fmap Just $ mapM peekCWString argv_ps

setWin32ProgArgv :: Maybe [String] -> IO ()
setWin32ProgArgv Nothing = c_setWin32ProgArgv 0 nullPtr
setWin32ProgArgv (Just argv) = withMany withCWString argv $ \argv_ps -> withArrayLen argv_ps $ \argc argv_p -> do
        c_setWin32ProgArgv (fromIntegral argc) argv_p

foreign import ccall unsafe "getWin32ProgArgv"
  c_getWin32ProgArgv :: Ptr CInt -> Ptr (Ptr CWString) -> IO ()

foreign import ccall unsafe "setWin32ProgArgv"
  c_setWin32ProgArgv :: CInt -> Ptr CWString -> IO ()

dropRTSArgs :: [String] -> [String]
dropRTSArgs []             = []
dropRTSArgs ("+RTS":rest)  = dropRTSArgs (dropWhile (/= "-RTS") rest)
dropRTSArgs ("--RTS":rest) = rest
dropRTSArgs ("-RTS":rest)  = dropRTSArgs rest
dropRTSArgs (arg:rest)     = arg : dropRTSArgs rest

#endif

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (not including the program name).
getArgs :: IO [String]

#ifdef mingw32_HOST_OS
getArgs =  fmap tail getWin32ProgArgv_certainly
#else
getArgs =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
   getProgArgv p_argc p_argv
   p    <- fromIntegral `liftM` peek p_argc
   argv <- peek p_argv
   enc <- getFileSystemEncoding
   peekArray (p - 1) (advancePtr argv 1) >>= mapM (GHC.peekCString enc)

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
#endif

{-|
Computation 'getProgName' returns the name of the program as it was
invoked.

However, this is hard-to-impossible to implement on some non-Unix
OSes, so instead, for maximum portability, we just return the leafname
of the program as invoked. Even then there are some differences
between platforms: on Windows, for example, a program invoked as foo
is probably really @FOO.EXE@, and that is what 'getProgName' will return.
-}
getProgName :: IO String
#ifdef mingw32_HOST_OS
-- Ignore the arguments to hs_init on Windows for the sake of Unicode compat
getProgName = fmap (basename . head) getWin32ProgArgv_certainly
#else
getProgName =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
     getProgArgv p_argc p_argv
     argv <- peek p_argv
     unpackProgName argv

unpackProgName  :: Ptr (Ptr CChar) -> IO String   -- argv[0]
unpackProgName argv = do
  enc <- getFileSystemEncoding
  s <- peekElemOff argv 0 >>= GHC.peekCString enc
  return (basename s)
#endif

basename :: FilePath -> FilePath
basename f = go f f
 where
  go acc [] = acc
  go acc (x:xs)
    | isPathSeparator x = go xs xs
    | otherwise         = go acc xs

  isPathSeparator :: Char -> Bool
  isPathSeparator '/'  = True
#ifdef mingw32_HOST_OS
  isPathSeparator '\\' = True
#endif
  isPathSeparator _    = False


-- | Computation 'getEnv' @var@ returns the value
-- of the environment variable @var@. For the inverse, POSIX users
-- can use 'System.Posix.Env.putEnv'.
--
-- This computation may fail with:
--
--  * 'System.IO.Error.isDoesNotExistError' if the environment variable
--    does not exist.

getEnv :: String -> IO String
getEnv name = lookupEnv name >>= maybe handleError return
  where
#ifdef mingw32_HOST_OS
    handleError = do
        err <- c_GetLastError
        if err == eRROR_ENVVAR_NOT_FOUND
            then ioe_missingEnvVar name
            else throwGetLastError "getEnv"

eRROR_ENVVAR_NOT_FOUND :: DWORD
eRROR_ENVVAR_NOT_FOUND = 203

foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
  c_GetLastError:: IO DWORD

#else
    handleError = ioe_missingEnvVar name
#endif

-- | Return the value of the environment variable @var@, or @Nothing@ if
-- there is no such value.
--
-- For POSIX users, this is equivalent to 'System.Posix.Env.getEnv'.
lookupEnv :: String -> IO (Maybe String)
#ifdef mingw32_HOST_OS
lookupEnv name = withCWString name $ \s -> try_size s 256
  where
    try_size s size = allocaArray (fromIntegral size) $ \p_value -> do
      res <- c_GetEnvironmentVariable s p_value size
      case res of
        0 -> return Nothing
        _ | res > size -> try_size s res -- Rare: size increased between calls to GetEnvironmentVariable
          | otherwise  -> peekCWString p_value >>= return . Just

foreign import WINDOWS_CCONV unsafe "windows.h GetEnvironmentVariableW"
  c_GetEnvironmentVariable :: LPWSTR -> LPWSTR -> DWORD -> IO DWORD
#else
lookupEnv name =
    withCString name $ \s -> do
      litstring <- c_getenv s
      if litstring /= nullPtr
        then do enc <- getFileSystemEncoding
                result <- GHC.peekCString enc litstring
                return $ Just result
        else return Nothing

foreign import ccall unsafe "getenv"
   c_getenv :: CString -> IO (Ptr CChar)
#endif

ioe_missingEnvVar :: String -> IO a
ioe_missingEnvVar name = ioException (IOError Nothing NoSuchThing "getEnv"
    "no environment variable" Nothing (Just name))

{-|
'withArgs' @args act@ - while executing action @act@, have 'getArgs'
return @args@.
-}
withArgs :: [String] -> IO a -> IO a
withArgs xs act = do
   p <- System.Environment.getProgName
   withArgv (p:xs) act

{-|
'withProgName' @name act@ - while executing action @act@,
have 'getProgName' return @name@.
-}
withProgName :: String -> IO a -> IO a
withProgName nm act = do
   xs <- System.Environment.getArgs
   withArgv (nm:xs) act

-- Worker routine which marshals and replaces an argv vector for
-- the duration of an action.

withArgv :: [String] -> IO a -> IO a

#ifdef mingw32_HOST_OS
-- We have to reflect the updated arguments in the RTS-side variables as
-- well, because the RTS still consults them for error messages and the like.
-- If we don't do this then ghc-e005 fails.
withArgv new_args act = withWin32ProgArgv new_args $ withProgArgv new_args act
#else
withArgv = withProgArgv
#endif

withProgArgv :: [String] -> IO a -> IO a
withProgArgv new_args act = do
  pName <- System.Environment.getProgName
  existing_args <- System.Environment.getArgs
  bracket (setProgArgv new_args)
          (\argv -> do _ <- setProgArgv (pName:existing_args)
                       freeProgArgv argv)
          (const act)

freeProgArgv :: Ptr CString -> IO ()
freeProgArgv argv = do
  size <- lengthArray0 nullPtr argv
  sequence_ [ peek (argv `advancePtr` i) >>= free
            | i <- [size - 1, size - 2 .. 0]]
  free argv

setProgArgv :: [String] -> IO (Ptr CString)
setProgArgv argv = do
  enc <- getFileSystemEncoding
  vs <- mapM (GHC.newCString enc) argv >>= newArray0 nullPtr
  c_setProgArgv (genericLength argv) vs
  return vs

foreign import ccall unsafe "setProgArgv"
  c_setProgArgv  :: CInt -> Ptr CString -> IO ()

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.
--
-- If an environment entry does not contain an @\'=\'@ character,
-- the @key@ is the whole entry and the @value@ is the empty string.
getEnvironment :: IO [(String, String)]

#ifdef mingw32_HOST_OS
getEnvironment = bracket c_GetEnvironmentStrings c_FreeEnvironmentStrings $ \pBlock ->
    if pBlock == nullPtr then return []
     else go pBlock
  where
    go pBlock = do
        -- The block is terminated by a null byte where there
        -- should be an environment variable of the form X=Y
        c <- peek pBlock
        if c == 0 then return []
         else do
          -- Seek the next pair (or terminating null):
          pBlock' <- seekNull pBlock False
          -- We now know the length in bytes, but ignore it when
          -- getting the actual String:
          str <- peekCWString pBlock
          fmap (divvy str :) $ go pBlock'

    -- Returns pointer to the byte *after* the next null
    seekNull pBlock done = do
        let pBlock' = pBlock `plusPtr` sizeOf (undefined :: CWchar)
        if done then return pBlock'
         else do
           c <- peek pBlock'
           seekNull pBlock' (c == (0 :: Word8 ))

foreign import WINDOWS_CCONV unsafe "windows.h GetEnvironmentStringsW"
  c_GetEnvironmentStrings :: IO (Ptr CWchar)

foreign import WINDOWS_CCONV unsafe "windows.h FreeEnvironmentStringsW"
  c_FreeEnvironmentStrings :: Ptr CWchar -> IO Bool
#else
getEnvironment = do
   pBlock <- getEnvBlock
   if pBlock == nullPtr then return []
    else do
      enc <- getFileSystemEncoding
      stuff <- peekArray0 nullPtr pBlock >>= mapM (GHC.peekCString enc)
      return (map divvy stuff)

foreign import ccall unsafe "__hscore_environ"
  getEnvBlock :: IO (Ptr CString)
#endif

divvy :: String -> (String, String)
divvy str =
  case break (=='=') str of
    (xs,[])        -> (xs,[]) -- don't barf (like Posix.getEnvironment)
    (name,_:value) -> (name,value)
#endif  /* __GLASGOW_HASKELL__ */

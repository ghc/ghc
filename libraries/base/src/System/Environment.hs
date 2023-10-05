{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

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
      executablePath,
      getExecutablePath,
      getEnv,
      lookupEnv,
      setEnv,
      unsetEnv,
      withArgs,
      withProgName,
      getEnvironment,
  ) where

import Foreign
import Foreign.C
import System.IO.Error (mkIOError)
import Control.Exception.Base (bracket_, throwIO)
#if defined(mingw32_HOST_OS)
import Control.Exception.Base (bracket)
#endif
-- import GHC.IO
import GHC.IO.Exception
import qualified GHC.Foreign as GHC
import Control.Monad
#if defined(mingw32_HOST_OS)
import GHC.IO.Encoding (argvEncoding)
import GHC.Windows
#else
import GHC.IO.Encoding (getFileSystemEncoding, argvEncoding)
import System.Posix.Internals (withFilePath)
#endif

import System.Environment.ExecutablePath

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

#include "HsBaseConfig.h"

-- ---------------------------------------------------------------------------
-- getArgs, getProgName, getEnv

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (not including the program name).
getArgs :: IO [String]
getArgs =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
   getProgArgv p_argc p_argv
   p    <- fromIntegral `liftM` peek p_argc
   argv <- peek p_argv
   enc <- argvEncoding
   peekArray (p - 1) (advancePtr argv 1) >>= mapM (GHC.peekCString enc)


foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

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
-- Ignore the arguments to hs_init on Windows for the sake of Unicode compat
getProgName =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
     getProgArgv p_argc p_argv
     argv <- peek p_argv
     unpackProgName argv

unpackProgName  :: Ptr (Ptr CChar) -> IO String   -- argv[0]
unpackProgName argv = do
  enc <- argvEncoding
  s <- peekElemOff argv 0 >>= GHC.peekCString enc
  return (basename s)

basename :: FilePath -> FilePath
basename f = go f f
 where
  go acc [] = acc
  go acc (x:xs)
    | isPathSeparator x = go xs xs
    | otherwise         = go acc xs

  isPathSeparator :: Char -> Bool
  isPathSeparator '/'  = True
#if defined(mingw32_HOST_OS)
  isPathSeparator '\\' = True
#endif
  isPathSeparator _    = False


-- | Computation 'getEnv' @var@ returns the value
-- of the environment variable @var@. For the inverse, the
-- `System.Environment.setEnv` function can be used.
--
-- This computation may fail with:
--
--  * 'System.IO.Error.isDoesNotExistError' if the environment variable
--    does not exist.

getEnv :: String -> IO String
getEnv name = lookupEnv name >>= maybe handleError return
  where
#if defined(mingw32_HOST_OS)
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
--
-- @since 4.6.0.0
lookupEnv :: String -> IO (Maybe String)
#if defined(mingw32_HOST_OS)
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

-- | @setEnv name value@ sets the specified environment variable to @value@.
--
-- Early versions of this function operated under the mistaken belief that
-- setting an environment variable to the /empty string/ on Windows removes
-- that environment variable from the environment.  For the sake of
-- compatibility, it adopted that behavior on POSIX.  In particular
--
-- @
-- setEnv name \"\"
-- @
--
-- has the same effect as
--
-- @
-- `unsetEnv` name
-- @
--
-- If you'd like to be able to set environment variables to blank strings,
-- use `System.Environment.Blank.setEnv`.
--
-- Throws `Control.Exception.IOException` if @name@ is the empty string or
-- contains an equals sign.
--
-- @since 4.7.0.0
setEnv :: String -> String -> IO ()
setEnv key_ value_
  | null key       = throwIO (mkIOError InvalidArgument "setEnv" Nothing Nothing)
  | '=' `elem` key = throwIO (mkIOError InvalidArgument "setEnv" Nothing Nothing)
  | null value     = unsetEnv key
  | otherwise      = setEnv_ key value
  where
    key   = takeWhile (/= '\NUL') key_
    value = takeWhile (/= '\NUL') value_

setEnv_ :: String -> String -> IO ()
#if defined(mingw32_HOST_OS)
setEnv_ key value = withCWString key $ \k -> withCWString value $ \v -> do
  success <- c_SetEnvironmentVariable k v
  unless success (throwGetLastError "setEnv")

foreign import WINDOWS_CCONV unsafe "windows.h SetEnvironmentVariableW"
  c_SetEnvironmentVariable :: LPTSTR -> LPTSTR -> IO Bool
#else

-- NOTE: The 'setenv()' function is not available on all systems, hence we use
-- 'putenv()'.  This leaks memory, but so do common implementations of
-- 'setenv()' (AFAIK).
setEnv_ k v = putEnv (k ++ "=" ++ v)

putEnv :: String -> IO ()
putEnv keyvalue = do
  s <- getFileSystemEncoding >>= (`GHC.newCString` keyvalue)
  -- IMPORTANT: Do not free `s` after calling putenv!
  --
  -- According to SUSv2, the string passed to putenv becomes part of the
  -- environment.
  throwErrnoIf_ (/= 0) "putenv" (c_putenv s)

foreign import ccall unsafe "putenv" c_putenv :: CString -> IO CInt
#endif

-- | @unsetEnv name@ removes the specified environment variable from the
-- environment of the current process.
--
-- Throws `Control.Exception.IOException` if @name@ is the empty string or
-- contains an equals sign.
--
-- @since 4.7.0.0
unsetEnv :: String -> IO ()
#if defined(mingw32_HOST_OS)
unsetEnv key = withCWString key $ \k -> do
  success <- c_SetEnvironmentVariable k nullPtr
  unless success $ do
    -- We consider unsetting an environment variable that does not exist not as
    -- an error, hence we ignore eRROR_ENVVAR_NOT_FOUND.
    err <- c_GetLastError
    unless (err == eRROR_ENVVAR_NOT_FOUND) $ do
      throwGetLastError "unsetEnv"
#else

#if defined(HAVE_UNSETENV)
unsetEnv key = withFilePath key (throwErrnoIf_ (/= 0) "unsetEnv" . c_unsetenv)
foreign import ccall unsafe "__hsbase_unsetenv" c_unsetenv :: CString -> IO CInt
#else
unsetEnv key = setEnv_ key ""
#endif

#endif

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
withArgv = withProgArgv

withProgArgv :: [String] -> IO a -> IO a
withProgArgv new_args act = do
  pName <- System.Environment.getProgName
  existing_args <- System.Environment.getArgs
  bracket_ (setProgArgv new_args)
           (setProgArgv (pName:existing_args))
           act

setProgArgv :: [String] -> IO ()
setProgArgv argv = do
  enc <- argvEncoding
  GHC.withCStringsLen enc argv $ \len css ->
    c_setProgArgv (fromIntegral len) css

-- setProgArgv copies the arguments
foreign import ccall unsafe "setProgArgv"
  c_setProgArgv  :: CInt -> Ptr CString -> IO ()

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.
--
-- If an environment entry does not contain an @\'=\'@ character,
-- the @key@ is the whole entry and the @value@ is the empty string.
getEnvironment :: IO [(String, String)]

#if defined(mingw32_HOST_OS)
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

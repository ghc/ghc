{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment.Blank
-- Copyright   :  (c) Habib Alamin 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A setEnv implementation that allows blank environment variables. Mimics
-- the `System.Posix.Env` module from the @unix@ package, but with support
-- for Windows too.
--
-- The matrix of platforms that:
--
--   * support putenv("FOO") to unset environment variables,
--   * support putenv("FOO=") to unset environment variables or set them
--     to blank values,
--   * support unsetenv to unset environment variables,
--   * support setenv to set environment variables,
--   * etc.
--
-- is very complicated. I think AIX is screwed, but we don't support it.
-- The whole situation with setenv(3), unsetenv(3), and putenv(3) is not
-- good. Even mingw32 adds its own crap to the pile, but luckily, we can
-- just use Windows' native environment functions to sidestep the issue.
--
-- #12494
--
-----------------------------------------------------------------------------

module System.Environment.Blank
    (
      getArgs,
      getProgName,
      getExecutablePath,
      getEnv,
      getEnvDefault,
      setEnv,
      unsetEnv,
      withArgs,
      withProgName,
      getEnvironment,
  ) where

import Foreign.C
#ifndef mingw32_HOST_OS
import System.Posix.Internals
#endif
import GHC.IO.Exception
import System.IO.Error
import Control.Exception.Base
import Data.Maybe

import qualified System.Environment as Environment

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

-- | `System.Environment.getArgs`.
getArgs           :: IO [String]
getArgs           = Environment.getArgs

-- | `System.Environment.getProgName`.
getProgName       :: IO String
getProgName       = Environment.getProgName

-- | `System.Environment.getExecutablePath`.
getExecutablePath :: IO FilePath
getExecutablePath = Environment.getExecutablePath

-- | `System.Environment.withArgs`.
withArgs          :: [String] -> IO a -> IO a
withArgs          = Environment.withArgs

-- | `System.Environment.withProgName`.
withProgName      :: String -> IO a -> IO a
withProgName      = Environment.withProgName


-- | `System.Environment.getEnvironment`.
getEnvironment    :: IO [(String, String)]
getEnvironment    = Environment.getEnvironment

-- | `System.Environment.lookupEnv`.
getEnv            :: String -> IO (Maybe String)
getEnv            = Environment.lookupEnv

-- | Get an environment value or a default value.
getEnvDefault ::
  String    {- ^ variable name                    -} ->
  String    {- ^ fallback value                   -} ->
  IO String {- ^ variable value or fallback value -}
getEnvDefault name fallback = fromMaybe fallback <$> getEnv name

-- | Like `System.Environment.setEnv`, but allows blank environment values
-- and mimics the function signature of `System.Posix.Env.setEnv` from the
-- @unix@ package.
setEnv ::
  String {- ^ variable name  -} ->
  String {- ^ variable value -} ->
  Bool   {- ^ overwrite      -} ->
  IO ()
setEnv key_ value_ overwrite
  | null key       = throwIO (mkIOError InvalidArgument "setEnv" Nothing Nothing)
  | '=' `elem` key = throwIO (mkIOError InvalidArgument "setEnv" Nothing Nothing)
  | otherwise      =
    if overwrite
    then setEnv_ key value
    else do
      env_var <- getEnv key
      case env_var of
          Just _  -> return ()
          Nothing -> setEnv_ key value
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
setEnv_ key value =
  withFilePath key $ \ keyP ->
    withFilePath value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum True))

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> IO CInt
#endif

-- | Like `System.Environment.unsetEnv`, but allows for the removal of
-- blank environment variables.
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
#elif HAVE_UNSETENV
# if !UNSETENV_RETURNS_VOID
unsetEnv name = withFilePath name $ \ s ->
  throwErrnoIfMinus1_ "unsetenv" (c_unsetenv s)

-- POSIX.1-2001 compliant unsetenv(3)
foreign import capi unsafe "HsBase.h unsetenv"
   c_unsetenv :: CString -> IO CInt
# else
unsetEnv name = withFilePath name c_unsetenv

-- pre-POSIX unsetenv(3) returning @void@
foreign import capi unsafe "HsBase.h unsetenv"
   c_unsetenv :: CString -> IO ()
# endif
#else
unsetEnv name =
  if '=' `elem` name
  then throwIO (mkIOError InvalidArgument "unsetEnv" Nothing Nothing)
  else putEnv name

putEnv :: String -> IO ()
putEnv keyvalue = do
  s <- getFileSystemEncoding >>= (`newCString` keyvalue)
  -- IMPORTANT: Do not free `s` after calling putenv!
  --
  -- According to SUSv2, the string passed to putenv becomes part of the
  -- environment. #7342
  throwErrnoIf_ (/= 0) "putenv" (c_putenv s)

foreign import ccall unsafe "putenv" c_putenv :: CString -> IO CInt
#endif

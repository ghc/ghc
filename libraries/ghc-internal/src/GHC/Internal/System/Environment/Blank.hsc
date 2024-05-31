{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.System.Environment.Blank
-- Copyright   :  (c) Habib Alamin 2017
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
--   * support @putenv("FOO")@ to unset environment variables,
--   * support @putenv("FOO=")@ to unset environment variables or set them
--     to blank values,
--   * support @unsetenv@ to unset environment variables,
--   * support @setenv@ to set environment variables,
--   * etc.
--
-- is very complicated. Some platforms don't support unsetting of environment
-- variables at all.
--
-----------------------------------------------------------------------------

module GHC.Internal.System.Environment.Blank
    (
      module GHC.Internal.System.Environment,
      getEnv,
      getEnvDefault,
      setEnv,
      unsetEnv,
  ) where

import GHC.Internal.Data.Functor
import GHC.Internal.Data.List (elem, null, takeWhile)
import GHC.Internal.Foreign.C.String
import GHC.Internal.Base
#if defined(mingw32_HOST_OS)
import GHC.Internal.Foreign.Ptr
import GHC.Internal.Windows
import GHC.Internal.Control.Monad
import GHC.Internal.Data.List (lookup)
#else
import GHC.Internal.Foreign.C.Error
import GHC.Internal.Foreign.C.Types
import GHC.Internal.Real (fromIntegral)
import GHC.Internal.Enum
import GHC.Internal.System.Posix.Internals
#endif
import GHC.Internal.IO.Exception
import GHC.Internal.System.IO.Error
import GHC.Internal.Control.Exception.Base
import GHC.Internal.Data.Maybe

import GHC.Internal.System.Environment
    (
      getArgs,
      getProgName,
      getExecutablePath,
      withArgs,
      withProgName,
      getEnvironment
  )
#if !defined(mingw32_HOST_OS)
import qualified GHC.Internal.System.Environment as Environment
#endif

#include "HsBaseConfig.h"

throwInvalidArgument :: String -> IO a
throwInvalidArgument from =
  throwIO (mkIOError InvalidArgument from Nothing Nothing)

-- | Similar to 'GHC.Internal.System.Environment.lookupEnv'.
getEnv :: String -> IO (Maybe String)
#if defined(mingw32_HOST_OS)
getEnv = (<$> getEnvironment) . lookup
#else
getEnv = Environment.lookupEnv
#endif

-- | Get an environment value or a default value.
getEnvDefault ::
  String    {- ^ variable name                    -} ->
  String    {- ^ fallback value                   -} ->
  IO String {- ^ variable value or fallback value -}
getEnvDefault name fallback = fromMaybe fallback <$> getEnv name

-- | Like 'GHC.Internal.System.Environment.setEnv', but allows blank environment values
-- and mimics the function signature of 'System.Posix.Env.setEnv' from the
-- @unix@ package.
--
-- Beware that this function must not be executed concurrently
-- with 'getEnv', 'lookupEnv', 'getEnvironment' and such. One thread
-- reading environment variables at the same time with another one modifying them
-- can result in a segfault, see
-- [Setenv is not Thread Safe](https://www.evanjones.ca/setenv-is-not-thread-safe.html)
-- for discussion.
setEnv ::
  String {- ^ variable name  -} ->
  String {- ^ variable value -} ->
  Bool   {- ^ overwrite      -} ->
  IO ()
setEnv key_ value_ overwrite
  | null key       = throwInvalidArgument "setEnv"
  | '=' `elem` key = throwInvalidArgument "setEnv"
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

foreign import ccall unsafe "windows.h SetEnvironmentVariableW"
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

-- | Like 'GHC.Internal.System.Environment.unsetEnv', but allows for the removal of
-- blank environment variables. May throw an exception if the underlying
-- platform doesn't support unsetting of environment variables.
--
-- Beware that this function must not be executed concurrently
-- with 'getEnv', 'lookupEnv', 'getEnvironment' and such. One thread
-- reading environment variables at the same time with another one modifying them
-- can result in a segfault, see
-- [Setenv is not Thread Safe](https://www.evanjones.ca/setenv-is-not-thread-safe.html)
-- for discussion.
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

eRROR_ENVVAR_NOT_FOUND :: DWORD
eRROR_ENVVAR_NOT_FOUND = 203

foreign import ccall unsafe "windows.h GetLastError"
  c_GetLastError:: IO DWORD
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
  then throwInvalidArgument "unsetEnv"
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

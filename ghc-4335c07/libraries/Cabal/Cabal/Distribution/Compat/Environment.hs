{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.Compat.Environment
       ( getEnvironment, lookupEnv, setEnv, unsetEnv )
       where

import Prelude ()
import qualified Prelude
import Distribution.Compat.Prelude

#ifndef mingw32_HOST_OS
#if __GLASGOW_HASKELL__ < 708
import Foreign.C.Error (throwErrnoIf_)
#endif
#endif

import qualified System.Environment as System
#if __GLASGOW_HASKELL__ >= 706
import System.Environment (lookupEnv)
#if __GLASGOW_HASKELL__ >= 708
import System.Environment (unsetEnv)
#endif
#else
import Distribution.Compat.Exception (catchIO)
#endif

import Distribution.Compat.Stack

#ifdef mingw32_HOST_OS
import Foreign.C
import GHC.Windows
#else
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error (throwErrnoIfMinus1_)
import System.Posix.Internals ( withFilePath )
#endif /* mingw32_HOST_OS */

getEnvironment :: NoCallStackIO [(String, String)]
#ifdef mingw32_HOST_OS
-- On Windows, the names of environment variables are case-insensitive, but are
-- often given in mixed-case (e.g. "PATH" is "Path"), so we have to normalise
-- them.
getEnvironment = fmap upcaseVars System.getEnvironment
  where
    upcaseVars = map upcaseVar
    upcaseVar (var, val) = (map toUpper var, val)
#else
getEnvironment = System.getEnvironment
#endif

#if __GLASGOW_HASKELL__ < 706
-- | @lookupEnv var@ returns the value of the environment variable @var@, or
-- @Nothing@ if there is no such value.
lookupEnv :: String -> IO (Maybe String)
lookupEnv name = (Just `fmap` System.getEnv name) `catchIO` const (return Nothing)
#endif /* __GLASGOW_HASKELL__ < 706 */

-- | @setEnv name value@ sets the specified environment variable to @value@.
--
-- Throws `Control.Exception.IOException` if either @name@ or @value@ is the
-- empty string or contains an equals sign.
setEnv :: String -> String -> IO ()
setEnv key value_ = setEnv_ key value
  where
    -- NOTE: Anything that follows NUL is ignored on both POSIX and Windows. We
    -- still strip it manually so that the null check above succeeds if a value
    -- starts with NUL.
    value = takeWhile (/= '\NUL') value_

setEnv_ :: String -> String -> IO ()

#ifdef mingw32_HOST_OS

setEnv_ key value = withCWString key $ \k -> withCWString value $ \v -> do
  success <- c_SetEnvironmentVariable k v
  unless success (throwGetLastError "setEnv")
 where
  _ = callStack -- TODO: attach CallStack to exception

# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif /* i386_HOST_ARCH */

foreign import WINDOWS_CCONV unsafe "windows.h SetEnvironmentVariableW"
  c_SetEnvironmentVariable :: LPTSTR -> LPTSTR -> Prelude.IO Bool
#else
setEnv_ key value = do
  withFilePath key $ \ keyP ->
    withFilePath value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum True))
 where
  _ = callStack -- TODO: attach CallStack to exception

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> Prelude.IO CInt
#endif /* mingw32_HOST_OS */

#if __GLASGOW_HASKELL__ < 708

-- | @unsetEnv name@ removes the specified environment variable from the
-- environment of the current process.
--
-- Throws `Control.Exception.IOException` if @name@ is the empty string or
-- contains an equals sign.
--
-- @since 4.7.0.0
unsetEnv :: String -> IO ()
#ifdef mingw32_HOST_OS
unsetEnv key = withCWString key $ \k -> do
  success <- c_SetEnvironmentVariable k nullPtr
  unless success $ do
    -- We consider unsetting an environment variable that does not exist not as
    -- an error, hence we ignore eRROR_ENVVAR_NOT_FOUND.
    err <- c_GetLastError
    unless (err == eRROR_ENVVAR_NOT_FOUND) $ do
      throwGetLastError "unsetEnv"
#else
unsetEnv key = withFilePath key (throwErrnoIf_ (/= 0) "unsetEnv" . c_unsetenv)
#if __GLASGOW_HASKELL__ > 706
foreign import ccall unsafe "__hsbase_unsetenv" c_unsetenv :: CString -> Prelude.IO CInt
#else
-- HACK: We hope very hard that !UNSETENV_RETURNS_VOID
foreign import ccall unsafe "unsetenv" c_unsetenv :: CString -> Prelude.IO CInt
#endif
#endif

#endif

{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Miscellaneous information about the system environment.
module System.Environment.Compat (
  getArgs
, getProgName
, getEnv
, lookupEnv
, setEnv
, unsetEnv
, withArgs
, withProgName
, getEnvironment
) where

import           System.Environment

#if !(MIN_VERSION_base(4,7,0))
import           Prelude.Compat

# ifdef mingw32_HOST_OS
import           Control.Monad
import           Foreign.C
import           Foreign.Safe
import           GHC.Windows
# else
import qualified System.Posix.Env as Posix
# endif

# ifdef mingw32_HOST_OS

#  if defined(i386_HOST_ARCH)
#   define WINDOWS_CCONV stdcall
#  elif defined(x86_64_HOST_ARCH)
#   define WINDOWS_CCONV ccall
#  else
#   error Unknown mingw32 arch
#  endif

foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
  c_GetLastError:: IO DWORD

eRROR_ENVVAR_NOT_FOUND :: DWORD
eRROR_ENVVAR_NOT_FOUND = 203

# endif

# if !(MIN_VERSION_base(4,6,0))
-- | Return the value of the environment variable @var@, or @Nothing@ if
-- there is no such value.
--
-- For POSIX users, this is equivalent to 'System.Posix.Env.getEnv'.
lookupEnv :: String -> IO (Maybe String)
lookupEnv k = lookup k `fmap` getEnvironment
# endif

-- | @setEnv name value@ sets the specified environment variable to @value@.
--
-- On Windows setting an environment variable to the /empty string/ removes
-- that environment variable from the environment.  For the sake of
-- compatibility we adopt that behavior.  In particular
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
-- If you don't care about Windows support and want to set an environment
-- variable to the empty string use @System.Posix.Env.setEnv@ from the @unix@
-- package instead.
--
-- Throws `Control.Exception.IOException` if @name@ is the empty string or
-- contains an equals sign.
-- 
-- Note that setting Unicode values may not work correctly on versions of GHC
-- prior to 7.2.
setEnv :: String -> String -> IO ()
setEnv key value_
  | null value = unsetEnv key
  | otherwise  = setEnv_ key value
  where
    -- NOTE: Anything that follows NUL is ignored on both POSIX and Windows.
    -- We still strip it manually so that the null check above succeds if a
    -- value starts with NUL, and `unsetEnv` is called.  This is important for
    -- two reasons.
    --
    --  * On POSIX setting an environment variable to the empty string does not
    --    remove it.
    --
    --  * On Windows setting an environment variable to the empty string
    --    removes that environment variable.  A subsequent call to
    --    GetEnvironmentVariable will then return 0, but the calling thread's
    --    last-error code will not be updated, and hence a call to GetLastError
    --    may not return ERROR_ENVVAR_NOT_FOUND.  The failed lookup will then
    --    result in a random error instead of the expected
    --    `isDoesNotExistError` (this is at least true for Windows XP, SP 3).
    --    Explicitly calling `unsetEnv` prevents this.
    value = takeWhile (/= '\NUL') value_

setEnv_ :: String -> String -> IO ()
# ifdef mingw32_HOST_OS
setEnv_ key value = withCWString key $ \k -> withCWString value $ \v -> do
  success <- c_SetEnvironmentVariable k v
  unless success (throwGetLastError "setEnv")

foreign import WINDOWS_CCONV unsafe "windows.h SetEnvironmentVariableW"
  c_SetEnvironmentVariable :: LPTSTR -> LPTSTR -> IO Bool
# else
setEnv_ k v = Posix.setEnv k v True
# endif

-- | @unsetEnv name@ removes the specified environment variable from the
-- environment of the current process.
--
-- Throws `Control.Exception.IOException` if @name@ is the empty string or
-- contains an equals sign.
unsetEnv :: String -> IO ()
# ifdef mingw32_HOST_OS
unsetEnv key = withCWString key $ \k -> do
  success <- c_SetEnvironmentVariable k nullPtr
  unless success $ do
    -- We consider unsetting an environment variable that does not exist not as
    -- an error, hence we ignore eRROR_ENVVAR_NOT_FOUND.
    err <- c_GetLastError
    unless (err == eRROR_ENVVAR_NOT_FOUND) $ do
      throwGetLastError "unsetEnv"
# else
unsetEnv = Posix.unsetEnv
# endif
#endif

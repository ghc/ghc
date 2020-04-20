{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GHC.SysTools.Terminal (stderrSupportsAnsiColors) where

import GHC.Prelude

#if defined(MIN_VERSION_terminfo)
import Control.Exception (catch)
import Data.Maybe (fromMaybe)
import System.Console.Terminfo (SetupTermError, Terminal, getCapability,
                                setupTermFromEnv, termColors)
import System.Posix (queryTerminal, stdError)
#elif defined(mingw32_HOST_OS)
import Control.Exception (catch, try)
import Data.Bits ((.|.), (.&.))
import Foreign (Ptr, peek, with)
import qualified Graphics.Win32 as Win32
import qualified System.Win32 as Win32
#endif

import System.IO.Unsafe

#if defined(mingw32_HOST_OS) && !defined(WINAPI)
# if defined(i386_HOST_ARCH)
#  define WINAPI stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINAPI ccall
# else
#  error unknown architecture
# endif
#endif

-- | Does the controlling terminal support ANSI color sequences?
-- This memoized to avoid thread-safety issues in ncurses (see #17922).
stderrSupportsAnsiColors :: Bool
stderrSupportsAnsiColors = unsafePerformIO stderrSupportsAnsiColors'
{-# NOINLINE stderrSupportsAnsiColors #-}

-- | Check if ANSI escape sequences can be used to control color in stderr.
stderrSupportsAnsiColors' :: IO Bool
stderrSupportsAnsiColors' = do
#if defined(MIN_VERSION_terminfo)
    stderr_available <- queryTerminal stdError
    if stderr_available then
      fmap termSupportsColors setupTermFromEnv
        `catch` \ (_ :: SetupTermError) -> pure False
    else
      pure False
  where
    termSupportsColors :: Terminal -> Bool
    termSupportsColors term = fromMaybe 0 (getCapability term termColors) > 0

#elif defined(mingw32_HOST_OS)
  h <- Win32.getStdHandle Win32.sTD_ERROR_HANDLE
         `catch` \ (_ :: IOError) ->
           pure Win32.nullHANDLE
  if h == Win32.nullHANDLE
    then pure False
    else do
      eMode <- try (getConsoleMode h)
      case eMode of
        Left (_ :: IOError) -> Win32.isMinTTYHandle h
                                 -- Check if the we're in a MinTTY terminal
                                 -- (e.g., Cygwin or MSYS2)
        Right mode
          | modeHasVTP mode -> pure True
          | otherwise       -> enableVTP h mode

  where

    enableVTP :: Win32.HANDLE -> Win32.DWORD -> IO Bool
    enableVTP h mode = do
        setConsoleMode h (modeAddVTP mode)
        modeHasVTP <$> getConsoleMode h
      `catch` \ (_ :: IOError) ->
        pure False

    modeHasVTP :: Win32.DWORD -> Bool
    modeHasVTP mode = mode .&. eNABLE_VIRTUAL_TERMINAL_PROCESSING /= 0

    modeAddVTP :: Win32.DWORD -> Win32.DWORD
    modeAddVTP mode = mode .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING

eNABLE_VIRTUAL_TERMINAL_PROCESSING :: Win32.DWORD
eNABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004

getConsoleMode :: Win32.HANDLE -> IO Win32.DWORD
getConsoleMode h = with 64 $ \ mode -> do
  Win32.failIfFalse_ "GetConsoleMode" (c_GetConsoleMode h mode)
  peek mode

setConsoleMode :: Win32.HANDLE -> Win32.DWORD -> IO ()
setConsoleMode h mode = do
  Win32.failIfFalse_ "SetConsoleMode" (c_SetConsoleMode h mode)

foreign import WINAPI unsafe "windows.h GetConsoleMode" c_GetConsoleMode
  :: Win32.HANDLE -> Ptr Win32.DWORD -> IO Win32.BOOL

foreign import WINAPI unsafe "windows.h SetConsoleMode" c_SetConsoleMode
  :: Win32.HANDLE -> Win32.DWORD -> IO Win32.BOOL

#else
   pure False
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GHC.SysTools.Terminal (stderrSupportsAnsiColors) where

import GHC.Prelude

#if !defined(mingw32_HOST_OS)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stderr)
#else
import GHC.IO (catchException)
import GHC.Utils.Exception (try)
import Foreign (Ptr, peek, with)
import qualified Graphics.Win32 as Win32
import qualified System.Win32 as Win32
#endif

import System.IO.Unsafe

-- | Does the controlling terminal support ANSI color sequences?
-- This memoized to avoid thread-safety issues in ncurses (see #17922).
stderrSupportsAnsiColors :: Bool
stderrSupportsAnsiColors = unsafePerformIO stderrSupportsAnsiColors'
{-# NOINLINE stderrSupportsAnsiColors #-}

-- | Check if ANSI escape sequences can be used to control color in stderr.
stderrSupportsAnsiColors' :: IO Bool
stderrSupportsAnsiColors' = do
#if !defined(mingw32_HOST_OS)
  -- Equivalent of https://hackage.haskell.org/package/ansi-terminal/docs/System-Console-ANSI.html#v:hSupportsANSI
  isTerminal <- hIsTerminalDevice stderr
  term <- lookupEnv "TERM"
  pure $ isTerminal && term /= Just "dumb"
#else
  h <- Win32.getStdHandle Win32.sTD_ERROR_HANDLE
         `catchException` \ (_ :: IOError) ->
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
      `catchException` \ (_ :: IOError) ->
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

foreign import ccall unsafe "windows.h GetConsoleMode" c_GetConsoleMode
  :: Win32.HANDLE -> Ptr Win32.DWORD -> IO Win32.BOOL

foreign import ccall unsafe "windows.h SetConsoleMode" c_SetConsoleMode
  :: Win32.HANDLE -> Win32.DWORD -> IO Win32.BOOL

#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SysTools.Terminal (stderrSupportsAnsiColors) where

import GhcPrelude

#if defined MIN_VERSION_terminfo
import Control.Exception (catch)
import Data.Maybe (fromMaybe)
import System.Console.Terminfo (SetupTermError, Terminal, getCapability,
                                setupTermFromEnv, termColors)
import System.Posix (queryTerminal, stdError)
#elif defined mingw32_HOST_OS
import Control.Exception (catch, try)
import Data.Bits ((.|.), (.&.))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Foreign (FunPtr, Ptr, allocaBytes, castPtrToFunPtr,
                peek, plusPtr, sizeOf, with)
import Foreign.C (CInt(..), CWchar, peekCWStringLen)
import qualified Graphics.Win32 as Win32
import qualified System.Win32 as Win32
#endif

#if defined mingw32_HOST_OS && !defined WINAPI
# if defined i386_HOST_ARCH
#  define WINAPI stdcall
# elif defined x86_64_HOST_ARCH
#  define WINAPI ccall
# else
#  error unknown architecture
# endif
#endif

-- | Check if ANSI escape sequences can be used to control color in stderr.
stderrSupportsAnsiColors :: IO Bool
stderrSupportsAnsiColors = do
#if defined MIN_VERSION_terminfo
  queryTerminal stdError `andM` do
    (termSupportsColors <$> setupTermFromEnv)
      `catch` \ (_ :: SetupTermError) ->
        pure False

  where

    andM :: Monad m => m Bool -> m Bool -> m Bool
    andM mx my = do
      x <- mx
      if x
        then my
        else pure x

    termSupportsColors :: Terminal -> Bool
    termSupportsColors term = fromMaybe 0 (getCapability term termColors) > 0

#elif defined mingw32_HOST_OS
  h <- Win32.getStdHandle Win32.sTD_ERROR_HANDLE
         `catch` \ (_ :: IOError) ->
           pure Win32.nullHANDLE
  if h == Win32.nullHANDLE
    then pure False
    else do
      eMode <- try (getConsoleMode h)
      case eMode of
        Left (_ :: IOError) -> queryCygwinTerminal h
        Right mode
          | modeHasVTP mode -> pure True
          | otherwise       -> enableVTP h mode

  where

    queryCygwinTerminal :: Win32.HANDLE -> IO Bool
    queryCygwinTerminal h = do
        fileType <- Win32.getFileType h
        if fileType /= Win32.fILE_TYPE_PIPE
          then pure False
          else do
            fn <- getFileNameByHandle h
            pure (("\\cygwin-" `isPrefixOf` fn || "\\msys-" `isPrefixOf` fn) &&
                  "-pty" `isInfixOf` fn &&
                  "-master" `isSuffixOf` fn)
      `catch` \ (_ :: IOError) ->
        pure False

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

fileNameInfo :: CInt
fileNameInfo = 2

mAX_PATH :: Num a => a
mAX_PATH = 260

getFileNameByHandle :: Win32.HANDLE -> IO String
getFileNameByHandle h = do
  let sizeOfDWORD = sizeOf (undefined :: Win32.DWORD)
  let sizeOfWchar = sizeOf (undefined :: CWchar)
  -- note: implicitly assuming that DWORD has stronger alignment than wchar_t
  let bufSize = sizeOfDWORD + mAX_PATH * sizeOfWchar
  allocaBytes bufSize $ \ buf -> do
    getFileInformationByHandleEx h fileNameInfo buf (fromIntegral bufSize)
    len :: Win32.DWORD <- peek buf
    let len' = fromIntegral len `div` sizeOfWchar
    peekCWStringLen (buf `plusPtr` sizeOfDWORD, min len' mAX_PATH)

getFileInformationByHandleEx
  :: Win32.HANDLE -> CInt -> Ptr a -> Win32.DWORD -> IO ()
getFileInformationByHandleEx h cls buf bufSize = do
  lib <- Win32.getModuleHandle (Just "kernel32.dll")
  ptr <- Win32.getProcAddress lib "GetFileInformationByHandleEx"
  let c_GetFileInformationByHandleEx =
        mk_GetFileInformationByHandleEx (castPtrToFunPtr ptr)
  Win32.failIfFalse_ "getFileInformationByHandleEx"
    (c_GetFileInformationByHandleEx h cls buf bufSize)

type F_GetFileInformationByHandleEx a =
  Win32.HANDLE -> CInt -> Ptr a -> Win32.DWORD -> IO Win32.BOOL

foreign import WINAPI "dynamic"
  mk_GetFileInformationByHandleEx
  :: FunPtr (F_GetFileInformationByHandleEx a)
  -> F_GetFileInformationByHandleEx a

#else
   pure False
#endif

{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Windows.Detect
  (
    ANSISupport (..)
  , ConsoleDefaultState (..)
  , aNSISupport
  , detectHandleSupportsANSI
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Exception (SomeException(..), throwIO, try)
import Data.Bits ((.&.), (.|.))
import System.Console.MinTTY (isMinTTYHandle)
import System.IO (Handle, hIsWritable, stdout)
import System.IO.Unsafe (unsafePerformIO)

import System.Console.ANSI.Windows.Foreign (ConsoleException(..),
  CONSOLE_SCREEN_BUFFER_INFO (..), DWORD, HANDLE, WORD,
  bACKGROUND_INTENSE_WHITE, eNABLE_VIRTUAL_TERMINAL_PROCESSING,
  fOREGROUND_INTENSE_WHITE, getConsoleMode, getConsoleScreenBufferInfo,
  iNVALID_HANDLE_VALUE, nullHANDLE, setConsoleMode, withHandleToHANDLE)

-- | The default state of the console.
data ConsoleDefaultState = ConsoleDefaultState
  { defaultForegroundAttributes :: WORD -- ^ Foreground attributes
  , defaultBackgroundAttributes :: WORD -- ^ Background attributes
  } deriving (Eq, Show)

-- | How the console is assumed to support ANSI control codes.
data ANSISupport
  = Native                       -- ^ Assume ANSI-enabled
  | Emulated ConsoleDefaultState -- ^ Not ANSI-enabled (including the state of
                                 -- the console when that status was determined)
  deriving (Eq, Show)

-- | Terminals on Windows
data Terminal
  = NativeANSIEnabled    -- ^ Windows 10 (Command Prompt or PowerShell)
  | NativeANSIIncapable  -- ^ Versions before Windows 10 (Command Prompt or
                         -- PowerShell)
  | Mintty               -- ^ ANSI-enabled
  | UnknownTerminal

-- | This function assumes that once it is first established whether or not the
-- Windows console requires emulation, that will not change. If the console
-- requires emulation, the state of the console is considered to be its default
-- state.
{-# NOINLINE aNSISupport #-}
aNSISupport :: ANSISupport
aNSISupport = unsafePerformIO $ withHandleToHANDLE stdout $ withHANDLE
  (throwIO $ ConsoleException 6)  -- Invalid handle or no handle
  (\h -> do
    terminal <- handleToTerminal h
    case terminal of
      NativeANSIIncapable -> Emulated <$> consoleDefaultState h
      _                   -> return Native)
 where
  consoleDefaultState h = do
    info <- getConsoleScreenBufferInfo h
    let attributes = csbi_attributes info
        fgAttributes = attributes .&. fOREGROUND_INTENSE_WHITE
        bgAttributes = attributes .&. bACKGROUND_INTENSE_WHITE
    return ConsoleDefaultState
      { defaultForegroundAttributes = fgAttributes
      , defaultBackgroundAttributes = bgAttributes }

-- | This function tests that the handle is writable. If what is attached to the
-- handle is not recognised as a known terminal, it returns @return Nothing@.
detectHandleSupportsANSI :: Handle -> IO (Maybe Bool)
detectHandleSupportsANSI handle = do
  isWritable <- hIsWritable handle
  if isWritable
    then withHandleToHANDLE handle $ withHANDLE
      (return $ Just False)  -- Invalid handle or no handle
      (\h -> do
        terminal <- handleToTerminal h
        case terminal of
          NativeANSIIncapable -> return (Just False)
          UnknownTerminal     -> return Nothing  -- Not sure!
          _                   -> return (Just True))
    else return (Just False)  -- Not an output handle

-- | This function assumes that the Windows handle is writable.
handleToTerminal :: HANDLE -> IO Terminal
handleToTerminal h = do
  tryMode <- try (getConsoleMode h) :: IO (Either SomeException DWORD)
  case tryMode of
    Left _     -> do  -- No ConHost mode
      isMinTTY <- isMinTTYHandle h
      if isMinTTY
        then return Mintty  -- 'mintty' terminal emulator
        else return UnknownTerminal  -- Not sure!
    Right mode -> if mode .&. eNABLE_VIRTUAL_TERMINAL_PROCESSING /= 0
      then return NativeANSIEnabled  -- VT processing already enabled
      else do
        let mode' = mode .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING
        trySetMode <- try (setConsoleMode h mode')
          :: IO (Either SomeException ())
        case trySetMode of
          Left _   -> return NativeANSIIncapable  -- Can't enable VT processing
          Right () -> return NativeANSIEnabled  -- VT processing enabled

-- | This function applies another to the Windows handle, if the handle is
-- valid. If it is invalid, the specified default action is returned.
withHANDLE :: IO a -> (HANDLE -> IO a) -> HANDLE -> IO a
withHANDLE invalid action h =
  if h == iNVALID_HANDLE_VALUE || h == nullHANDLE
    then invalid  -- Invalid handle or no handle
    else action h

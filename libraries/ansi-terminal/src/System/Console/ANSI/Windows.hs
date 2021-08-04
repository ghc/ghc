#include "Common-Safe-Haskell.hs"
{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Windows
  (
-- This file contains code that is common to modules
-- System.Console.ANSI.Unix and System.Console.ANSI.Windows, namely the module
-- exports and the associated Haddock documentation.
#include "Exports-Include.hs"
  ) where

import System.IO (Handle)

import System.Console.ANSI.Types
import qualified System.Console.ANSI.Unix as U
import System.Console.ANSI.Windows.Detect (ANSISupport (..),
  ConsoleDefaultState (..), aNSISupport)
import qualified System.Console.ANSI.Windows.Emulator as E

-- This file contains code that is common to modules System.Console.ANSI.Unix,
-- System.Console.ANSI.Windows and System.Console.ANSI.Windows.Emulator, such as
-- type signatures and the definition of functions specific to stdout in terms
-- of the corresponding more general functions, inclduding the related Haddock
-- documentation.
#include "Common-Include.hs"
-- This file contains code that is common save that different code is required
-- in the case of the module System.Console.ANSI.Windows.Emulator (see the file
-- Common-Include-Emulator.hs in respect of the latter).
#include "Common-Include-Enabled.hs"

-- | A helper function which returns the native or emulated version, depending
-- on `aNSISupport`.
nativeOrEmulated :: a -> a -> a
nativeOrEmulated native emulated = case aNSISupport of
  Native     -> native
  Emulated _ -> emulated

-- | A helper function which returns the native or emulated version, depending
-- on `aNSISupport`, where the emulator uses the default console state.
nativeOrEmulatedWithDefault :: a -> (ConsoleDefaultState -> a) -> a
nativeOrEmulatedWithDefault native emulated = case aNSISupport of
  Native       -> native
  Emulated def -> emulated def


-- * Cursor movement by character
hCursorUp       = nativeOrEmulated U.hCursorUp       E.hCursorUp
hCursorDown     = nativeOrEmulated U.hCursorDown     E.hCursorDown
hCursorForward  = nativeOrEmulated U.hCursorForward  E.hCursorForward
hCursorBackward = nativeOrEmulated U.hCursorBackward E.hCursorBackward

cursorUpCode :: Int -> String
cursorUpCode = nativeOrEmulated U.cursorUpCode E.cursorUpCode

cursorDownCode :: Int -> String
cursorDownCode = nativeOrEmulated U.cursorDownCode E.cursorDownCode

cursorForwardCode :: Int -> String
cursorForwardCode = nativeOrEmulated U.cursorForwardCode E.cursorForwardCode

cursorBackwardCode :: Int -> String
cursorBackwardCode = nativeOrEmulated U.cursorBackwardCode E.cursorBackwardCode

-- * Cursor movement by line
hCursorUpLine   = nativeOrEmulated U.hCursorUpLine   E.hCursorUpLine
hCursorDownLine = nativeOrEmulated U.hCursorDownLine E.hCursorDownLine

cursorUpLineCode :: Int -> String
cursorUpLineCode = nativeOrEmulated U.cursorUpLineCode E.cursorUpLineCode

cursorDownLineCode :: Int -> String
cursorDownLineCode = nativeOrEmulated U.cursorDownLineCode E.cursorDownLineCode

-- * Directly changing cursor position
hSetCursorColumn = nativeOrEmulated U.hSetCursorColumn E.hSetCursorColumn

setCursorColumnCode :: Int -> String
setCursorColumnCode = nativeOrEmulated
  U.setCursorColumnCode E.setCursorColumnCode

hSetCursorPosition = nativeOrEmulated U.hSetCursorPosition E.hSetCursorPosition

setCursorPositionCode :: Int -> Int -> String
setCursorPositionCode = nativeOrEmulated
  U.setCursorPositionCode E.setCursorPositionCode

-- * Saving, restoring and reporting cursor position
hSaveCursor = nativeOrEmulated U.hSaveCursor E.hSaveCursor
hRestoreCursor = nativeOrEmulated U.hRestoreCursor E.hRestoreCursor
hReportCursorPosition = nativeOrEmulated
  U.hReportCursorPosition E.hReportCursorPosition

saveCursorCode :: String
saveCursorCode = nativeOrEmulated U.saveCursorCode E.saveCursorCode

restoreCursorCode :: String
restoreCursorCode = nativeOrEmulated U.restoreCursorCode E.restoreCursorCode

reportCursorPositionCode :: String
reportCursorPositionCode = nativeOrEmulated
    U.reportCursorPositionCode E.reportCursorPositionCode

-- * Clearing parts of the screen
hClearFromCursorToScreenEnd = nativeOrEmulatedWithDefault
  U.hClearFromCursorToScreenEnd E.hClearFromCursorToScreenEnd
hClearFromCursorToScreenBeginning = nativeOrEmulatedWithDefault
  U.hClearFromCursorToScreenBeginning E.hClearFromCursorToScreenBeginning
hClearScreen = nativeOrEmulatedWithDefault U.hClearScreen E.hClearScreen

clearFromCursorToScreenEndCode :: String
clearFromCursorToScreenEndCode = nativeOrEmulated
  U.clearFromCursorToScreenEndCode E.clearFromCursorToScreenEndCode

clearFromCursorToScreenBeginningCode :: String
clearFromCursorToScreenBeginningCode = nativeOrEmulated
    U.clearFromCursorToScreenBeginningCode E.clearFromCursorToScreenBeginningCode

clearScreenCode :: String
clearScreenCode = nativeOrEmulated U.clearScreenCode E.clearScreenCode

hClearFromCursorToLineEnd = nativeOrEmulatedWithDefault
  U.hClearFromCursorToLineEnd E.hClearFromCursorToLineEnd
hClearFromCursorToLineBeginning = nativeOrEmulatedWithDefault
  U.hClearFromCursorToLineBeginning E.hClearFromCursorToLineBeginning
hClearLine = nativeOrEmulatedWithDefault U.hClearLine E.hClearLine

clearFromCursorToLineEndCode :: String
clearFromCursorToLineEndCode = nativeOrEmulated
  U.clearFromCursorToLineEndCode E.clearFromCursorToLineEndCode

clearFromCursorToLineBeginningCode :: String
clearFromCursorToLineBeginningCode = nativeOrEmulated
  U.clearFromCursorToLineBeginningCode E.clearFromCursorToLineBeginningCode

clearLineCode :: String
clearLineCode = nativeOrEmulated U.clearLineCode E.clearLineCode

-- * Scrolling the screen
hScrollPageUp   = nativeOrEmulatedWithDefault U.hScrollPageUp   E.hScrollPageUp
hScrollPageDown = nativeOrEmulatedWithDefault U.hScrollPageDown E.hScrollPageDown

scrollPageUpCode :: Int -> String
scrollPageUpCode = nativeOrEmulated U.scrollPageUpCode E.scrollPageUpCode

scrollPageDownCode :: Int -> String
scrollPageDownCode = nativeOrEmulated U.scrollPageDownCode E.scrollPageDownCode

-- * Select Graphic Rendition mode: colors and other whizzy stuff
--
-- The following SGR codes are NOT implemented by Windows 10 Threshold 2:
-- 2   SetConsoleIntensity FaintIntensity
-- 3   SetItalicized True
-- 5   SetBlinkSpeed SlowBlink
-- 6   SetBlinkSpeed RapidBlink
-- 8   SetVisible False
-- 21  SetUnderlining DoubleUnderline
-- 23  SetItalicized False
-- 25  SetBlinkSpeed NoBlink
-- 28  SetVisible True

hSetSGR = nativeOrEmulatedWithDefault U.hSetSGR E.hSetSGR

setSGRCode :: [SGR] -> String
setSGRCode = nativeOrEmulated U.setSGRCode E.setSGRCode

-- * Cursor visibilty changes
hHideCursor = nativeOrEmulated U.hHideCursor E.hHideCursor
hShowCursor = nativeOrEmulated U.hShowCursor E.hShowCursor

hideCursorCode :: String
hideCursorCode = nativeOrEmulated U.hideCursorCode E.hideCursorCode

showCursorCode :: String
showCursorCode = nativeOrEmulated U.showCursorCode E.showCursorCode

-- * Changing the title
hSetTitle = nativeOrEmulated U.hSetTitle E.hSetTitle

setTitleCode :: String -> String
setTitleCode = nativeOrEmulated U.setTitleCode E.setTitleCode

-- hSupportsANSI :: Handle -> IO Bool
-- (See Common-Include.hs for Haddock documentation)
hSupportsANSI = E.hSupportsANSI

-- hSupportsANSIWithoutEmulation :: Handle -> IO (Maybe Bool)
-- (See Common-Include.hs for Haddock documentation)
hSupportsANSIWithoutEmulation = E.hSupportsANSIWithoutEmulation

-- getReportedCursorPosition :: IO String
-- (See Common-Include.hs for Haddock documentation)
getReportedCursorPosition = E.getReportedCursorPosition

-- hGetCursorPosition :: Handle -> IO (Maybe (Int, Int))
-- (See Common-Include.hs for Haddock documentation)
hGetCursorPosition = E.hGetCursorPosition

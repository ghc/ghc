#include "Common-Safe-Haskell.hs"
{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Unix
  (
-- This file contains code that is common to modules
-- System.Console.ANSI.Unix and System.Console.ANSI.Windows, namely the module
-- exports and the associated Haddock documentation.
#include "Exports-Include.hs"
  ) where

import Data.Maybe (fromMaybe)
import Control.Exception.Base (bracket)
import Control.Monad (when)
import System.IO (BufferMode (..), Handle, hGetBuffering, hGetEcho,
  hIsTerminalDevice, hIsWritable, hPutStr, hReady, hSetBuffering, hSetEcho,
  stdin)
import System.Timeout (timeout)
import Text.ParserCombinators.ReadP (readP_to_S)

import System.Console.ANSI.Codes
import System.Console.ANSI.Types

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

hCursorUp h n = hPutStr h $ cursorUpCode n
hCursorDown h n = hPutStr h $ cursorDownCode n
hCursorForward h n = hPutStr h $ cursorForwardCode n
hCursorBackward h n = hPutStr h $ cursorBackwardCode n

hCursorDownLine h n = hPutStr h $ cursorDownLineCode n
hCursorUpLine h n = hPutStr h $ cursorUpLineCode n

hSetCursorColumn h n = hPutStr h $ setCursorColumnCode n
hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m

hSaveCursor h = hPutStr h saveCursorCode
hRestoreCursor h = hPutStr h restoreCursorCode
hReportCursorPosition h = hPutStr h reportCursorPositionCode

hClearFromCursorToScreenEnd h = hPutStr h clearFromCursorToScreenEndCode
hClearFromCursorToScreenBeginning h
    = hPutStr h clearFromCursorToScreenBeginningCode
hClearScreen h = hPutStr h clearScreenCode

hClearFromCursorToLineEnd h = hPutStr h clearFromCursorToLineEndCode
hClearFromCursorToLineBeginning h = hPutStr h clearFromCursorToLineBeginningCode
hClearLine h = hPutStr h clearLineCode

hScrollPageUp h n = hPutStr h $ scrollPageUpCode n
hScrollPageDown h n = hPutStr h $ scrollPageDownCode n

hSetSGR h sgrs = hPutStr h $ setSGRCode sgrs

hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode

hSetTitle h title = hPutStr h $ setTitleCode title

-- hSupportsANSI :: Handle -> IO Bool
-- (See Common-Include.hs for Haddock documentation)
--
-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> isNotDumb
 where
  -- cannot use lookupEnv since it only appeared in GHC 7.6
  isNotDumb = (/= Just "dumb") . lookup "TERM" <$> getEnvironment

-- hSupportsANSIWithoutEmulation :: Handle -> IO (Maybe Bool)
-- (See Common-Include.hs for Haddock documentation)
hSupportsANSIWithoutEmulation h =
  Just <$> ((&&) <$> hIsWritable h <*> hSupportsANSI h)

-- getReportedCursorPosition :: IO String
-- (See Common-Include.hs for Haddock documentation)
getReportedCursorPosition = do
  -- If, unexpectedly, no data is available on the console input stream then
  -- the timeout will prevent the getChar blocking. For consistency with the
  -- Windows equivalent, returns "" if the expected information is unavailable.
  fromMaybe "" <$> timeout 500000 get -- 500 milliseconds
 where
  get = do
    c <- getChar
    if c == '\ESC'
      then get' [c]
      else return [c] -- If the first character is not the expected \ESC then
                      -- give up. This provides a modicom of protection against
                      -- unexpected data in the input stream.
  get' s = do
    c <- getChar
    if c /= 'R'
      then get' (c:s) -- Continue building the list, until the expected 'R'
                      -- character is obtained. Build the list in reverse order,
                      -- in order to avoid O(n^2) complexity.
      else return $ reverse (c:s) -- Reverse the order of the built list.

-- hGetCursorPosition :: Handle -> IO (Maybe (Int, Int))
-- (See Common-Include.hs for Haddock documentation)
hGetCursorPosition h = fmap to0base <$> getCursorPosition'
 where
  to0base (row, col) = (row - 1, col - 1)
  getCursorPosition' = do
    input <- bracket (hGetBuffering stdin) (hSetBuffering stdin) $ \_ -> do
      -- set no buffering (if 'no buffering' is not already set, the contents of
      -- the buffer will be discarded, so this needs to be done before the
      -- cursor positon is emitted)
      hSetBuffering stdin NoBuffering
      -- ensure that echoing is off
      bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
        hSetEcho stdin False
        clearStdin
        hReportCursorPosition h
        hFlush h -- ensure the report cursor position code is sent to the
                 -- operating system
        getReportedCursorPosition
    case readP_to_S cursorPosition input of
      [] -> return Nothing
      [((row, col),_)] -> return $ Just (row, col)
      (_:_) -> return Nothing
  clearStdin = do
    isReady <- hReady stdin
    when isReady $ do
      _ <-getChar
      clearStdin

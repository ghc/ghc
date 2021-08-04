#include "Common-Safe-Haskell.hs"
{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Windows.Emulator.Codes
  (
    -- * Cursor movement by character
    cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode

    -- * Cursor movement by line
  , cursorUpLineCode, cursorDownLineCode

    -- * Directly changing cursor position
  , setCursorColumnCode, setCursorPositionCode

    -- * Saving, restoring and reporting cursor position
  , saveCursorCode, restoreCursorCode, reportCursorPositionCode

    -- * Clearing parts of the screen
  , clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode
  , clearScreenCode, clearFromCursorToLineEndCode
  , clearFromCursorToLineBeginningCode, clearLineCode

    -- * Scrolling the screen
  , scrollPageUpCode, scrollPageDownCode

    -- * Select Graphic Rendition mode: colors and other whizzy stuff
  , setSGRCode

    -- * Cursor visibilty changes
  , hideCursorCode, showCursorCode

    -- * Changing the title
  , setTitleCode
  ) where

import System.Console.ANSI.Types

cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode
  :: Int -- ^ Number of lines or characters to move
  -> String
cursorUpCode _       = ""
cursorDownCode _     = ""
cursorForwardCode _  = ""
cursorBackwardCode _ = ""

cursorDownLineCode, cursorUpLineCode :: Int -- ^ Number of lines to move
                                     -> String
cursorDownLineCode _ = ""
cursorUpLineCode _   = ""

setCursorColumnCode :: Int -- ^ 0-based column to move to
                    -> String
setCursorColumnCode _ = ""

setCursorPositionCode :: Int -- ^ 0-based row to move to
                      -> Int -- ^ 0-based column to move to
                      -> String
setCursorPositionCode _ _ = ""

saveCursorCode, restoreCursorCode, reportCursorPositionCode :: String
saveCursorCode = ""
restoreCursorCode = ""
reportCursorPositionCode = ""

clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode,
  clearScreenCode :: String
clearFromCursorToLineEndCode, clearFromCursorToLineBeginningCode,
  clearLineCode :: String

clearFromCursorToScreenEndCode       = ""
clearFromCursorToScreenBeginningCode = ""
clearScreenCode                      = ""
clearFromCursorToLineEndCode         = ""
clearFromCursorToLineBeginningCode   = ""
clearLineCode                        = ""

scrollPageUpCode, scrollPageDownCode :: Int -- ^ Number of lines to scroll by
                                     -> String
scrollPageUpCode _   = ""
scrollPageDownCode _ = ""

setSGRCode :: [SGR] -- ^ Commands: these will typically be applied on top of the
                    -- current console SGR mode. An empty list of commands is
                    -- equivalent to the list @[Reset]@. Commands are applied
                    -- left to right.
           -> String
setSGRCode _ = ""

hideCursorCode, showCursorCode :: String
hideCursorCode = ""
showCursorCode = ""

setTitleCode :: String -- ^ New title
             -> String
setTitleCode _ = ""

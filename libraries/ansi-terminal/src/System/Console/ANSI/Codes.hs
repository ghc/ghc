#include "Common-Safe-Haskell.hs"

{-| This module exports functions that return 'String' values containing codes
in accordance with the \'ANSI\' standards for control character sequences
described in the documentation of module "System.Console.ANSI".

The module "System.Console.ANSI" exports functions with the same names as those
in this module. On some versions of Windows, the terminal in use may not be
ANSI-capable. When that is the case, the same-named functions exported by module
"System.Console.ANSI" return \"\", for the reasons set out in the documentation
of that module.

Consequently, if module "System.Console.ANSI" is also imported, this module is
intended to be imported qualified, to avoid name clashes with those functions.
For example:

> import qualified System.Console.ANSI.Codes as ANSI
-}
module System.Console.ANSI.Codes
  (
    -- * Basic data types
    module System.Console.ANSI.Types

    -- * Cursor movement by character
  , cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode

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
    -- | Thanks to Brandon S. Allbery and Curt Sampson for pointing me in the
    -- right direction on xterm title setting on haskell-cafe. The "0"
    -- signifies that both the title and "icon" text should be set: i.e. the
    -- text for the window in the Start bar (or similar) as well as that in
    -- the actual window title. This is chosen for consistent behaviour
    -- between Unixes and Windows.
  , setTitleCode

    -- * Utilities
  , colorToCode, csi, sgrToCode
  ) where

import Data.List (intersperse)

import Data.Colour.SRGB (toSRGB24, RGB (..))

import System.Console.ANSI.Types

-- | 'csi' @parameters controlFunction@, where @parameters@ is a list of 'Int',
-- returns the control sequence comprising the control function CONTROL
-- SEQUENCE INTRODUCER (CSI) followed by the parameter(s) (separated by \';\')
-- and ending with the @controlFunction@ character(s) that identifies the
-- control function.
csi :: [Int]  -- ^ List of parameters for the control sequence
    -> String -- ^ Character(s) that identify the control function
    -> String
csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

-- | 'colorToCode' @color@ returns the 0-based index of the color (one of the
-- eight colors in the ANSI standard).
colorToCode :: Color -> Int
colorToCode color = case color of
  Black   -> 0
  Red     -> 1
  Green   -> 2
  Yellow  -> 3
  Blue    -> 4
  Magenta -> 5
  Cyan    -> 6
  White   -> 7

-- | 'sgrToCode' @sgr@ returns the parameter of the SELECT GRAPHIC RENDITION
-- (SGR) aspect identified by @sgr@.
sgrToCode :: SGR -- ^ The SGR aspect
          -> [Int]
sgrToCode sgr = case sgr of
  Reset -> [0]
  SetConsoleIntensity intensity -> case intensity of
    BoldIntensity   -> [1]
    FaintIntensity  -> [2]
    NormalIntensity -> [22]
  SetItalicized True  -> [3]
  SetItalicized False -> [23]
  SetUnderlining underlining -> case underlining of
    SingleUnderline -> [4]
    DoubleUnderline -> [21]
    NoUnderline     -> [24]
  SetBlinkSpeed blink_speed -> case blink_speed of
    SlowBlink   -> [5]
    RapidBlink  -> [6]
    NoBlink     -> [25]
  SetVisible False -> [8]
  SetVisible True  -> [28]
  SetSwapForegroundBackground True  -> [7]
  SetSwapForegroundBackground False -> [27]
  SetColor Foreground Dull color  -> [30 + colorToCode color]
  SetColor Foreground Vivid color -> [90 + colorToCode color]
  SetColor Background Dull color  -> [40 + colorToCode color]
  SetColor Background Vivid color -> [100 + colorToCode color]
  SetPaletteColor Foreground index -> [38, 5, fromIntegral index]
  SetPaletteColor Background index -> [48, 5, fromIntegral index]
  SetRGBColor Foreground color -> [38, 2] ++ toRGB color
  SetRGBColor Background color -> [48, 2] ++ toRGB color
  SetDefaultColor Foreground -> [39]
  SetDefaultColor Background -> [49]
 where
  toRGB color = let RGB r g b = toSRGB24 color
                in  map fromIntegral [r, g, b]

cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode
  :: Int -- ^ Number of lines or characters to move
  -> String
cursorUpCode n = csi [n] "A"
cursorDownCode n = csi [n] "B"
cursorForwardCode n = csi [n] "C"
cursorBackwardCode n = csi [n] "D"

cursorDownLineCode, cursorUpLineCode :: Int -- ^ Number of lines to move
                                     -> String
cursorDownLineCode n = csi [n] "E"
cursorUpLineCode n = csi [n] "F"

-- | Code to move the cursor to the specified column. The column numbering is
-- 0-based (that is, the left-most column is numbered 0).
setCursorColumnCode :: Int -- ^ 0-based column to move to
                    -> String
setCursorColumnCode n = csi [n + 1] "G"

-- | Code to move the cursor to the specified position (row and column). The
-- position is 0-based (that is, the top-left corner is at row 0 column 0).
setCursorPositionCode :: Int -- ^ 0-based row to move to
                      -> Int -- ^ 0-based column to move to
                      -> String
setCursorPositionCode n m = csi [n + 1, m + 1] "H"

-- | @since 0.7.1
saveCursorCode, restoreCursorCode :: String
saveCursorCode = "\ESC7"
restoreCursorCode = "\ESC8"

-- | Code to emit the cursor position into the console input stream, immediately
-- after being recognised on the output stream, as:
-- @ESC [ \<cursor row> ; \<cursor column> R@
--
-- Note that the information that is emitted is 1-based (the top-left corner is
-- at row 1 column 1) but 'setCursorPositionCode' is 0-based.
--
-- In isolation of 'getReportedCursorPosition' or 'getCursorPosition', this
-- function may be of limited use on Windows operating systems because of
-- difficulties in obtaining the data emitted into the console input stream.
-- The function 'hGetBufNonBlocking' in module "System.IO" does not work on
-- Windows. This has been attributed to the lack of non-blocking primatives in
-- the operating system (see the GHC bug report #806 at
-- <https://ghc.haskell.org/trac/ghc/ticket/806>).
--
-- @since 0.7.1
reportCursorPositionCode :: String

reportCursorPositionCode = csi [] "6n"

clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode,
  clearScreenCode :: String
clearFromCursorToLineEndCode, clearFromCursorToLineBeginningCode,
  clearLineCode :: String

clearFromCursorToScreenEndCode = csi [0] "J"
clearFromCursorToScreenBeginningCode = csi [1] "J"
clearScreenCode = csi [2] "J"
clearFromCursorToLineEndCode = csi [0] "K"
clearFromCursorToLineBeginningCode = csi [1] "K"
clearLineCode = csi [2] "K"

scrollPageUpCode, scrollPageDownCode :: Int -- ^ Number of lines to scroll by
                                     -> String
scrollPageUpCode n = csi [n] "S"
scrollPageDownCode n = csi [n] "T"

setSGRCode :: [SGR] -- ^ Commands: these will typically be applied on top of the
                    -- current console SGR mode. An empty list of commands is
                    -- equivalent to the list @[Reset]@. Commands are applied
                    -- left to right.
           -> String
setSGRCode sgrs = csi (concatMap sgrToCode sgrs) "m"

hideCursorCode, showCursorCode :: String
hideCursorCode = csi [] "?25l"
showCursorCode = csi [] "?25h"


-- | XTerm control sequence to set the Icon Name and Window Title.
setTitleCode :: String -- ^ New Icon Name and Window Title
             -> String
setTitleCode title = "\ESC]0;" ++ filter (/= '\007') title ++ "\007"

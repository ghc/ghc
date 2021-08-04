-- This file contains code that is common to modules
-- System.Console.ANSI.Unix and System.Console.ANSI.Windows, namely the module
-- exports and the associated Haddock documentation.

    -- * Basic data types
    module System.Console.ANSI.Types

    -- * Cursor movement by character
  , cursorUp
  , cursorDown
  , cursorForward
  , cursorBackward
  , hCursorUp
  , hCursorDown
  , hCursorForward
  , hCursorBackward
  , cursorUpCode
  , cursorDownCode
  , cursorForwardCode
  , cursorBackwardCode

    -- * Cursor movement by line
    -- | The difference between movements \"by character\" and \"by line\" is
    -- that @*Line@ functions additionally move the cursor to the start of the
    -- line, while functions like @cursorUp@ and @cursorDown@ keep the column
    -- the same.
    --
    -- Also keep in mind that @*Line@ functions are not as portable. See
    -- <https://github.com/feuerbach/ansi-terminal/issues/10> for the details.
  , cursorUpLine
  , cursorDownLine
  , hCursorUpLine
  , hCursorDownLine
  , cursorUpLineCode
  , cursorDownLineCode

    -- * Directly changing cursor position
  , setCursorColumn
  , hSetCursorColumn
  , setCursorColumnCode

  , setCursorPosition
  , hSetCursorPosition
  , setCursorPositionCode

    -- * Saving, restoring and reporting cursor position
  , saveCursor
  , hSaveCursor
  , saveCursorCode

  , restoreCursor
  , hRestoreCursor
  , restoreCursorCode

  , reportCursorPosition
  , hReportCursorPosition
  , reportCursorPositionCode

    -- * Clearing parts of the screen
    -- | Note that these functions only clear parts of the screen. They do not move the
    -- cursor.
  , clearFromCursorToScreenEnd
  , clearFromCursorToScreenBeginning
  , clearScreen
  , hClearFromCursorToScreenEnd
  , hClearFromCursorToScreenBeginning
  , hClearScreen
  , clearFromCursorToScreenEndCode
  , clearFromCursorToScreenBeginningCode
  , clearScreenCode

  , clearFromCursorToLineEnd
  , clearFromCursorToLineBeginning
  , clearLine
  , hClearFromCursorToLineEnd
  , hClearFromCursorToLineBeginning
  , hClearLine
  , clearFromCursorToLineEndCode
  , clearFromCursorToLineBeginningCode
  , clearLineCode

    -- * Scrolling the screen
  , scrollPageUp
  , scrollPageDown
  , hScrollPageUp
  , hScrollPageDown
  , scrollPageUpCode
  , scrollPageDownCode

    -- * Select Graphic Rendition mode: colors and other whizzy stuff
  , setSGR
  , hSetSGR
  , setSGRCode

    -- * Cursor visibilty changes
  , hideCursor
  , showCursor
  , hHideCursor
  , hShowCursor
  , hideCursorCode
  , showCursorCode

    -- * Changing the title
  , setTitle
  , hSetTitle
  , setTitleCode

    -- * Checking if handle supports ANSI (not portable: GHC only)
  , hSupportsANSI
  , hSupportsANSIColor
  , hSupportsANSIWithoutEmulation

    -- * Getting the cursor position
  , getCursorPosition
  , hGetCursorPosition
  , getReportedCursorPosition
  , cursorPosition

    -- * Getting the terminal size
  , getTerminalSize
  , hGetTerminalSize

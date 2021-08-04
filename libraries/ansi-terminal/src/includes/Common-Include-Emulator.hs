-- This file contains code that is required in the case of the module
-- System.Console.ANSI.Windows.Emulator and differs from the common code in
-- file Common-Include-Enabled.hs.

-- | Set the Select Graphic Rendition mode
hSetSGR
  :: ConsoleDefaultState -- ^ The default console state
  -> Handle
  -> [SGR] -- ^ Commands: these will typically be applied on top of the
           -- current console SGR mode. An empty list of commands is
           -- equivalent to the list @[Reset]@. Commands are applied left to
           -- right.
  -> IO ()

-- | Set the Select Graphic Rendition mode
setSGR
  :: ConsoleDefaultState -- ^ The default console state
  -> [SGR] -- ^ Commands: these will typically be applied on top of the
           -- current console SGR mode. An empty list of commands is
           -- equivalent to the list @[Reset]@. Commands are applied left to
           -- right.
  -> IO ()
setSGR def = hSetSGR def stdout

hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen
  :: ConsoleDefaultState -- ^ The default console state
  -> Handle
  -> IO ()

clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen
  :: ConsoleDefaultState -- ^ The default console state
  -> IO ()
clearFromCursorToScreenEnd def = hClearFromCursorToScreenEnd def stdout
clearFromCursorToScreenBeginning def
  = hClearFromCursorToScreenBeginning def stdout
clearScreen def = hClearScreen def stdout

hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine
  :: ConsoleDefaultState -- ^ The default console state
  -> Handle
  -> IO ()

clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine
  :: ConsoleDefaultState -- ^ The default console state
  -> IO ()
clearFromCursorToLineEnd def = hClearFromCursorToLineEnd def stdout
clearFromCursorToLineBeginning def = hClearFromCursorToLineBeginning def stdout
clearLine def = hClearLine def stdout

-- | Scroll the displayed information up or down the terminal: not widely
-- supported
hScrollPageUp, hScrollPageDown
  :: ConsoleDefaultState -- ^ The default console state
  -> Handle
  -> Int -- ^ Number of lines to scroll by
  -> IO ()

-- | Scroll the displayed information up or down the terminal: not widely
-- supported
scrollPageUp, scrollPageDown
  :: ConsoleDefaultState -- ^ The default console state
  -> Int -- ^ Number of lines to scroll by
  -> IO ()
scrollPageUp def = hScrollPageUp def stdout
scrollPageDown def = hScrollPageDown def stdout

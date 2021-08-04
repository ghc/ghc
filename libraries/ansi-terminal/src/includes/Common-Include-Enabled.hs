-- This file contains code that is common save that different code is required
-- in the case of the module System.Console.ANSI.Windows.Emulator (see the file
-- Common-Include-Emulator.hs in respect of the latter).

-- | Set the Select Graphic Rendition mode
hSetSGR
  :: Handle
  -> [SGR] -- ^ Commands: these will typically be applied on top of the
           -- current console SGR mode. An empty list of commands is
           -- equivalent to the list @[Reset]@. Commands are applied left to
           -- right.
  -> IO ()

-- | Set the Select Graphic Rendition mode
setSGR
  :: [SGR] -- ^ Commands: these will typically be applied on top of the
           -- current console SGR mode. An empty list of commands is
           -- equivalent to the list @[Reset]@. Commands are applied left to
           -- right.
  -> IO ()
setSGR = hSetSGR stdout

hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen
  :: Handle
  -> IO ()

clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen
  :: IO ()
clearFromCursorToScreenEnd = hClearFromCursorToScreenEnd stdout
clearFromCursorToScreenBeginning = hClearFromCursorToScreenBeginning stdout
clearScreen = hClearScreen stdout

hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine
  :: Handle
  -> IO ()

clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine
  :: IO ()
clearFromCursorToLineEnd = hClearFromCursorToLineEnd stdout
clearFromCursorToLineBeginning = hClearFromCursorToLineBeginning stdout
clearLine = hClearLine stdout

-- | Scroll the displayed information up or down the terminal: not widely
-- supported
hScrollPageUp, hScrollPageDown
  :: Handle
  -> Int -- ^ Number of lines to scroll by
  -> IO ()

-- | Scroll the displayed information up or down the terminal: not widely
-- supported
scrollPageUp, scrollPageDown
  :: Int -- ^ Number of lines to scroll by
  -> IO ()
scrollPageUp = hScrollPageUp stdout
scrollPageDown = hScrollPageDown stdout

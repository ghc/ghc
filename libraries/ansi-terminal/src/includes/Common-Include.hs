-- This file contains code that is common to modules System.Console.ANSI.Unix,
-- System.Console.ANSI.Windows and System.Console.ANSI.Windows.Emulator, such as
-- type signatures and the definition of functions specific to stdout in terms
-- of the corresponding more general functions, inclduding the related Haddock
-- documentation.

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
#endif

import Control.Monad (void)
import Data.Char (isDigit)
import System.Environment (getEnvironment)
import System.IO (hFlush, stdout)
import Text.ParserCombinators.ReadP (char, many1, ReadP, satisfy)

hCursorUp, hCursorDown, hCursorForward, hCursorBackward
  :: Handle
  -> Int -- ^ Number of lines or characters to move
  -> IO ()
cursorUp, cursorDown, cursorForward, cursorBackward
  :: Int -- ^ Number of lines or characters to move
  -> IO ()
cursorUp = hCursorUp stdout
cursorDown = hCursorDown stdout
cursorForward = hCursorForward stdout
cursorBackward = hCursorBackward stdout

hCursorDownLine, hCursorUpLine :: Handle
                               -> Int -- ^ Number of lines to move
                               -> IO ()
cursorDownLine, cursorUpLine :: Int -- ^ Number of lines to move
                             -> IO ()
cursorDownLine = hCursorDownLine stdout
cursorUpLine = hCursorUpLine stdout

hSetCursorColumn :: Handle
                 -> Int -- ^ 0-based column to move to
                 -> IO ()

-- | Move the cursor to the specified column. The column numbering is 0-based
-- (that is, the left-most column is numbered 0).
setCursorColumn :: Int -- ^ 0-based column to move to
                -> IO ()
setCursorColumn = hSetCursorColumn stdout

hSetCursorPosition :: Handle
                   -> Int -- ^ 0-based row to move to
                   -> Int -- ^ 0-based column to move to
                   -> IO ()

-- | Move the cursor to the specified position (row and column). The position is
-- 0-based (that is, the top-left corner is at row 0 column 0).
setCursorPosition :: Int -- ^ 0-based row to move to
                  -> Int -- ^ 0-based column to move to
                  -> IO ()
setCursorPosition = hSetCursorPosition stdout

hSaveCursor, hRestoreCursor, hReportCursorPosition :: Handle -> IO ()

-- | Save the cursor position in memory. The only way to access the saved value
-- is with the 'restoreCursor' command.
--
-- @since 0.7.1
saveCursor :: IO ()

-- | Restore the cursor position from memory. There will be no value saved in
-- memory until the first use of the 'saveCursor' command.
--
-- @since 0.7.1
restoreCursor :: IO ()

-- | Looking for a way to get the cursors position? See
-- 'getCursorPosition'.
--
-- Emit the cursor position into the console input stream, immediately after
-- being recognised on the output stream, as:
-- @ESC [ \<cursor row> ; \<cursor column> R@
--
-- Note that the information that is emitted is 1-based (the top-left corner is
-- at row 1 column 1) but 'setCursorColumn' and 'setCursorPosition' are
-- 0-based.
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
reportCursorPosition :: IO ()

saveCursor = hSaveCursor stdout
restoreCursor = hRestoreCursor stdout
reportCursorPosition = hReportCursorPosition stdout

hHideCursor, hShowCursor :: Handle
                         -> IO ()
hideCursor, showCursor :: IO ()
hideCursor = hHideCursor stdout
showCursor = hShowCursor stdout

-- | Set the terminal window title
hSetTitle :: Handle
          -> String -- ^ New title
          -> IO ()
-- | Set the terminal window title
setTitle :: String -- ^ New title
         -> IO ()
setTitle = hSetTitle stdout

-- | Use heuristics to determine whether the functions defined in this
-- package will work with a given handle. This function assumes that the handle
-- is writable (that is, it manages output - see 'hIsWritable').
--
-- For Unix-like operating systems, the current implementation checks
-- that: (1) the handle is a terminal; and (2) a @TERM@
-- environment variable is not set to @dumb@ (which is what the GNU Emacs text
-- editor sets for its integrated terminal).
--
-- For Windows, the current implementation performs the same checks as for
-- Unix-like operating systems and, as an alternative, checks whether the
-- handle is connected to a \'mintty\' terminal. (That is because the function
-- 'hIsTerminalDevice' is used to check if the handle is a
-- terminal. However, where a non-native Windows terminal (such as \'mintty\')
-- is implemented using redirection, that function will not identify a
-- handle to the terminal as a terminal.) On Windows 10, if the handle is
-- identified as connected to a native terminal, this function does /not/ enable
-- the processing of \'ANSI\' control characters in output (see
-- 'hSupportsANSIWithoutEmulation').
--
-- @since 0.6.2
hSupportsANSI :: Handle -> IO Bool

-- | Some terminals (e.g. Emacs) are not fully ANSI compliant but can support
-- ANSI colors. This can be used in such cases, if colors are all that is
-- needed.
--
-- @since 0.9
hSupportsANSIColor :: Handle -> IO Bool
hSupportsANSIColor h = (||) <$> hSupportsANSI h <*> isEmacsTerm
  where
    isEmacsTerm = (\env -> (insideEmacs env) && (isDumb env)) <$> getEnvironment
    insideEmacs env = any (\(k, _) -> k == "INSIDE_EMACS") env
    isDumb env = Just "dumb" == lookup "TERM" env

-- | Use heuristics to determine whether a given handle will support \'ANSI\'
-- control characters in output. (On Windows versions before Windows 10, that
-- means \'support without emulation\'.)
--
-- If the handle is not writable (that is, it cannot manage output - see
-- 'hIsWritable'), then @return (Just False)@ is returned.
--
-- On Unix-like operating systems, with one exception, the function is
-- consistent with 'hSupportsANSI'. The exception is if the handle is not
-- writable.
--
-- On Windows, what is returned will depend on what the handle is connected to
-- and the version of the operating system. If the handle is identified as
-- connected to a \'mintty\' terminal, @return (Just True)@ is
-- returned. If it is identifed as connected to a native terminal, then, on
-- Windows 10, the processing of \'ANSI\' control characters will be enabled and
-- @return (Just True)@ returned; and, on versions of Windows before Windows 10,
-- @return (Just False)@ is returned. Otherwise, if a @TERM@ environment
-- variable is set to @dumb@, @return (Just False)@ is returned. In all other
-- cases of a writable handle, @return Nothing@ is returned; this indicates that
-- the heuristics cannot assist - the handle may be connected to a file or
-- to another type of terminal.
--
-- @since 0.8.1
hSupportsANSIWithoutEmulation :: Handle -> IO (Maybe Bool)

-- | Parses the characters emitted by 'reportCursorPosition' into the console
-- input stream. Returns the cursor row and column as a tuple.
--
-- For example, if the characters emitted by 'reportCursorPosition' are in
-- 'String' @input@ then the parser could be applied like this:
--
-- > let result = readP_to_S cursorPosition input
-- > case result of
-- >     [] -> putStrLn $ "Error: could not parse " ++ show input
-- >     [((row, column), _)] -> putStrLn $ "The cursor was at row " ++ show row
-- >         ++ " and column" ++ show column ++ "."
-- >     (_:_) -> putStrLn $ "Error: parse not unique"
--
-- @since 0.7.1
cursorPosition :: ReadP (Int, Int)
cursorPosition = do
  void $ char '\ESC'
  void $ char '['
  row <- decimal -- A non-negative whole decimal number
  void $ char ';'
  col <- decimal -- A non-negative whole decimal number
  void $ char 'R'
  return (read row, read col)
 where
  digit = satisfy isDigit
  decimal = many1 digit

-- | Attempts to get the reported cursor position data from the console input
-- stream. The function is intended to be called immediately after
-- 'reportCursorPosition' (or related functions) have caused characters to be
-- emitted into the stream.
--
-- For example, on a Unix-like operating system:
--
-- > -- set no buffering (if 'no buffering' is not already set, the contents of
-- > -- the buffer will be discarded, so this needs to be done before the cursor
-- > -- positon is emitted)
-- > hSetBuffering stdin NoBuffering
-- > -- ensure that echoing is off
-- > input <- bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
-- >   hSetEcho stdin False
-- >   reportCursorPosition
-- >   hFlush stdout -- ensure the report cursor position code is sent to the
-- >                 -- operating system
-- >   getReportedCursorPosition
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Win32 console of the Windows API.
-- (Command Prompt and PowerShell are based on the Win32 console.)
--
-- @since 0.7.1
getReportedCursorPosition :: IO String

-- | Attempts to get the reported cursor position, combining the functions
-- 'reportCursorPosition', 'getReportedCursorPosition' and 'cursorPosition'. Any
-- position @(row, column)@ is translated to be 0-based (that is, the top-left
-- corner is at @(0, 0)@), consistent with `setCursorColumn` and
-- `setCursorPosition`. (Note that the information emitted into the console
-- input stream by 'reportCursorPosition' is 1-based.) Returns 'Nothing' if any
-- data emitted by 'reportCursorPosition', obtained by
-- 'getReportedCursorPosition', cannot be parsed by 'cursorPosition'. Uses
-- 'stdout'. If 'stdout' will be redirected, see 'hGetCursorPosition' for a more
-- general function.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Win32 console of the Windows API.
-- (Command Prompt and PowerShell are based on the Win32 console.)
--
-- @since 0.10.3
getCursorPosition :: IO (Maybe (Int, Int))
getCursorPosition = hGetCursorPosition stdout

-- | Attempts to get the reported cursor position, combining the functions
-- 'hReportCursorPosition' (with the specified handle),
-- 'getReportedCursorPosition' and 'cursorPosition'. Any position
-- @(row, column)@ is translated to be 0-based (that is, the top-left corner is
-- at @(0, 0)@), consistent with 'hSetCursorColumn' and 'hSetCursorPosition'.
-- (Note that the information emitted into the console input stream by
-- 'hReportCursorPosition' is 1-based.) Returns 'Nothing' if any data emitted by
-- 'hReportCursorPosition', obtained by 'getReportedCursorPosition', cannot be
-- parsed by 'cursorPosition'.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Win32 console of the Windows API.
-- (Command Prompt and PowerShell are based on the Win32 console.)
--
-- @since 0.10.1
hGetCursorPosition :: Handle -> IO (Maybe (Int, Int))

-- | Attempts to get the current terminal size (height in rows, width in
-- columns), by using 'getCursorPosition' to query the console input stream
-- after attempting to set the cursor position beyond the bottom right corner of
-- the terminal. Uses 'stdout'. If 'stdout' will be redirected, see
-- 'hGetTerminalSize' for a more general function.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Win32 console of the Windows API.
-- (Command Prompt and PowerShell are based on the Win32 console.)
--
-- @since 0.9
getTerminalSize :: IO (Maybe (Int, Int))
getTerminalSize = hGetTerminalSize stdout

-- | Attempts to get the current terminal size (height in rows, width in
-- columns), by writing control character sequences to the specified handle
-- (which will typically be 'stdout' or 'stderr') and using 'hGetCursorPosition'
-- to query the console input stream after attempting to set the cursor position
-- beyond the bottom right corner of the terminal.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Win32 console of the Windows API.
-- (Command Prompt and PowerShell are based on the Win32 console.)
--
-- @since 0.10.1
hGetTerminalSize :: Handle -> IO (Maybe (Int, Int))
hGetTerminalSize h = do
  hSaveCursor h
  hSetCursorPosition h 9999 9999  -- Attempt to set the cursor position beyond
                                  -- the bottom right corner of the terminal.
  mPos <- hGetCursorPosition h
  hRestoreCursor h
  hFlush h -- ensure the restore cursor position code is sent to the
           -- operating system
  return $ fmap (\(r, c) -> (r + 1, c + 1)) mPos

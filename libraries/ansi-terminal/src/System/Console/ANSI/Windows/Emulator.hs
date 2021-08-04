{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Windows.Emulator
  (
#include "Exports-Include.hs"
  ) where

import Control.Exception (catchJust, IOException)
import qualified Control.Exception as CE (catch)
import Control.Monad (unless)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl', minimumBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map (Map, empty, insert, lookup)
import System.IO (Handle, hIsTerminalDevice, stdin)
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadP (readP_to_S)

import Data.Colour (Colour)
import Data.Colour.Names (black, blue, cyan, green, grey, lime, magenta, maroon,
  navy, olive, purple, red, silver, teal, white, yellow)
import Data.Colour.SRGB (RGB (..), toSRGB)
import System.Console.MinTTY (isMinTTYHandle)

import System.Console.ANSI.Types
import qualified System.Console.ANSI.Unix as Unix
import System.Console.ANSI.Windows.Detect
import System.Console.ANSI.Windows.Emulator.Codes
import System.Console.ANSI.Windows.Foreign

-- This file contains code that is common to modules System.Console.ANSI.Unix,
-- System.Console.ANSI.Windows and System.Console.ANSI.Windows.Emulator, such as
-- type signatures and the definition of functions specific to stdout in terms
-- of the corresponding more general functions, inclduding the related Haddock
-- documentation.
#include "Common-Include.hs"
-- This file contains code that is required in the case of the module
-- System.Console.ANSI.Windows.Emulator and differs from the common code in
-- file Common-Include-Enabled.hs.
#include "Common-Include-Emulator.hs"

withHandle :: Handle -> (HANDLE -> IO a) -> IO a
withHandle handle action = do
  -- It's VERY IMPORTANT that we flush before issuing any sort of Windows API
  -- call to change the console because on Windows the arrival of
  -- API-initiated state changes is not necessarily synchronised with that of
  -- the text they are attempting to modify.
  hFlush handle
  withHandleToHANDLE handle action

-- Unfortunately, the emulator is not perfect. In particular, it has a tendency
-- to die with exceptions about invalid handles when it is used with certain
-- Windows consoles (e.g. mintty, terminator, or cygwin sshd).
--
-- This happens because in those environments the stdout family of handles are
-- not actually associated with a real console.
--
-- My observation is that every time I've seen this in practice, the handle we
-- have instead of the actual console handle is there so that the terminal
-- supports ANSI escape codes. So 99% of the time, the correct thing to do is
-- just to fall back on the Unix module to output the ANSI codes and hope for
-- the best.
emulatorFallback :: IO a -> IO a -> IO a
emulatorFallback fallback first_try
  = catchJust invalidHandle first_try (const fallback)
 where
  invalidHandle (ConsoleException 6) = Just ()  -- 6 is the Windows error code
                                                -- for invalid handles
  invalidHandle (_)                  = Nothing

adjustCursorPosition :: HANDLE
                     -> (SHORT -> SHORT -> SHORT)
                     -> (SHORT -> SHORT -> SHORT)
                     -> IO ()
adjustCursorPosition handle change_x change_y = do
  screen_buffer_info <- getConsoleScreenBufferInfo handle
  let window = csbi_window screen_buffer_info
      l = rect_left window
      t = rect_top window
      r = rect_right window
      b = rect_bottom window
      (COORD x y) = csbi_cursor_position screen_buffer_info
      clamp mn mx = max mn . min mx
      x' = clamp l r (change_x l x)
      y' = clamp t b (change_y t y)
      cursor_pos' = COORD x' y'
  setConsoleCursorPosition handle cursor_pos'

hCursorUp h n
  = emulatorFallback (Unix.hCursorUp h n) $ withHandle h $
      \handle -> adjustCursorPosition handle (\_ x -> x)
                   (\_ y -> y - fromIntegral n)
hCursorDown h n
  = emulatorFallback (Unix.hCursorDown h n) $ withHandle h $
      \handle -> adjustCursorPosition handle (\_ x -> x)
                   (\_ y -> y + fromIntegral n)
hCursorForward h n
  = emulatorFallback (Unix.hCursorForward h n) $ withHandle h $
      \handle -> adjustCursorPosition handle (\_ x -> x + fromIntegral n)
                   (\_ y -> y)
hCursorBackward h n
  = emulatorFallback (Unix.hCursorBackward h n) $ withHandle h $
      \handle -> adjustCursorPosition handle (\_ x -> x - fromIntegral n)
                   (\_ y -> y)

adjustLine :: HANDLE -> (SHORT -> SHORT -> SHORT) -> IO ()
adjustLine handle change_y
  = adjustCursorPosition handle (\window_left _ -> window_left) change_y

hCursorDownLine h n
  = emulatorFallback (Unix.hCursorDownLine h n) $ withHandle h $
      \handle -> adjustLine handle (\_ y -> y + fromIntegral n)

hCursorUpLine h n
  = emulatorFallback (Unix.hCursorUpLine h n) $ withHandle h $
      \handle -> adjustLine handle (\_ y -> y - fromIntegral n)

hSetCursorColumn h x
  = emulatorFallback (Unix.hSetCursorColumn h x) $ withHandle h $
      \handle -> adjustCursorPosition handle
                   (\window_left _ -> window_left + fromIntegral x) (\_ y -> y)

hSetCursorPosition h y x
  = emulatorFallback (Unix.hSetCursorPosition h y x) $ withHandle h $
      \handle -> adjustCursorPosition handle
                   (\window_left _ -> window_left + fromIntegral x)
                   (\window_top _ -> window_top + fromIntegral y)

clearChar :: WCHAR
clearChar = charToWCHAR ' '

-- | The 'clear' attribute is equated with the default background attributes.
clearAttribute :: ConsoleDefaultState -> WORD
clearAttribute = defaultBackgroundAttributes

hClearScreenFraction
  :: ConsoleDefaultState
  -> HANDLE
  -> (SMALL_RECT -> COORD -> (SHORT, SHORT, SHORT, SHORT, SHORT, SHORT))
  -> IO ()
hClearScreenFraction cds handle fraction_finder = do
  screen_buffer_info <- getConsoleScreenBufferInfo handle
  let window = csbi_window screen_buffer_info
      cursor_pos = csbi_cursor_position screen_buffer_info
      (left, top, right, bottom, start_x, end_x)
          = fraction_finder window cursor_pos
  mapM_ (fill_line left top right bottom start_x end_x) [top .. bottom]
 where
  fill_line left top right bottom start_x end_x y = do
    let left'  = if y == top    then start_x else left
        right' = if y == bottom then end_x   else right
        fill_cursor_pos = COORD left' y
        fill_length = fromIntegral $ right' - left' + 1
    _ <- fillConsoleOutputCharacter handle clearChar fill_length fill_cursor_pos
    fillConsoleOutputAttribute handle (clearAttribute cds) fill_length
      fill_cursor_pos

hClearFromCursorToScreenEnd cds h
  = emulatorFallback (Unix.hClearFromCursorToScreenEnd h) $ withHandle h $
      \handle -> hClearScreenFraction cds handle go
 where
  go window cursor_pos = (left, top, right, bottom, start_x, right)
   where
    SMALL_RECT (COORD left _) (COORD right bottom) = window
    COORD start_x top = cursor_pos

hClearFromCursorToScreenBeginning cds h
  = emulatorFallback (Unix.hClearFromCursorToScreenBeginning h) $ withHandle h $
      \handle -> hClearScreenFraction cds handle go
 where
  go window cursor_pos = (left, top, right, bottom, left, end_x)
   where
    SMALL_RECT (COORD left top) (COORD right _) = window
    COORD end_x bottom = cursor_pos

hClearScreen cds h
  = emulatorFallback (Unix.hClearScreen h) $ withHandle h $
      \handle -> hClearScreenFraction cds handle go
 where
  go window _ = (left, top, right, bottom, left, right)
   where
    SMALL_RECT (COORD left top) (COORD right bottom) = window

hClearFromCursorToLineEnd cds h
  = emulatorFallback (Unix.hClearFromCursorToLineEnd h) $ withHandle h $
      \handle -> hClearScreenFraction cds handle go
 where
  go window cursor_pos = (left, y, right, y, start_x, right)
   where
    SMALL_RECT (COORD left _) (COORD right _) = window
    COORD start_x y = cursor_pos

hClearFromCursorToLineBeginning cds h
  = emulatorFallback (Unix.hClearFromCursorToLineBeginning h) $ withHandle h $
        \handle -> hClearScreenFraction cds handle go
 where
  go window cursor_pos = (left, y, right, y, left, end_x)
   where
    SMALL_RECT (COORD left _) (COORD right _) = window
    COORD end_x y = cursor_pos

hClearLine cds h
  = emulatorFallback (Unix.hClearLine h) $ withHandle h $
      \handle -> hClearScreenFraction cds handle go
 where
  go window cursor_pos = (left, y, right, y, left, right)
   where
    SMALL_RECT (COORD left _) (COORD right _) = window
    COORD _ y = cursor_pos

hScrollPage
  :: ConsoleDefaultState -- ^ The default console state
  -> HANDLE
  -> Int
  -> IO ()
hScrollPage cds handle new_origin_y = do
  screen_buffer_info <- getConsoleScreenBufferInfo handle
  let fill = CHAR_INFO clearChar (clearAttribute cds)
      window = csbi_window screen_buffer_info
      origin = COORD (rect_left window)
                     (rect_top window + fromIntegral new_origin_y)
  scrollConsoleScreenBuffer handle window Nothing origin fill

hScrollPageUp cds h n
  = emulatorFallback (Unix.hScrollPageUp h n) $ withHandle h $
      \handle -> hScrollPage cds handle (negate n)

hScrollPageDown cds h n
  = emulatorFallback (Unix.hScrollPageDown h n) $ withHandle h $
      \handle -> hScrollPage cds handle n

{-# INLINE applyANSIColorToAttribute #-}
applyANSIColorToAttribute :: WORD -> WORD -> WORD -> Color -> WORD -> WORD
applyANSIColorToAttribute rED gREEN bLUE color attribute = case color of
  Black   -> attribute'
  Red     -> attribute' .|. rED
  Green   -> attribute' .|. gREEN
  Yellow  -> attribute' .|. rED .|. gREEN
  Blue    -> attribute' .|. bLUE
  Magenta -> attribute' .|. rED .|. bLUE
  Cyan    -> attribute' .|. gREEN .|. bLUE
  White   -> attribute' .|. wHITE
 where
  wHITE = rED .|. gREEN .|. bLUE
  attribute' = attribute .&. (complement wHITE)

applyForegroundANSIColorToAttribute, applyBackgroundANSIColorToAttribute
  :: Color
  -> WORD
  -> WORD
applyForegroundANSIColorToAttribute
  = applyANSIColorToAttribute fOREGROUND_RED fOREGROUND_GREEN fOREGROUND_BLUE
applyBackgroundANSIColorToAttribute
  = applyANSIColorToAttribute bACKGROUND_RED bACKGROUND_GREEN bACKGROUND_BLUE

swapForegroundBackgroundColors :: WORD -> WORD
swapForegroundBackgroundColors attribute
  = clean_attribute .|. foreground_attribute' .|. background_attribute'
 where
  foreground_attribute = attribute .&. fOREGROUND_INTENSE_WHITE
  background_attribute = attribute .&. bACKGROUND_INTENSE_WHITE
  clean_attribute = attribute .&. complement
    (fOREGROUND_INTENSE_WHITE .|. bACKGROUND_INTENSE_WHITE)
  foreground_attribute' = background_attribute `shiftR` 4
  background_attribute' = foreground_attribute `shiftL` 4

applyANSISGRToAttribute :: WORD -> SGR -> WORD -> WORD
applyANSISGRToAttribute def sgr attribute = case sgr of
  Reset -> def
  SetDefaultColor Foreground ->
    (attribute .&. complement fOREGROUND_INTENSE_WHITE) .|.
    (def .&. fOREGROUND_INTENSE_WHITE)
  SetDefaultColor Background ->
    (attribute .&. complement bACKGROUND_INTENSE_WHITE) .|.
    (def .&. bACKGROUND_INTENSE_WHITE)
  SetConsoleIntensity intensity -> case intensity of
    BoldIntensity   -> attribute .|. iNTENSITY
    FaintIntensity  -> attribute .&. (complement iNTENSITY) -- Not supported
    NormalIntensity -> attribute .&. (complement iNTENSITY)
  SetItalicized _ -> attribute -- Not supported
  SetUnderlining underlining -> case underlining of
    NoUnderline -> attribute .&. (complement cOMMON_LVB_UNDERSCORE)
    _           -> attribute .|. cOMMON_LVB_UNDERSCORE -- Not supported, since
    -- cOMMON_LVB_UNDERSCORE seems to have no effect
  SetBlinkSpeed _ -> attribute -- Not supported
  SetVisible _    -> attribute -- Not supported
  -- The cOMMON_LVB_REVERSE_VIDEO doesn't actually appear to have any affect
  -- on the colors being displayed, so the emulator just uses it to carry
  -- information and implements the color-swapping behaviour itself. Bit of a
  -- hack, I guess :-)
  SetSwapForegroundBackground True ->
    -- Check if the color-swapping flag is already set
    if attribute .&. cOMMON_LVB_REVERSE_VIDEO /= 0
      then attribute
      else swapForegroundBackgroundColors attribute .|. cOMMON_LVB_REVERSE_VIDEO
  SetSwapForegroundBackground False ->
    -- Check if the color-swapping flag is already not set
    if attribute .&. cOMMON_LVB_REVERSE_VIDEO == 0
      then attribute
      else swapForegroundBackgroundColors attribute .&.
          (complement cOMMON_LVB_REVERSE_VIDEO)
  SetColor Foreground Dull color  -> applyForegroundANSIColorToAttribute color
    (attribute .&. (complement fOREGROUND_INTENSITY))
  SetColor Foreground Vivid color -> applyForegroundANSIColorToAttribute color
    (attribute .|. fOREGROUND_INTENSITY)
  SetColor Background Dull color  -> applyBackgroundANSIColorToAttribute color
    (attribute .&. (complement bACKGROUND_INTENSITY))
  SetColor Background Vivid color -> applyBackgroundANSIColorToAttribute color
    (attribute .|. bACKGROUND_INTENSITY)
  SetRGBColor Foreground color ->
    let (colorIntensity, aNSIColor) = toANSIColor color
        attribute' = case colorIntensity of
          Dull  -> attribute .&. complement fOREGROUND_INTENSITY
          Vivid -> attribute .|. fOREGROUND_INTENSITY
    in applyForegroundANSIColorToAttribute aNSIColor attribute'
  SetRGBColor Background color ->
    let (colorIntensity, aNSIColor) = toANSIColor color
        attribute' = case colorIntensity of
          Dull  -> attribute .&. complement bACKGROUND_INTENSITY
          Vivid -> attribute .|. bACKGROUND_INTENSITY
    in applyBackgroundANSIColorToAttribute aNSIColor attribute'
  SetPaletteColor _ _ -> attribute  -- Not supported
 where
  iNTENSITY = fOREGROUND_INTENSITY

hSetSGR cds h sgr
  = emulatorFallback (Unix.hSetSGR h sgr) $ withHandle h $ \handle -> do
      screen_buffer_info <- getConsoleScreenBufferInfo handle
      let attribute  = csbi_attributes screen_buffer_info
          def        =     defaultForegroundAttributes cds
                       .|. defaultBackgroundAttributes cds
          attribute' = foldl' (flip $ applyANSISGRToAttribute def) attribute
            -- make [] equivalent to [Reset], as documented
            (if null sgr then [Reset] else sgr)
      setConsoleTextAttribute handle attribute'

hChangeCursorVisibility :: HANDLE -> Bool -> IO ()
hChangeCursorVisibility handle cursor_visible = do
  cursor_info <- getConsoleCursorInfo handle
  setConsoleCursorInfo handle
    (cursor_info { cci_cursor_visible = cursor_visible })

hHideCursor h
  = emulatorFallback (Unix.hHideCursor h) $ withHandle h $
      \handle -> hChangeCursorVisibility handle False
hShowCursor h
  = emulatorFallback (Unix.hShowCursor h) $ withHandle h $
      \handle -> hChangeCursorVisibility handle True

-- Windows only supports setting the terminal title on a process-wide basis, so
-- for now we will assume that that is what the user intended. This will fail if
-- they are sending the command over e.g. a network link... but that's not
-- really what I'm designing for.
hSetTitle h title
  = emulatorFallback (Unix.hSetTitle h title) $
      withTString title $ setConsoleTitle

cursorPositionRef :: IORef (Map.Map HANDLE COORD)
{-# NOINLINE cursorPositionRef #-}
cursorPositionRef = unsafePerformIO $ newIORef Map.empty

hSaveCursor h
  = emulatorFallback (Unix.hSaveCursor h) $ withHandle h $ \handle -> do
      screen_buffer_info <- getConsoleScreenBufferInfo handle
      m <- readIORef cursorPositionRef
      writeIORef cursorPositionRef
        (Map.insert handle (csbi_cursor_position screen_buffer_info) m)

hRestoreCursor h
  = emulatorFallback (Unix.hRestoreCursor h) $ withHandle h $ \handle -> do
      m <- readIORef cursorPositionRef
      let result = Map.lookup handle m
      maybe (return ()) (setConsoleCursorPosition handle) result

hReportCursorPosition h
  = emulatorFallback (Unix.hReportCursorPosition h) $ withHandle h $
      \handle -> do
        result <- getConsoleScreenBufferInfo handle
        let (COORD cx cy) = csbi_cursor_position result
            window = csbi_window result
            x = cx - rect_left window + 1
            y = cy - rect_top window + 1
        hIn <- getStdHandle sTD_INPUT_HANDLE
        _ <- writeConsoleInput hIn $ keyPresses $
            "\ESC[" ++ show y ++ ";" ++ show x ++ "R"
        return ()

keyPress :: Char -> [INPUT_RECORD]
keyPress c = [keyDown, keyUp]
 where
  keyDown = key True
  keyUp   = key False
  c' = UnicodeAsciiChar $ charToWCHAR c
  key isDown = INPUT_RECORD kEY_EVENT $
    InputKeyEvent (KEY_EVENT_RECORD isDown 1 0 0 c' 0)

keyPresses :: String -> [INPUT_RECORD]
keyPresses = concatMap keyPress

aNSIColors :: [((ColorIntensity, Color), Colour Float)]
aNSIColors = [ ((Dull,  Black),   black)
             , ((Dull,  Blue),    navy)
             , ((Dull,  Green),   green)
             , ((Dull,  Cyan),    teal)
             , ((Dull,  Red),     maroon)
             , ((Dull,  Magenta), purple)
             , ((Dull,  Yellow),  olive)
             , ((Dull,  White),   silver)
             , ((Vivid, Black),   grey)
             , ((Vivid, Blue),    blue)
             , ((Vivid, Green),   lime)
             , ((Vivid, Cyan),    cyan)
             , ((Vivid, Red),     red)
             , ((Vivid, Magenta), magenta)
             , ((Vivid, Yellow),  yellow)
             , ((Vivid, White),   white) ]

toANSIColor :: Colour Float -> (ColorIntensity, Color)
toANSIColor color = fst $ minimumBy order aNSIColors
 where
  RGB r g b = toSRGB color
  order (_, c1) (_, c2) = compare (dist c1) (dist c2)
  dist c = let RGB r' g' b' = toSRGB c
               dr = r' - r
               dg = g' - g
               db = b' - b
           in  dr * dr + dg * dg + db * db

-- hSupportsANSI :: Handle -> IO Bool
-- (See Common-Include.hs for Haddock documentation)
hSupportsANSI h = (||) <$> isTDNotDumb h <*> isMinTTY
 where
  isMinTTY = withHandleToHANDLE h isMinTTYHandle

-- hSupportsANSIWithoutEmulation :: Handle -> IO (Maybe Bool)
-- (See Common-Include.hs for Haddock documentation)
hSupportsANSIWithoutEmulation handle = do
  supportsANSI <- detectHandleSupportsANSI handle  -- Without reference to the
                                                   -- environment
  case supportsANSI of
    Just isSupported -> return (Just isSupported)
    Nothing -> do  -- Not sure, based on the handle alone
      notDumb <- isNotDumb  -- Test the environment
      if notDumb
        then return Nothing  -- Still not sure!
        else return (Just False) -- A dumb terminal

-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
isTDNotDumb :: Handle -> IO Bool
isTDNotDumb h = (&&) <$> hIsTerminalDevice h <*> isNotDumb

-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
isNotDumb :: IO Bool
-- cannot use lookupEnv since it only appeared in GHC 7.6
isNotDumb = (/= Just "dumb") . lookup "TERM" <$> getEnvironment

-- getReportedCursorPosition :: IO String
-- (See Common-Include.hs for Haddock documentation)
getReportedCursorPosition
  = CE.catch getReportedCursorPosition' getCPExceptionHandler
 where
  getReportedCursorPosition' = withHandleToHANDLE stdin action
   where
    action hdl = do
      n <- getNumberOfConsoleInputEvents hdl
      if n == 0
        then return ""
        else do
          es <- readConsoleInput hdl n
          return $ stringFromInputEvents es
    stringFromInputEvents = cWcharsToChars . wCharsFromInputEvents
    wCharsFromInputEvents = mapMaybe wCharFromInputEvent
    wCharFromInputEvent e = if isKeyDownEvent
      then Just (unicodeAsciiChar $ keyEventChar keyEventRecord)
      else Nothing
     where
      eventType = inputEventType e
      InputKeyEvent keyEventRecord = inputEvent e
      isKeyDown = keyEventKeyDown keyEventRecord
      isKeyDownEvent = eventType == 1 && isKeyDown

-- hGetCursorPosition :: Handle -> IO (Maybe (Int, Int))
-- (See Common-Include.hs for Haddock documentation)
hGetCursorPosition h = fmap to0base <$> getCursorPosition'
 where
  to0base (row, col) = (row - 1, col - 1)
  getCursorPosition' = CE.catch getCursorPosition'' getCPExceptionHandler
   where
    getCursorPosition'' = do
      withHandleToHANDLE stdin flush -- Flush the console input buffer
      hReportCursorPosition h
      hFlush h -- ensure the report cursor position code is sent to the
               -- operating system
      input <- getReportedCursorPosition
      case readP_to_S cursorPosition input of
        [] -> return Nothing
        [((row, col),_)] -> return $ Just (row, col)
        (_:_) -> return Nothing
     where
      flush hdl = do
        n <- getNumberOfConsoleInputEvents hdl
        unless (n == 0) (void $ readConsoleInput hdl n)

getCPExceptionHandler :: IOException -> IO a
getCPExceptionHandler e = error msg
 where
  msg = "Error: " ++ show e ++ "\nThis error may be avoided by using a " ++
        "console based on the Win32 console of the Windows API, such as " ++
        "Command Prompt or PowerShell."

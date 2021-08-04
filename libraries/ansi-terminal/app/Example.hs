module Main
  (
    main
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, replicateM_)
import System.IO (hFlush, stdout)
import Text.Printf(printf)

import Data.Colour.SRGB (sRGB24)

import System.Console.ANSI

examples :: [IO ()]
examples = [ cursorMovementExample
           , lineChangeExample
           , setCursorPositionExample
           , saveRestoreCursorExample
           , clearExample
           , scrollExample
           , sgrColorExample
           , sgrOtherExample
           , cursorVisibilityExample
           , titleExample
           , getCursorPositionExample
           , getTerminalSizeExample
           ]

main :: IO ()
main = mapM_ (\example -> resetScreen >> example) examples

-- Annex D to Standard ECMA-48 (5th Ed, 1991) identifies that the representation
-- of an erased state is implementation-dependent. There may or may not be a
-- distinction between a character position in the erased state and one imaging
-- SPACE. Consequently, to reset the screen, the default graphic rendition must
-- be selected (setSGR [Reset]) before all character positions are put into the
-- erased state (clearScreen).
resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

pause :: IO ()
pause = do
  hFlush stdout
  -- 1 second pause
  threadDelay 1000000

cursorMovementExample :: IO ()
cursorMovementExample = do
  putStrLn "Line One"
  putStr "Line Two"
  pause
  -- Line One
  -- Line Two

  cursorUp 1
  putStr " - Extras"
  pause
  -- Line One - Extras
  -- Line Two

  cursorBackward 2
  putStr "zz"
  pause
  -- Line One - Extrzz
  -- Line Two

  cursorForward 2
  putStr "- And More"
  pause
  -- Line One - Extrzz  - And More
  -- Line Two

  cursorDown 1
  putStr "Disconnected"
  pause
  -- Line One - Extrzz  - And More
  -- Line Two                     Disconnected

lineChangeExample :: IO ()
lineChangeExample = do
  putStrLn "Line One"
  putStr "Line Two"
  pause
  -- Line One
  -- Line Two

  cursorUpLine 1
  putStr "New Line One"
  pause
  -- New Line One
  -- Line Two

  cursorDownLine 1
  putStr "New Line Two"
  pause
  -- New Line One
  -- New Line Two

setCursorPositionExample :: IO ()
setCursorPositionExample = do
  putStrLn "Line One"
  putStrLn "Line Two"
  pause
  -- Line One
  -- Line Two

  setCursorPosition 0 5
  putStr "Foo"
  pause
  -- Line Foo
  -- Line Two

  setCursorPosition 1 5
  putStr "Bar"
  pause
  -- Line Foo
  -- Line Bar

  setCursorColumn 1
  putStr "oaf"
  pause
  -- Line Foo
  -- Loaf Bar

saveRestoreCursorExample :: IO ()
saveRestoreCursorExample = do
  putStr "Start sentence ..."
  pause
  -- Start sentence ...

  saveCursor
  setCursorPosition 2 3
  putStr "SPLASH!"
  pause
  -- Start sentence ...
  --
  --    SPLASH!

  restoreCursor
  putStr " end sentence, uninterrupted."
  pause
  -- Start sentence ... end sentence, uninterrupted
  --
  --    SPLASH!

clearExample :: IO ()
clearExample = do
  putStrLn "Line One"
  putStrLn "Line Two"
  pause
  -- Line One
  -- Line Two

  setCursorPosition 0 4
  clearFromCursorToScreenEnd
  pause
  -- Line

  resetScreen
  putStrLn "Line One"
  putStrLn "Line Two"
  pause
  -- Line One
  -- Line Two

  setCursorPosition 1 4
  clearFromCursorToScreenBeginning
  pause
  --
  --     Two

  resetScreen
  putStrLn "Line One"
  putStrLn "Line Two"
  pause
  -- Line One
  -- Line Two

  setCursorPosition 0 4
  clearFromCursorToLineEnd
  pause
  -- Line
  -- Line Two

  setCursorPosition 1 4
  clearFromCursorToLineBeginning
  pause
  -- Line
  --      Two

  clearLine
  pause
  -- Line

  clearScreen
  pause
  --

scrollExample :: IO ()
scrollExample = do
  putStrLn "Line One"
  putStrLn "Line Two"
  putStrLn "Line Three"
  pause
  -- Line One
  -- Line Two
  -- Line Three

  scrollPageDown 2
  pause
  --
  --
  -- Line One
  -- Line Two
  -- Line Three

  scrollPageUp 3
  pause
  -- Line Two
  -- Line Three

sgrColorExample :: IO ()
sgrColorExample = do
  let colors = enumFromTo minBound maxBound :: [Color]
  forM_ [Foreground, Background] $ \layer ->  do
    forM_ [Dull, Vivid] $ \intensity -> do
      resetScreen
      forM_ colors $ \color -> do
        setSGR [Reset]
        setSGR [SetColor layer intensity color]
        putStrLn (show color)
      pause
  -- The ANSI eight standard colors, 4 times in sequence (two layers and two
  -- intensities)

  resetScreen
  putStrLn "True color (24 bit color depth)"
  putStrLn "-------------------------------"
  putStrLn ""
  setSGR [SetRGBColor Foreground $ sRGB24 0 0 0]
  forM_ [0 .. 23] $ \row -> do
    forM_ [0 .. 47] $ \col -> do
      let r = row * 11
          g = 255 - r
          b = col * 5
      setSGR [SetRGBColor Background $ sRGB24 r g b]
      putStr "-"
    putStrLn ""
  replicateM_ 5 pause
  -- True colors, a swatch of 24 rows and 48 columns

  resetScreen
  putStrLn "A 256-color palette"
  putStrLn "-------------------"
  putStrLn ""

  -- First 16 colors ('system' colors in xterm protocol), in a row
  --
  -- 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 A A B B C C D D E E F F
  forM_ [Dull .. Vivid] $ \intensity -> do
    forM_ [Black .. White] $ \color -> do
      let i = fromEnum intensity * 8 + fromEnum color
          eol = i == 15
      setSGR [SetPaletteColor Background $ xtermSystem intensity color]
      setSGR [SetPaletteColor Foreground $ xtermSystem Dull Black]
      printf "%X " i
      setSGR [SetPaletteColor Foreground $ xtermSystem Vivid White]
      printf "%X" i
      if eol
        then putStrLn ""
        else do
          setSGR [Reset]
          putStr " "
  putStrLn ""

  -- Next 216 colors (6 level RGB in xterm protocol), in 12 rows of 18
  --
  -- 000 001 002 003 004 005 010 011 012 013 014 015 020 021 022 023 024 025
  -- 030 031 032 033 034 035 040 041 042 043 044 045 050 051 052 053 054 055
  -- 100 101 102 103 104 105 110 111 112 113 114 115 120 121 122 123 124 125
  -- ... and so on ...
  forM_ [0 .. 5] $ \r -> do
    forM_ [0 .. 5] $ \g -> do
      forM_ [0 .. 5] $ \b -> do
        let i = 16 + b + g * 6 + r * 36
            eol = i `mod` 18 == 15
            r' = (r + 3) `mod` 6
            g' = (g + 3) `mod` 6
            b' = (b + 3) `mod` 6
        setSGR [SetPaletteColor Foreground $ xterm6LevelRGB r' g' b']
        setSGR [SetPaletteColor Background $ xterm6LevelRGB r g b]
        putStr $ show r ++ show g ++ show b
        if eol
          then putStrLn ""
          else do
            setSGR [Reset]
            putStr " "
  putStrLn ""

  -- Final 24 colors (24 levels of gray in xterm protocol), in two rows
  --
  --   0   1   2   3   4   5   6   7   8   9  10  11
  --  12  13  14  15  16  17  18  19  20  21  22  23
  forM_ [0 .. 23] $ \y -> do
    setSGR [SetPaletteColor Foreground $ xterm24LevelGray $ (y + 12) `mod` 24]
    setSGR [SetPaletteColor Background $ xterm24LevelGray y]
    printf "%3d" y
    if y == 11
      then putStrLn ""
      else do
        setSGR [Reset]
        putStr " "
  replicateM_ 5 pause

sgrOtherExample :: IO ()
sgrOtherExample = do
  let named_styles = [ (SetConsoleIntensity BoldIntensity, "Bold")
                     , (SetConsoleIntensity FaintIntensity, "Faint")
                     , (SetConsoleIntensity NormalIntensity, "Normal")
                     , (SetItalicized True, "Italic")
                     , (SetItalicized False, "No Italics")
                     , (SetUnderlining SingleUnderline, "Single Underline")
                     , (SetUnderlining DoubleUnderline, "Double Underline")
                     , (SetUnderlining NoUnderline, "No Underline")
                     , (SetBlinkSpeed SlowBlink, "Slow Blink")
                     , (SetBlinkSpeed RapidBlink, "Rapid Blink")
                     , (SetBlinkSpeed NoBlink, "No Blink")
                     , (SetVisible False, "Conceal")
                     , (SetVisible True, "Reveal")
                     ]
  forM_ named_styles $ \(style, name) -> do
    resetScreen
    setSGR [style]
    putStrLn name
    pause
  -- Text describing a style displayed in that style in sequence

  setSGR [SetColor Foreground Vivid Red]
  setSGR [SetColor Background Vivid Blue]

  clearScreen >> setCursorPosition 0 0
  setSGR [SetSwapForegroundBackground False]
  putStr "Red-On-Blue"
  pause
  -- Red-On-Blue

  clearScreen >> setCursorPosition 0 0
  setSGR [SetSwapForegroundBackground True]
  putStr "Blue-On-Red"
  pause
  -- Blue-On-Red

cursorVisibilityExample :: IO ()
cursorVisibilityExample = do
  putStr "Cursor Demo"
  pause
  -- Cursor Demo|

  hideCursor
  pause
  -- Cursor Demo

  showCursor
  pause
  -- Cursor Demo|

titleExample :: IO ()
titleExample = do
  putStr "Title Demo"
  pause
  -- ~/foo/ - ansi-terminal-ex - 83x70
  ------------------------------------
  -- Title Demo

  setTitle "Yup, I'm a new title!"
  pause
  -- Yup, I'm a new title! - ansi-terminal-ex - 83x70
  ---------------------------------------------------
  -- Title Demo

getCursorPositionExample :: IO ()
getCursorPositionExample = do
  putStrLn "         11111111112222222222"
  putStrLn "12345678901234567890123456789"
  putStr   "Report cursor position here:"
  pause
  --          11111111112222222222
  -- 12345678901234567890123456789
  -- Report cursor position here:|
  result <- getCursorPosition
  putStrLn " (3rd row, 29th column) to stdin, as CSI 3 ; 29 R.\n"
  case result of
    Just (row, col) -> putStrLn $ "The cursor was at row number " ++
      show (row + 1) ++ " and column number " ++ show (col + 1) ++ ".\n"
    Nothing -> putStrLn "Error: unable to get the cursor position\n"
  replicateM_ 3 pause
  --          11111111112222222222
  -- 12345678901234567890123456789
  -- Report cursor position here: (3rd row, 29th column) to stdin, as CSI 3 ; 29 R.
  --
  -- The cursor was at row number 3 and column number 29.

getTerminalSizeExample :: IO ()
getTerminalSizeExample = do
  result <- getTerminalSize
  case result of
    Just (h, w) -> putStrLn $ "The size of the terminal is " ++ show h ++
      " rows by " ++ show w ++ " columns.\n"
    Nothing -> putStrLn "Error: unable to get the terminal size\n"
  pause
  -- The size of the terminal is 25 rows by 80 columns.

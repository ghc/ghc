-- |
-- Module      : Basement.Terminal.ANSI
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
--
-- ANSI Terminal escape for cursor and attributes manipulations
--
-- On Unix system, it should be supported by most terminal emulators.
--
-- On Windows system, all escape sequences are empty for maximum
-- compatibility purpose, and easy implementation. newer version
-- of Windows 10 supports ANSI escape now, but we'll need
-- some kind of detection.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Basement.Terminal.ANSI
    (
    -- * Types
      Escape
    , Displacement
    , ColorComponent
    , GrayComponent
    , RGBComponent
    -- * Simple ANSI escape factory functions
    , cursorUp
    , cursorDown
    , cursorForward
    , cursorBack
    , cursorNextLine
    , cursorPrevLine
    , cursorHorizontalAbsolute
    , cursorPosition
    , eraseScreenFromCursor
    , eraseScreenToCursor
    , eraseScreenAll
    , eraseLineFromCursor
    , eraseLineToCursor
    , eraseLineAll
    , scrollUp
    , scrollDown
    , sgrReset
    , sgrForeground
    , sgrBackground
    , sgrForegroundGray24
    , sgrBackgroundGray24
    , sgrForegroundColor216
    , sgrBackgroundColor216
    ) where

import Basement.String
import Basement.Bounded
import Basement.Imports
import Basement.Numerical.Multiplicative
import Basement.Numerical.Additive

#ifndef mingw32_HOST_OS
#define SUPPORT_ANSI_ESCAPE
#endif

type Escape = String

type Displacement = Word64

-- | Simple color component on 8 color terminal (maximum compatibility)
type ColorComponent = Zn64 8

-- | Gray color compent on 256colors terminals
type GrayComponent = Zn64 24

-- | Color compent on 256colors terminals
type RGBComponent = Zn64 6

cursorUp, cursorDown, cursorForward, cursorBack
    , cursorNextLine, cursorPrevLine
    , cursorHorizontalAbsolute :: Displacement -> Escape
cursorUp n = csi1 n "A"
cursorDown n = csi1 n "B"
cursorForward n = csi1 n "C"
cursorBack n = csi1 n "D"
cursorNextLine n = csi1 n "E"
cursorPrevLine n = csi1 n "F"
cursorHorizontalAbsolute n = csi1 n "G"

cursorPosition :: Displacement -> Displacement -> Escape
cursorPosition row col = csi2 row col "H"

eraseScreenFromCursor
    , eraseScreenToCursor
    , eraseScreenAll
    , eraseLineFromCursor
    , eraseLineToCursor
    , eraseLineAll :: Escape
eraseScreenFromCursor = csi1 0 "J"
eraseScreenToCursor = csi1 1 "J"
eraseScreenAll = csi1 2 "J"
eraseLineFromCursor = csi1 0 "K"
eraseLineToCursor = csi1 1 "K"
eraseLineAll = csi1 2 "K"

scrollUp, scrollDown :: Displacement -> Escape
scrollUp n = csi1 n "S"
scrollDown n = csi1 n "T"

-- | All attribute off
sgrReset :: Escape
sgrReset = csi1 0 "m"

-- | 8 Colors + Bold attribute for foreground
sgrForeground :: ColorComponent -> Bool -> Escape
sgrForeground n bold
    | bold      = csi2 (30+unZn64 n) 1 "m"
    | otherwise = csi1 (30+unZn64 n) "m"

-- | 8 Colors + Bold attribute for background
sgrBackground :: ColorComponent -> Bool -> Escape
sgrBackground n bold
    | bold      = csi2 (40+unZn64 n) 1 "m" 
    | otherwise = csi1 (40+unZn64 n) "m"

-- 256 colors mode

sgrForegroundGray24, sgrBackgroundGray24 :: GrayComponent -> Escape
sgrForegroundGray24 v = csi3 38 5 (0xE8 + unZn64 v) "m"
sgrBackgroundGray24 v = csi3 48 5 (0xE8 + unZn64 v) "m"

sgrForegroundColor216 :: RGBComponent -- ^ Red component
                      -> RGBComponent -- ^ Green component
                      -> RGBComponent -- ^ Blue component
                      -> Escape
sgrForegroundColor216 r g b = csi3 38 5 (0x10 + 36 * unZn64 r + 6 * unZn64 g + unZn64 b) "m"

sgrBackgroundColor216 :: RGBComponent -- ^ Red component
                      -> RGBComponent -- ^ Green component
                      -> RGBComponent -- ^ Blue component
                      -> Escape
sgrBackgroundColor216 r g b = csi3 48 5 (0x10 + 36 * unZn64 r + 6 * unZn64 g + unZn64 b) "m"

#ifdef SUPPORT_ANSI_ESCAPE

csi0 :: String -> String
csi0 suffix = mconcat ["\ESC[", suffix]

csi1 :: Displacement -> String -> String
csi1 p1 suffix = mconcat ["\ESC[", pshow p1, suffix]

csi2 :: Displacement -> Displacement -> String -> String
csi2 p1 p2 suffix = mconcat ["\ESC[", pshow p1, ";", pshow p2, suffix]

csi3 :: Displacement -> Displacement -> Displacement -> String -> String
csi3 p1 p2 p3 suffix = mconcat ["\ESC[", pshow p1, ";", pshow p2, ";", pshow p3, suffix]

pshow = show

#else

csi0 :: String -> String
csi0 _ = ""

csi1 :: Displacement -> String -> String
csi1 _ _ = ""

csi2 :: Displacement -> Displacement -> String -> String
csi2 _ _ _ = ""

csi3 :: Displacement -> Displacement -> Displacement -> String -> String
csi3 _ _ _ _ = ""

#endif

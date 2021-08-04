#include "Common-Safe-Haskell.hs"

{-| The \'ANSI\' standards refer to the visual style of displaying characters as
their \'graphic rendition\'. The style includes the color of a character or its
background, the intensity (bold, normal or faint) of a character, or whether the
character is italic or underlined (single or double), blinking (slowly or
rapidly) or visible or not. The \'ANSI\' codes to establish the graphic
rendition for subsequent text are referred to as SELECT GRAPHIC RENDITION (SGR).

This module exports types and functions used to represent SGR aspects. See also
'System.Console.ANSI.setSGR' and related functions.
-}
module System.Console.ANSI.Types
  (
  -- * Types used to represent SGR aspects
    SGR (..)
  , ConsoleLayer (..)
  , Color (..)
  , ColorIntensity (..)
  , ConsoleIntensity (..)
  , Underlining (..)
  , BlinkSpeed (..)
  -- * Constructors of xterm 256-color palette indices
  , xterm6LevelRGB
  , xterm24LevelGray
  , xtermSystem
  ) where

import Data.Ix (Ix)
import Data.Word (Word8)

import Data.Colour (Colour)

-- | ANSI's eight standard colors. They come in two intensities, which are
-- controlled by 'ColorIntensity'. Many terminals allow the colors of the
-- standard palette to be customised, so that, for example,
-- @setSGR [ SetColor Foreground Vivid Green ]@ may not result in bright green
-- characters.
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White
           deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI's standard colors come in two intensities
data ColorIntensity = Dull
                    | Vivid
                    deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI colors can be set on two different layers
data ConsoleLayer = Foreground
                  | Background
                  deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI blink speeds: values other than 'NoBlink' are not widely supported
data BlinkSpeed = SlowBlink -- ^ Less than 150 blinks per minute
                | RapidBlink -- ^ More than 150 blinks per minute
                | NoBlink
                deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI text underlining
data Underlining
  = SingleUnderline
  -- | Not widely supported. Not supported natively on Windows 10
  | DoubleUnderline
  | NoUnderline
  deriving (Eq, Ord, Bounded ,Enum, Show, Read, Ix)

-- | ANSI general console intensity: usually treated as setting the font style
-- (e.g. 'BoldIntensity' causes text to be bold)
data ConsoleIntensity
  = BoldIntensity
  -- | Not widely supported: sometimes treated as concealing text. Not supported
  -- natively on Windows 10
  | FaintIntensity
  | NormalIntensity
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI Select Graphic Rendition (SGR) command
--
-- In respect of colors, there are three alternative commands:
--
-- (1) the \'ANSI\' standards allow for eight standard colors (with two
-- intensities). Windows and many other terminals (including xterm) allow the
-- user to redefine the standard colors (so, for example 'Vivid' 'Green' may not
-- correspond to bright green;
--
-- (2) an extension of the standard that allows true colors (24 bit color depth)
-- in RGB space. This is usually the best alternative for more colors; and
--
-- (3) another extension that allows a palette of 256 colors, each color
-- specified by an index. Xterm provides a protocol for a palette of 256 colors
-- that many other terminals, including Windows 10, follow. Some terminals
-- (including xterm) allow the user to redefine some or all of the palette
-- colors.
data SGR
  -- | Default rendition, cancels the effect of any preceding occurrence of SGR
  -- (implementation-defined)
  = Reset
  -- | Set the character intensity. Partially supported natively on Windows 10
  | SetConsoleIntensity !ConsoleIntensity
  -- | Set italicized. Not widely supported: sometimes treated as swapping
  -- foreground and background. Not supported natively on Windows 10
  | SetItalicized !Bool
  -- | Set or clear underlining. Partially supported natively on Windows 10
  | SetUnderlining !Underlining
  -- | Set or clear character blinking. Not supported natively on Windows 10
  | SetBlinkSpeed !BlinkSpeed
  -- | Set revealed or concealed. Not widely supported. Not supported natively
  -- on Windows 10
  | SetVisible !Bool
  -- | Set negative or positive image. Supported natively on Windows 10
  | SetSwapForegroundBackground !Bool
  -- | Set a color from the standard palette of 16 colors (8 colors by 2
  -- color intensities). Many terminals allow the palette colors to be
  -- customised
  | SetColor !ConsoleLayer !ColorIntensity !Color
  -- | Set a true color (24 bit color depth). Supported natively on Windows 10
  -- from the Creators Update (April 2017)
  --
  -- @since 0.7
  | SetRGBColor !ConsoleLayer !(Colour Float)
  -- | Set a color from a palette of 256 colors using a numerical index
  -- (0-based). Supported natively on Windows 10 from the Creators Update (April
  -- 2017) but not on legacy Windows native terminals. See 'xtermSystem',
  -- 'xterm6LevelRGB' and 'xterm24LevelGray' to construct indices based on
  -- xterm's standard protocol for a 256-color palette.
  --
  -- @since 0.9
  | SetPaletteColor !ConsoleLayer !Word8
  -- | Set a color to the default (implementation-defined)
  --
  -- @since 0.10
  | SetDefaultColor !ConsoleLayer
  deriving (Eq, Show, Read)

-- | Given xterm's standard protocol for a 256-color palette, returns the index
-- to that part of the palette which is a 6 level (6x6x6) color cube of 216 RGB
-- colors. Throws an error if any of the red, green or blue channels is outside
-- the range 0 to 5. An example of use is:
--
-- >>> setSGR [ SetPaletteColor $ xterm6LevelRGB 5 2 0 ] -- Dark Orange
--
-- @since 0.9
xterm6LevelRGB :: Int -> Int -> Int -> Word8
xterm6LevelRGB r g b
  -- RGB colors are represented by index:
  -- 16 + 36 × r + 6 × g + b (0 ≤ r, g, b ≤ 5)
  | r >= 0 && r < 6 && g >= 0 && g < 6 && b >= 0 && b < 6
  =  fromIntegral $ 16 + 36 * r + 6 * g + b
  | otherwise
  = error $ show r ++ " " ++ show g ++ " " ++ show b ++ " (r g b) is " ++
            "outside of a 6 level (6x6x6) color cube."

-- | Given xterm's standard protocol for a 256-color palette, returns the index
-- to that part of the palette which is a spectrum of 24 grays, from dark
-- gray (0) to near white (23) (black and white are themselves excluded). Throws
-- an error if the gray is outside of the range 0 to 23. An example of use is:
--
-- >>> setSGR [ SetPaletteColor $ xterm24LevelGray 12 ] -- Gray50
--
-- @since 0.9
xterm24LevelGray :: Int -> Word8
xterm24LevelGray y
  -- Grayscale colors are represented by index:
  -- 232 + g (0 ≤ g ≤ 23)
  | y >= 0 && y < 24 = fromIntegral $ 232 + y
  | otherwise
  = error $ show y ++ " (gray) is outside of the range 0 to 23."

-- | Given xterm's standard protocol for a 256-color palette, returns the index
-- to that part of the palette which corresponds to the \'ANSI\' standards' 16
-- standard, or \'system\', colors (eight colors in two intensities). An example
-- of use is:
--
-- >>> setSGR [ SetPaletteColor $ xtermSystem Vivid Green ]
--
-- @since 0.9
xtermSystem :: ColorIntensity -> Color -> Word8
xtermSystem intensity color
  | intensity == Dull  = index
  | otherwise          = index + 8
 where
  index = fromIntegral $ fromEnum color

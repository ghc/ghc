#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.Font
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module Graphics.Win32.GDI.Font
{-
        ( CharSet
        , PitchAndFamily
        , OutPrecision
        , ClipPrecision
        , FontQuality
        , FontWeight

        , createFont, deleteFont

        , StockFont, getStockFont
        , oEM_FIXED_FONT, aNSI_FIXED_FONT, aNSI_VAR_FONT, sYSTEM_FONT
        , dEVICE_DEFAULT_FONT, sYSTEM_FIXED_FONT
        ) where
-}
        where

import System.Win32.Types
import Graphics.Win32.GDI.Types

import Foreign

##include "windows_cconv.h"
#include <windows.h>

----------------------------------------------------------------
-- Types
----------------------------------------------------------------

type CharSet        = UINT
type PitchAndFamily = UINT
type OutPrecision   = UINT
type ClipPrecision  = UINT
type FontQuality    = UINT
type FontWeight     = Word32
type FaceName       = String

-- A FaceName is a string no more that LF_FACESIZE in length
-- (including null terminator).
-- %const Int LF_FACESIZE         # == 32
-- %sentinel_array : FaceName : CHAR : char : $0 = '\0' : ('\0' == $0) : LF_FACESIZE

----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

#{enum CharSet,
 , aNSI_CHARSET        = ANSI_CHARSET
 , dEFAULT_CHARSET     = DEFAULT_CHARSET
 , sYMBOL_CHARSET      = SYMBOL_CHARSET
 , sHIFTJIS_CHARSET    = SHIFTJIS_CHARSET
 , hANGEUL_CHARSET     = HANGEUL_CHARSET
 , cHINESEBIG5_CHARSET = CHINESEBIG5_CHARSET
 , oEM_CHARSET         = OEM_CHARSET
 }

#{enum PitchAndFamily,
 , dEFAULT_PITCH  = DEFAULT_PITCH
 , fIXED_PITCH    = FIXED_PITCH
 , vARIABLE_PITCH = VARIABLE_PITCH
 , fF_DONTCARE    = FF_DONTCARE
 , fF_ROMAN       = FF_ROMAN
 , fF_SWISS       = FF_SWISS
 , fF_MODERN      = FF_MODERN
 , fF_SCRIPT      = FF_SCRIPT
 , fF_DECORATIVE  = FF_DECORATIVE
 }

familyMask, pitchMask :: PitchAndFamily
familyMask = 0xF0
pitchMask  = 0x0F

#{enum OutPrecision,
 , oUT_DEFAULT_PRECIS   = OUT_DEFAULT_PRECIS
 , oUT_STRING_PRECIS    = OUT_STRING_PRECIS
 , oUT_CHARACTER_PRECIS = OUT_CHARACTER_PRECIS
 , oUT_STROKE_PRECIS    = OUT_STROKE_PRECIS
 , oUT_TT_PRECIS        = OUT_TT_PRECIS
 , oUT_DEVICE_PRECIS    = OUT_DEVICE_PRECIS
 , oUT_RASTER_PRECIS    = OUT_RASTER_PRECIS
 , oUT_TT_ONLY_PRECIS   = OUT_TT_ONLY_PRECIS
 }

#{enum ClipPrecision,
 , cLIP_DEFAULT_PRECIS   = CLIP_DEFAULT_PRECIS
 , cLIP_CHARACTER_PRECIS = CLIP_CHARACTER_PRECIS
 , cLIP_STROKE_PRECIS    = CLIP_STROKE_PRECIS
 , cLIP_MASK             = CLIP_MASK
 , cLIP_LH_ANGLES        = CLIP_LH_ANGLES
 , cLIP_TT_ALWAYS        = CLIP_TT_ALWAYS
 , cLIP_EMBEDDED         = CLIP_EMBEDDED
 }

#{enum FontQuality,
 , dEFAULT_QUALITY = DEFAULT_QUALITY
 , dRAFT_QUALITY   = DRAFT_QUALITY
 , pROOF_QUALITY   = PROOF_QUALITY
 }

#{enum FontWeight,
 , fW_DONTCARE   = FW_DONTCARE
 , fW_THIN       = FW_THIN
 , fW_EXTRALIGHT = FW_EXTRALIGHT
 , fW_LIGHT      = FW_LIGHT
 , fW_NORMAL     = FW_NORMAL
 , fW_MEDIUM     = FW_MEDIUM
 , fW_SEMIBOLD   = FW_SEMIBOLD
 , fW_BOLD       = FW_BOLD
 , fW_EXTRABOLD  = FW_EXTRABOLD
 , fW_HEAVY      = FW_HEAVY
 , fW_REGULAR    = FW_REGULAR
 , fW_ULTRALIGHT = FW_ULTRALIGHT
 , fW_DEMIBOLD   = FW_DEMIBOLD
 , fW_ULTRABOLD  = FW_ULTRABOLD
 , fW_BLACK      = FW_BLACK
 }

----------------------------------------------------------------
-- Functions
----------------------------------------------------------------

-- was: ErrorMsg("CreateFont","NullHandle")

createFont
     :: INT -> INT -> INT -> INT
     -> FontWeight -> Bool -> Bool -> Bool
     -> CharSet -> OutPrecision -> ClipPrecision
     -> FontQuality -> PitchAndFamily -> FaceName
     -> IO HFONT
createFont h w esc orient wt ital under strike cset out clip q pf face =
  withTString face $ \ c_face ->
  failIfNull "CreateFont" $
    c_CreateFont h w esc orient wt ital under strike cset out clip q pf c_face
foreign import WINDOWS_CCONV unsafe "windows.h CreateFontW"
  c_CreateFont
     :: INT -> INT -> INT -> INT
     -> FontWeight -> Bool -> Bool -> Bool
     -> CharSet -> OutPrecision -> ClipPrecision
     -> FontQuality -> PitchAndFamily -> LPCTSTR
     -> IO HFONT

-- test :: IO ()
-- test = do
--   f <- createFont_adr (100,100) 0 False False "Arial"
--   putStrLn "Created first font"
--   f <- createFont_adr (100,100) (-90) False False "Bogus"
--   putStrLn "Created second font"
--
-- createFont_adr (width, height) escapement bold italic family =
--  createFont height width
--                   (round (escapement * 1800/pi))
--                   0                     -- orientation
--                   weight
--                   italic False False    -- italic, underline, strikeout
--                   aNSI_CHARSET
--                   oUT_DEFAULT_PRECIS
--                   cLIP_DEFAULT_PRECIS
--                   dEFAULT_QUALITY
--                   dEFAULT_PITCH
--                   family
--  where
--   weight | bold      = fW_BOLD
--          | otherwise = fW_NORMAL


-- missing CreateFontIndirect from WinFonts.ss; GSL ???

foreign import WINDOWS_CCONV unsafe "windows.h DeleteObject"
  deleteFont :: HFONT -> IO ()

----------------------------------------------------------------

type StockFont      = WORD

#{enum StockFont,
 , oEM_FIXED_FONT      = OEM_FIXED_FONT
 , aNSI_FIXED_FONT     = ANSI_FIXED_FONT
 , aNSI_VAR_FONT       = ANSI_VAR_FONT
 , sYSTEM_FONT         = SYSTEM_FONT
 , dEVICE_DEFAULT_FONT = DEVICE_DEFAULT_FONT
 , sYSTEM_FIXED_FONT   = SYSTEM_FIXED_FONT
 }

foreign import WINDOWS_CCONV unsafe "windows.h GetStockObject"
  getStockFont :: StockFont -> IO HFONT

----------------------------------------------------------------
-- End
----------------------------------------------------------------

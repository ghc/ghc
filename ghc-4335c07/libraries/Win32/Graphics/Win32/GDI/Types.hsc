#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.Types
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

module Graphics.Win32.GDI.Types
{-  -- still incomplete
        ( POINT,        marshall_point, unmarshall_point
        , ListPOINT,    marshall_ListPOINT_
        , ListLenPOINT, marshall_ListLenPOINT_
        , RECT,         marshall_rect, unmarshall_rect
        , SIZE,         marshall_size, unmarshall_size
        , nullPtr
        , HBITMAP       , MbHBITMAP
        , HFONT         , MbHFONT
        , HCURSOR       , MbHCURSOR
        , HICON         , MbHICON
        , HRGN          , MbHRGN
        , PRGN
        , HPALETTE      , MbHPALETTE
        , HBRUSH        , MbHBRUSH
        , HPEN          , MbHPEN
        , HACCEL        --, MbHACCEL
        , HDC           , MbHDC
        , HDWP          , MbHDWP
        , HWND          , MbHWND
        , HMENU         , MbHMENU
        , PolyFillMode
        , ArcDirection
        , MbArcDirection
        , GraphicsMode
        , MbGraphicsMode
        , BackgroundMode
        , HatchStyle
        , StretchBltMode
        , COLORREF
        , TextAlignment
        , ClippingMode
        , RegionType
        , gDI_ERROR
        )
-}
        where

import System.Win32.Types

import Control.Monad( zipWithM_ )
import Foreign

#include <windows.h>

{-# CFILES cbits/HsGDI.c #-}

----------------------------------------------------------------
--
----------------------------------------------------------------

type POINT =
  ( LONG  -- x
  , LONG  -- y
  )

sizeofPOINT :: Int
sizeofPOINT = #{size POINT}

allocaPOINT :: (Ptr POINT -> IO a) -> IO a
allocaPOINT =
  allocaBytes sizeofPOINT

peekPOINT :: Ptr POINT -> IO POINT
peekPOINT p = do
  x <- #{peek POINT,x} p
  y <- #{peek POINT,y} p
  return (x,y)

pokePOINT :: Ptr POINT -> POINT -> IO ()
pokePOINT p (x,y) = do
  #{poke POINT,x} p x
  #{poke POINT,y} p y

withPOINT :: POINT -> (Ptr POINT -> IO a) -> IO a
withPOINT p f =
  allocaPOINT $ \ ptr -> do
  pokePOINT ptr p
  f ptr

type RECT =
  ( LONG  -- left
  , LONG  -- top
  , LONG  -- right
  , LONG  -- bottom
  )

allocaRECT :: (Ptr RECT -> IO a) -> IO a
allocaRECT =
  allocaBytes (#{size RECT})

peekRECT :: Ptr RECT -> IO RECT
peekRECT p = do
  left   <- #{peek RECT,left} p
  top    <- #{peek RECT,top} p
  right  <- #{peek RECT,right} p
  bottom <- #{peek RECT,bottom} p
  return (left, top, right, bottom)

pokeRECT :: Ptr RECT -> RECT -> IO ()
pokeRECT p (left, top, right, bottom) = do
  #{poke RECT,left}   p left
  #{poke RECT,top}    p top
  #{poke RECT,right}  p right
  #{poke RECT,bottom} p bottom

type SIZE =
  ( LONG  -- cx
  , LONG  -- cy
  )

allocaSIZE :: (Ptr SIZE -> IO a) -> IO a
allocaSIZE =
  allocaBytes (#{size SIZE})

peekSIZE :: Ptr SIZE -> IO SIZE
peekSIZE p = do
  cx <- #{peek SIZE,cx} p
  cy <- #{peek SIZE,cy} p
  return (cx,cy)

pokeSIZE :: Ptr SIZE -> SIZE -> IO ()
pokeSIZE p (cx,cy) = do
  #{poke SIZE,cx} p cx
  #{poke SIZE,cy} p cy

----------------------------------------------------------------

withPOINTArray :: [POINT] -> (Ptr POINT -> Int -> IO a) -> IO a
withPOINTArray xs f = allocaBytes (sizeofPOINT * len) $ \ ptr -> do
  pokePOINTArray ptr xs
  f ptr len
 where
  len = length xs

pokePOINTArray :: Ptr POINT -> [POINT] -> IO ()
pokePOINTArray ptr xs = zipWithM_ (setPOINT ptr) [0..] xs

setPOINT :: Ptr POINT -> Int -> POINT -> IO ()
setPOINT ptr off = pokePOINT (ptr `plusPtr` (off*sizeofPOINT))

type   LPRECT   = Ptr RECT
type MbLPRECT   = Maybe LPRECT

withRECT :: RECT -> (Ptr RECT -> IO a) -> IO a
withRECT r f =
  allocaRECT $ \ ptr -> do
  pokeRECT ptr r
  f ptr

getRECT :: LPRECT -> IO RECT
getRECT = peekRECT

----------------------------------------------------------------
-- (GDI related) Handles
----------------------------------------------------------------

type   HBITMAP    = HANDLE
type MbHBITMAP    = Maybe HBITMAP

type   HFONT      = HANDLE
type MbHFONT      = Maybe HFONT

type   HCURSOR    = HICON
type MbHCURSOR    = Maybe HCURSOR

type   HICON      = HANDLE
type MbHICON      = Maybe HICON


-- This is not the only handle / resource that should be
-- finalised for you, but it's a start.
-- ToDo.

type   HRGN       = ForeignHANDLE
type   PRGN       = HANDLE

type MbHRGN       = Maybe HRGN

type   HPALETTE   = HANDLE
type MbHPALETTE   = Maybe HPALETTE

type   HBRUSH     = HANDLE
type MbHBRUSH     = Maybe HBRUSH

type   HPEN       = HANDLE
type MbHPEN       = Maybe HPEN

type   HACCEL     = HANDLE

type   HDC        = HANDLE
type MbHDC        = Maybe HDC

type   HDWP       = HANDLE
type MbHDWP       = Maybe HDWP

type   HWND       = HANDLE
type MbHWND       = Maybe HWND

#{enum HWND, castUINTPtrToPtr
 , hWND_BOTTOM    = (UINT_PTR)HWND_BOTTOM
 , hWND_NOTOPMOST = (UINT_PTR)HWND_NOTOPMOST
 , hWND_TOP       = (UINT_PTR)HWND_TOP
 , hWND_TOPMOST   = (UINT_PTR)HWND_TOPMOST
 }

type   HMENU      = HANDLE
type MbHMENU      = Maybe HMENU

----------------------------------------------------------------
-- COLORREF
----------------------------------------------------------------

type COLORREF   = #{type COLORREF}

foreign import ccall unsafe "HsGDI.h"
  rgb :: BYTE -> BYTE -> BYTE -> COLORREF

foreign import ccall unsafe "HsGDI.h"
  getRValue :: COLORREF -> BYTE

foreign import ccall unsafe "HsGDI.h"
  getGValue :: COLORREF -> BYTE

foreign import ccall unsafe "HsGDI.h"
  getBValue :: COLORREF -> BYTE

foreign import ccall unsafe "HsGDI.h"
  pALETTERGB :: BYTE -> BYTE -> BYTE -> COLORREF

foreign import ccall unsafe "HsGDI.h"
  pALETTEINDEX :: WORD -> COLORREF

----------------------------------------------------------------
-- RasterOp macro
----------------------------------------------------------------

type RasterOp3 = Word32
type RasterOp4 = Word32

foreign import ccall unsafe "HsGDI.h"
  mAKEROP4 :: RasterOp3 -> RasterOp3 -> RasterOp4

----------------------------------------------------------------
-- Miscellaneous enumerations
----------------------------------------------------------------

type PolyFillMode   = INT
#{enum PolyFillMode,
 , aLTERNATE        = ALTERNATE
 , wINDING          = WINDING
 }

----------------------------------------------------------------

type ArcDirection   = INT
type MbArcDirection = Maybe ArcDirection
#{enum ArcDirection,
 , aD_COUNTERCLOCKWISE = AD_COUNTERCLOCKWISE
 , aD_CLOCKWISE        = AD_CLOCKWISE
 }

----------------------------------------------------------------

type GraphicsMode   = DWORD
type MbGraphicsMode = Maybe GraphicsMode
#{enum GraphicsMode,
 , gM_COMPATIBLE    = GM_COMPATIBLE
 , gM_ADVANCED      = GM_ADVANCED
 }

----------------------------------------------------------------

type BackgroundMode = INT
#{enum BackgroundMode,
 , tRANSPARENT  = TRANSPARENT
 , oPAQUE       = OPAQUE
 }

----------------------------------------------------------------

type HatchStyle   = INT
#{enum HatchStyle,
 , hS_HORIZONTAL  = HS_HORIZONTAL
 , hS_VERTICAL    = HS_VERTICAL
 , hS_FDIAGONAL   = HS_FDIAGONAL
 , hS_BDIAGONAL   = HS_BDIAGONAL
 , hS_CROSS       = HS_CROSS
 , hS_DIAGCROSS   = HS_DIAGCROSS
 }

----------------------------------------------------------------

type StretchBltMode    = INT
#{enum StretchBltMode,
 , bLACKONWHITE        = BLACKONWHITE
 , wHITEONBLACK        = WHITEONBLACK
 , cOLORONCOLOR        = COLORONCOLOR
 , hALFTONE            = HALFTONE
 , sTRETCH_ANDSCANS    = STRETCH_ANDSCANS
 , sTRETCH_ORSCANS     = STRETCH_ORSCANS
 , sTRETCH_DELETESCANS = STRETCH_DELETESCANS
 }

----------------------------------------------------------------

type TextAlignment = UINT
#{enum TextAlignment,
 , tA_NOUPDATECP   = TA_NOUPDATECP
 , tA_UPDATECP     = TA_UPDATECP
 , tA_LEFT         = TA_LEFT
 , tA_RIGHT        = TA_RIGHT
 , tA_CENTER       = TA_CENTER
 , tA_TOP          = TA_TOP
 , tA_BOTTOM       = TA_BOTTOM
 , tA_BASELINE     = TA_BASELINE
 }

----------------------------------------------------------------

type ClippingMode  = INT
#{enum ClippingMode,
 , rGN_AND         = RGN_AND
 , rGN_OR          = RGN_OR
 , rGN_XOR         = RGN_XOR
 , rGN_DIFF        = RGN_DIFF
 , rGN_COPY        = RGN_COPY
 }

----------------------------------------------------------------

type RegionType    = INT
#{enum RegionType,
 , eRROR           = ERROR
 , nULLREGION      = NULLREGION
 , sIMPLEREGION    = SIMPLEREGION
 , cOMPLEXREGION   = COMPLEXREGION
 }

gDI_ERROR  :: Num a => a
gDI_ERROR   = #{const GDI_ERROR}

cLR_INVALID :: COLORREF
cLR_INVALID = #{const CLR_INVALID}

----------------------------------------------------------------

#{enum UINT,
 , oBJ_PEN         = OBJ_PEN
 , oBJ_BRUSH       = OBJ_BRUSH
 , oBJ_DC          = OBJ_DC
 , oBJ_METADC      = OBJ_METADC
 , oBJ_PAL         = OBJ_PAL
 , oBJ_FONT        = OBJ_FONT
 , oBJ_BITMAP      = OBJ_BITMAP
 , oBJ_REGION      = OBJ_REGION
 , oBJ_METAFILE    = OBJ_METAFILE
 , oBJ_MEMDC       = OBJ_MEMDC
 , oBJ_EXTPEN      = OBJ_EXTPEN
 , oBJ_ENHMETADC   = OBJ_ENHMETADC
 , oBJ_ENHMETAFILE = OBJ_ENHMETAFILE
 }

----------------------------------------------------------------
-- Miscellaneous primitives
----------------------------------------------------------------

-- Can't pass structs with current FFI, so use C wrappers

foreign import ccall unsafe "HsGDI.h"
  prim_ChildWindowFromPoint :: HWND -> Ptr POINT -> IO HWND
foreign import ccall unsafe "HsGDI.h"
  prim_ChildWindowFromPointEx :: HWND -> Ptr POINT -> DWORD -> IO HWND
foreign import ccall unsafe "HsGDI.h"
  prim_MenuItemFromPoint :: HWND -> HMENU -> Ptr POINT -> IO UINT

----------------------------------------------------------------
-- End
----------------------------------------------------------------

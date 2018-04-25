#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.HDC
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

module Graphics.Win32.GDI.HDC
        ( module Graphics.Win32.GDI.HDC
        ) where

import System.Win32.Types
import Graphics.Win32.GDI.Types

import Foreign

#include "windows_cconv.h"

{- Note [Overflow checking and fromIntegral]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some windows APIs use the value 0x80000000 to represent failure return
codes. However, when GHC builds libraries with -XNegativeLiterals
enabled, it will fail in contexts where the type would suffer from
signed overflow - such as Int32. (minBound :: Int32 == 0x80000000)

Technically, the frontend is correct that the literal overflows in the
context it is used in. So instead, we use fromIntegral to convert the
literal from a Word32 to the necessary type. This isn't any less
efficient (fromIntegral is optimized away,) and conveys the idea we
simply want the same representational value.
-}

----------------------
-- Implement GetPixel
----------------------

getPixel :: HDC -> Int -> Int -> IO COLORREF
getPixel dc x y = c_GetPixel dc x y
foreign import WINDOWS_CCONV unsafe "windows.h GetPixel"
  c_GetPixel :: HDC -> Int -> Int -> IO COLORREF

----------------------

setArcDirection :: HDC -> ArcDirection -> IO ArcDirection
setArcDirection dc dir =
  failIfZero "SetArcDirection" $ c_SetArcDirection dc dir
foreign import WINDOWS_CCONV unsafe "windows.h SetArcDirection"
  c_SetArcDirection :: HDC ->  ArcDirection  -> IO  ArcDirection

getArcDirection :: HDC -> IO ArcDirection
getArcDirection dc =
  failIfZero "GetArcDirection" $ c_GetArcDirection dc
foreign import WINDOWS_CCONV unsafe "windows.h GetArcDirection"
  c_GetArcDirection :: HDC -> IO  ArcDirection

setPolyFillMode :: HDC -> PolyFillMode -> IO PolyFillMode
setPolyFillMode dc mode =
  failIfZero "SetPolyFillMode" $ c_SetPolyFillMode dc mode
foreign import WINDOWS_CCONV unsafe "windows.h SetPolyFillMode"
  c_SetPolyFillMode :: HDC ->  PolyFillMode  -> IO  PolyFillMode

getPolyFillMode :: HDC -> IO PolyFillMode
getPolyFillMode dc =
  failIfZero "GetPolyFillMode" $ c_GetPolyFillMode dc
foreign import WINDOWS_CCONV unsafe "windows.h GetPolyFillMode"
  c_GetPolyFillMode :: HDC -> IO  PolyFillMode

setGraphicsMode :: HDC -> GraphicsMode -> IO GraphicsMode
setGraphicsMode dc mode =
  failIfZero "SetGraphicsMode" $ c_SetGraphicsMode dc mode
foreign import WINDOWS_CCONV unsafe "windows.h SetGraphicsMode"
  c_SetGraphicsMode :: HDC ->  GraphicsMode  -> IO  GraphicsMode

getGraphicsMode :: HDC -> IO GraphicsMode
getGraphicsMode dc =
  failIfZero "GetGraphicsMode" $ c_GetGraphicsMode dc
foreign import WINDOWS_CCONV unsafe "windows.h GetGraphicsMode"
  c_GetGraphicsMode :: HDC -> IO  GraphicsMode

setStretchBltMode :: HDC -> StretchBltMode -> IO StretchBltMode
setStretchBltMode dc mode =
  failIfZero "SetStretchBltMode" $ c_SetStretchBltMode dc mode
foreign import WINDOWS_CCONV unsafe "windows.h SetStretchBltMode"
  c_SetStretchBltMode :: HDC ->  StretchBltMode  -> IO  StretchBltMode

getStretchBltMode :: HDC -> IO StretchBltMode
getStretchBltMode dc =
  failIfZero "GetStretchBltMode" $ c_GetStretchBltMode dc
foreign import WINDOWS_CCONV unsafe "windows.h GetStretchBltMode"
  c_GetStretchBltMode :: HDC -> IO  StretchBltMode

setBkColor :: HDC -> COLORREF -> IO COLORREF
setBkColor dc color =
  failIfZero "SetBkColor" $ c_SetBkColor dc color
foreign import WINDOWS_CCONV unsafe "windows.h SetBkColor"
  c_SetBkColor :: HDC ->  COLORREF  -> IO  COLORREF

getBkColor :: HDC -> IO COLORREF
getBkColor dc =
  failIfZero "GetBkColor" $ c_GetBkColor dc
foreign import WINDOWS_CCONV unsafe "windows.h GetBkColor"
  c_GetBkColor :: HDC -> IO  COLORREF

setTextColor :: HDC -> COLORREF -> IO COLORREF
setTextColor dc color =
  failIf (== cLR_INVALID) "SetTextColor" $ c_SetTextColor dc color
foreign import WINDOWS_CCONV unsafe "windows.h SetTextColor"
  c_SetTextColor :: HDC ->  COLORREF  -> IO  COLORREF

getTextColor :: HDC -> IO COLORREF
getTextColor dc =
  failIf (== cLR_INVALID) "GetTextColor" $ c_GetTextColor dc
foreign import WINDOWS_CCONV unsafe "windows.h GetTextColor"
  c_GetTextColor :: HDC -> IO  COLORREF

setBkMode :: HDC -> BackgroundMode -> IO BackgroundMode
setBkMode dc mode =
  failIfZero "SetBkMode" $ c_SetBkMode dc mode
foreign import WINDOWS_CCONV unsafe "windows.h SetBkMode"
  c_SetBkMode :: HDC ->  BackgroundMode  -> IO  BackgroundMode

getBkMode :: HDC -> IO BackgroundMode
getBkMode dc =
  failIfZero "GetBkMode" $ c_GetBkMode dc
foreign import WINDOWS_CCONV unsafe "windows.h GetBkMode"
  c_GetBkMode :: HDC -> IO  BackgroundMode

setBrushOrgEx :: HDC -> Int -> Int -> IO POINT
setBrushOrgEx dc x y =
  allocaPOINT $ \ pt -> do
  failIfFalse_ "SetBrushOrgEx" $ c_SetBrushOrgEx dc x y pt
  peekPOINT pt
foreign import WINDOWS_CCONV unsafe "windows.h SetBrushOrgEx"
  c_SetBrushOrgEx :: HDC -> Int -> Int -> Ptr POINT -> IO Bool

getBrushOrgEx :: HDC -> IO POINT
getBrushOrgEx dc =
  allocaPOINT $ \ pt -> do
  failIfFalse_ "GetBrushOrgEx" $ c_GetBrushOrgEx dc pt
  peekPOINT pt
foreign import WINDOWS_CCONV unsafe "windows.h GetBrushOrgEx"
  c_GetBrushOrgEx :: HDC -> Ptr POINT -> IO Bool

setTextAlign :: HDC -> TextAlignment -> IO TextAlignment
setTextAlign dc align =
  failIf (== gDI_ERROR) "SetTextAlign" $ c_SetTextAlign dc align
foreign import WINDOWS_CCONV unsafe "windows.h SetTextAlign"
  c_SetTextAlign :: HDC ->  TextAlignment  -> IO  TextAlignment

getTextAlign :: HDC -> IO TextAlignment
getTextAlign dc =
  failIf (== gDI_ERROR) "GetTextAlign" $ c_GetTextAlign dc
foreign import WINDOWS_CCONV unsafe "windows.h GetTextAlign"
  c_GetTextAlign :: HDC -> IO  TextAlignment

setTextCharacterExtra :: HDC -> Int -> IO Int
setTextCharacterExtra dc extra =
  -- See Note [Overflow checking and fromIntegral]
  failIf (== fromIntegral (0x80000000 :: Word32)) "SetTextCharacterExtra" $
    c_SetTextCharacterExtra dc extra
foreign import WINDOWS_CCONV unsafe "windows.h SetTextCharacterExtra"
  c_SetTextCharacterExtra :: HDC ->  Int  -> IO  Int

getTextCharacterExtra :: HDC -> IO Int
getTextCharacterExtra dc =
  -- See Note [Overflow checking and fromIntegral]
  failIf (== fromIntegral (0x80000000 :: Word32)) "GetTextCharacterExtra" $ c_GetTextCharacterExtra dc
foreign import WINDOWS_CCONV unsafe "windows.h GetTextCharacterExtra"
  c_GetTextCharacterExtra :: HDC -> IO  Int

getMiterLimit :: HDC -> IO Float
getMiterLimit dc =
  alloca $ \ p_res -> do
  failIfFalse_ "GetMiterLimit" $ c_GetMiterLimit dc p_res
  peek p_res
foreign import WINDOWS_CCONV unsafe "windows.h GetMiterLimit"
  c_GetMiterLimit :: HDC -> Ptr FLOAT -> IO Bool

setMiterLimit :: HDC -> Float -> IO Float
setMiterLimit dc new_limit =
  alloca $ \ p_old_limit -> do
  failIfFalse_ "SetMiterLimit" $ c_SetMiterLimit dc new_limit p_old_limit
  peek p_old_limit
foreign import WINDOWS_CCONV unsafe "windows.h SetMiterLimit"
  c_SetMiterLimit :: HDC -> FLOAT -> Ptr FLOAT -> IO Bool

----------------------------------------------------------------

saveDC :: HDC -> IO Int
saveDC dc =
  failIfZero "SaveDC" $ c_SaveDC dc
foreign import WINDOWS_CCONV unsafe "windows.h SaveDC"
  c_SaveDC :: HDC -> IO Int

restoreDC :: HDC -> Int -> IO ()
restoreDC dc saved =
  failIfFalse_ "RestoreDC" $ c_RestoreDC dc saved
foreign import WINDOWS_CCONV unsafe "windows.h RestoreDC"
  c_RestoreDC :: HDC -> Int -> IO Bool

----------------------------------------------------------------

getCurrentBitmap :: HDC -> IO HBITMAP
getCurrentBitmap dc =
  failIfNull "GetCurrentBitmap" $ c_GetCurrentBitmap dc oBJ_BITMAP
foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentObject"
  c_GetCurrentBitmap :: HDC -> UINT -> IO  HBITMAP

getCurrentBrush :: HDC -> IO HBRUSH
getCurrentBrush dc =
  failIfNull "GetCurrentBrush" $ c_GetCurrentBrush dc oBJ_BRUSH
foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentObject"
  c_GetCurrentBrush :: HDC -> UINT -> IO   HBRUSH

getCurrentFont :: HDC -> IO HFONT
getCurrentFont dc =
  failIfNull "GetCurrentFont" $ c_GetCurrentFont dc oBJ_FONT
foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentObject"
  c_GetCurrentFont :: HDC -> UINT -> IO    HFONT

getCurrentPalette :: HDC -> IO HPALETTE
getCurrentPalette dc =
  failIfNull "GetCurrentPalette" $ c_GetCurrentPalette dc oBJ_PAL
foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentObject"
  c_GetCurrentPalette :: HDC -> UINT -> IO     HPALETTE

getCurrentPen :: HDC -> IO HPEN
getCurrentPen dc =
  failIfNull "GetCurrentPen" $ c_GetCurrentPen dc oBJ_PEN
foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentObject"
  c_GetCurrentPen :: HDC -> UINT -> IO     HPEN

selectBitmap :: HDC -> HBITMAP -> IO HBITMAP
selectBitmap dc bitmap =
  failIfNull "SelectBitmap" $ c_SelectBitmap dc bitmap
foreign import WINDOWS_CCONV unsafe "windows.h SelectObject"
  c_SelectBitmap :: HDC ->   HBITMAP  -> IO   HBITMAP

selectBrush :: HDC -> HBRUSH -> IO HBRUSH
selectBrush dc brush =
  failIfNull "SelectBrush" $ c_SelectBrush dc brush
foreign import WINDOWS_CCONV unsafe "windows.h SelectObject"
  c_SelectBrush :: HDC ->    HBRUSH  -> IO    HBRUSH

selectFont :: HDC -> HFONT -> IO HFONT
selectFont dc font =
  failIfNull "SelectFont" $ c_SelectFont dc font
foreign import WINDOWS_CCONV unsafe "windows.h SelectObject"
  c_SelectFont :: HDC ->     HFONT  -> IO     HFONT

selectPen :: HDC -> HPEN -> IO HPEN
selectPen dc pen =
  failIfNull "SelectPen" $ c_SelectPen dc pen
foreign import WINDOWS_CCONV unsafe "windows.h SelectObject"
  c_SelectPen :: HDC ->      HPEN  -> IO      HPEN

----------------------------------------------------------------
--
----------------------------------------------------------------

selectPalette :: HDC -> HPALETTE -> Bool -> IO HPALETTE
selectPalette dc palette force_bg =
  failIfNull "SelectPalette" $ c_SelectPalette dc palette force_bg
foreign import WINDOWS_CCONV unsafe "windows.h SelectPalette"
  c_SelectPalette :: HDC -> HPALETTE -> Bool -> IO HPALETTE

selectRgn :: HDC -> HRGN -> IO RegionType
selectRgn dc rgn =
  withForeignPtr rgn $ \ p_rgn ->
  failIf (== gDI_ERROR) "SelectRgn" $ c_SelectRgn dc p_rgn
foreign import ccall unsafe "windows.h SelectObjectInt"
  c_SelectRgn :: HDC -> PRGN -> IO RegionType
-- avoid using SelectObject() at different types by calling our own
-- wrapper.

selectClipRgn :: HDC -> Maybe HRGN -> IO RegionType
selectClipRgn dc mb_rgn =
  maybeWith withForeignPtr mb_rgn $ \ p_rgn ->
  failIfZero "SelectClipRgn" $ c_SelectClipRgn dc p_rgn
foreign import WINDOWS_CCONV unsafe "windows.h SelectClipRgn"
  c_SelectClipRgn :: HDC -> PRGN -> IO RegionType

extSelectClipRgn :: HDC -> Maybe HRGN -> ClippingMode -> IO RegionType
extSelectClipRgn dc mb_rgn mode =
  maybeWith withForeignPtr mb_rgn $ \ p_rgn ->
  failIfZero "ExtSelectClipRgn" $ c_ExtSelectClipRgn dc p_rgn mode
foreign import WINDOWS_CCONV unsafe "windows.h ExtSelectClipRgn"
  c_ExtSelectClipRgn :: HDC -> PRGN -> ClippingMode -> IO RegionType

selectClipPath :: HDC -> ClippingMode -> IO RegionType
selectClipPath dc mode =
  failIfZero "SelectClipPath" $ c_SelectClipPath dc mode
foreign import WINDOWS_CCONV unsafe "windows.h SelectClipPath"
  c_SelectClipPath :: HDC -> ClippingMode -> IO RegionType

----------------------------------------------------------------
-- Misc
----------------------------------------------------------------

cancelDC :: HDC -> IO ()
cancelDC dc =
  failIfFalse_ "CancelDC" $ c_CancelDC dc
foreign import WINDOWS_CCONV unsafe "windows.h CancelDC"
  c_CancelDC :: HDC -> IO Bool

createCompatibleDC :: Maybe HDC -> IO HDC
createCompatibleDC mb_dc =
  failIfNull "CreateCompatibleDC" $ c_CreateCompatibleDC (maybePtr mb_dc)
foreign import WINDOWS_CCONV unsafe "windows.h CreateCompatibleDC"
  c_CreateCompatibleDC :: HDC -> IO HDC

deleteDC :: HDC -> IO ()
deleteDC dc =
  failIfFalse_ "DeleteDC" $ c_DeleteDC dc
foreign import WINDOWS_CCONV unsafe "windows.h DeleteDC"
  c_DeleteDC :: HDC -> IO Bool

----------------------------------------------------------------
-- End
----------------------------------------------------------------

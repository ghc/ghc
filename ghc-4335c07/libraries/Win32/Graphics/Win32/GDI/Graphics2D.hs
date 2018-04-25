#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.Graphics2D
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- 2D graphics operations
--
-----------------------------------------------------------------------------

module Graphics.Win32.GDI.Graphics2D
        where

import System.Win32.Types
import Graphics.Win32.GDI.Types

import Foreign

#include "windows_cconv.h"

----------------------------------------------------------------
-- Lines and Curves
----------------------------------------------------------------

moveToEx :: HDC -> Int32 -> Int32 -> IO POINT
moveToEx dc x y =
  allocaPOINT $ \ p_point -> do
  failIfFalse_ "MoveToEx" $ c_MoveToEx dc x y p_point
  peekPOINT p_point
foreign import WINDOWS_CCONV unsafe "windows.h MoveToEx"
  c_MoveToEx :: HDC -> Int32 -> Int32 -> Ptr POINT -> IO Bool

lineTo :: HDC -> Int32 -> Int32 -> IO ()
lineTo dc x y =
  failIfFalse_ "LineTo" $ c_LineTo dc x y
foreign import WINDOWS_CCONV unsafe "windows.h LineTo"
  c_LineTo :: HDC -> Int32 -> Int32 -> IO Bool

polyline :: HDC -> [POINT] -> IO ()
polyline dc points =
  withPOINTArray points $ \ pount_array npoints ->
  failIfFalse_ "Polyline" $ c_Polyline dc pount_array npoints
foreign import WINDOWS_CCONV unsafe "windows.h Polyline"
  c_Polyline :: HDC -> Ptr POINT -> Int -> IO Bool

polylineTo :: HDC -> [POINT] -> IO ()
polylineTo dc points =
  withPOINTArray points $ \ pount_array npoints ->
  failIfFalse_ "PolylineTo" $ c_PolylineTo dc pount_array npoints
foreign import WINDOWS_CCONV unsafe "windows.h PolylineTo"
  c_PolylineTo :: HDC -> Ptr POINT -> Int -> IO Bool

polygon :: HDC -> [POINT] -> IO ()
polygon dc points =
  withPOINTArray points $ \ pount_array npoints ->
  failIfFalse_ "Polygon" $ c_Polygon dc pount_array npoints
foreign import WINDOWS_CCONV unsafe "windows.h Polygon"
  c_Polygon :: HDC -> Ptr POINT -> Int -> IO Bool

polyBezier :: HDC -> [POINT] -> IO ()
polyBezier dc points =
  withPOINTArray points $ \ pount_array npoints ->
  failIfFalse_ "PolyBezier" $ c_PolyBezier dc pount_array npoints
foreign import WINDOWS_CCONV unsafe "windows.h PolyBezier"
  c_PolyBezier :: HDC -> Ptr POINT -> Int -> IO Bool

polyBezierTo :: HDC -> [POINT] -> IO ()
polyBezierTo dc points =
  withPOINTArray points $ \ pount_array npoints ->
  failIfFalse_ "PolyBezierTo" $ c_PolyBezierTo dc pount_array npoints
foreign import WINDOWS_CCONV unsafe "windows.h PolyBezierTo"
  c_PolyBezierTo :: HDC -> Ptr POINT -> Int -> IO Bool

arc :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
arc dc left top right bottom x1 y1 x2 y2 =
  failIfFalse_ "Arc" $ c_Arc dc left top right bottom x1 y1 x2 y2
foreign import WINDOWS_CCONV unsafe "windows.h Arc"
  c_Arc :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO Bool

arcTo :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
arcTo dc left top right bottom x1 y1 x2 y2 =
  failIfFalse_ "ArcTo" $ c_ArcTo dc left top right bottom x1 y1 x2 y2
foreign import WINDOWS_CCONV unsafe "windows.h ArcTo"
  c_ArcTo :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO Bool

angleArc :: HDC -> Int32 -> Int32 -> WORD -> Float -> Float -> IO ()
angleArc dc x y r start sweep =
  failIfFalse_ "AngleArc" $ c_AngleArc dc x y r start sweep
foreign import WINDOWS_CCONV unsafe "windows.h AngleArc"
  c_AngleArc :: HDC -> Int32 -> Int32 -> WORD -> Float -> Float -> IO Bool

----------------------------------------------------------------
-- Filled Shapes
----------------------------------------------------------------

-- ToDo: We ought to be able to specify a colour instead of the
-- Brush by adding 1 to colour number.

fillRect :: HDC -> RECT -> HBRUSH -> IO ()
fillRect dc rect brush =
  withRECT rect $ \ c_rect ->
  failIfFalse_ "FillRect" $ c_FillRect dc c_rect brush
foreign import WINDOWS_CCONV unsafe "windows.h FillRect"
  c_FillRect :: HDC -> Ptr RECT -> HBRUSH -> IO Bool

frameRect :: HDC -> RECT -> HBRUSH -> IO ()
frameRect dc rect brush =
  withRECT rect $ \ c_rect ->
  failIfFalse_ "FrameRect" $ c_FrameRect dc c_rect brush
foreign import WINDOWS_CCONV unsafe "windows.h FrameRect"
  c_FrameRect :: HDC -> Ptr RECT -> HBRUSH -> IO Bool

invertRect :: HDC -> RECT -> IO ()
invertRect dc rect =
  withRECT rect $ \ c_rect ->
  failIfFalse_ "InvertRect" $ c_InvertRect dc c_rect
foreign import WINDOWS_CCONV unsafe "windows.h InvertRect"
  c_InvertRect :: HDC -> Ptr RECT -> IO Bool

rectangle :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
rectangle dc left top right bottom =
  failIfFalse_ "Rectangle" $ c_Rectangle dc left top right bottom
foreign import WINDOWS_CCONV unsafe "windows.h Rectangle"
  c_Rectangle :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> IO Bool

roundRect :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
roundRect dc left top right bottom w h =
  failIfFalse_ "RoundRect" $ c_RoundRect dc left top right bottom w h
foreign import WINDOWS_CCONV unsafe "windows.h RoundRect"
  c_RoundRect :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO Bool

ellipse :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
ellipse dc left top right bottom =
  failIfFalse_ "Ellipse" $ c_Ellipse dc left top right bottom
foreign import WINDOWS_CCONV unsafe "windows.h Ellipse"
  c_Ellipse :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> IO Bool

chord :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
chord dc left top right bottom x1 y1 x2 y2 =
  failIfFalse_ "Chord" $ c_Chord dc left top right bottom x1 y1 x2 y2
foreign import WINDOWS_CCONV unsafe "windows.h Chord"
  c_Chord :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO Bool

pie :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
pie dc left top right bottom x1 y1 x2 y2 =
  failIfFalse_ "Pie" $ c_Pie dc left top right bottom x1 y1 x2 y2
foreign import WINDOWS_CCONV unsafe "windows.h Pie"
  c_Pie :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO Bool

----------------------------------------------------------------
-- Bitmaps
----------------------------------------------------------------

bitBlt :: HDC -> INT -> INT -> INT -> INT -> HDC -> INT -> INT -> RasterOp3 -> IO ()
bitBlt dcDest xDest yDest w h dcSrc xSrc ySrc rop =
  failIfFalse_ "BitBlt" $ c_BitBlt dcDest xDest yDest w h dcSrc xSrc ySrc rop
foreign import WINDOWS_CCONV unsafe "windows.h BitBlt"
  c_BitBlt :: HDC -> INT -> INT -> INT -> INT -> HDC -> INT -> INT -> RasterOp3 -> IO Bool

maskBlt :: HDC -> INT -> INT -> INT -> INT -> HDC -> INT -> INT -> HBITMAP -> INT -> INT -> RasterOp4 -> IO ()
maskBlt dcDest xDest yDest w h dcSrc xSrc ySrc bm xMask yMask rop =
  failIfFalse_ "MaskBlt" $
    c_MaskBlt dcDest xDest yDest w h dcSrc xSrc ySrc bm xMask yMask rop
foreign import WINDOWS_CCONV unsafe "windows.h MaskBlt"
  c_MaskBlt :: HDC -> INT -> INT -> INT -> INT -> HDC -> INT -> INT -> HBITMAP -> INT -> INT -> RasterOp4 -> IO Bool

stretchBlt :: HDC -> INT -> INT -> INT -> INT -> HDC -> INT -> INT -> INT -> INT -> RasterOp3 -> IO ()
stretchBlt dcDest xDest yDest wDest hDest hdcSrc xSrc ySrc wSrc hSrc rop =
  failIfFalse_ "StretchBlt" $
    c_StretchBlt dcDest xDest yDest wDest hDest hdcSrc xSrc ySrc wSrc hSrc rop
foreign import WINDOWS_CCONV unsafe "windows.h StretchBlt"
  c_StretchBlt :: HDC -> INT -> INT -> INT -> INT -> HDC -> INT -> INT -> INT -> INT -> RasterOp3 -> IO Bool

-- We deviate slightly from the Win32 interface

-- %C typedef POINT ThreePts[3];

-- Old 2nd line:
-- %start POINT vertices[3];

plgBlt :: HDC -> POINT -> POINT -> POINT -> HDC -> INT -> INT -> INT -> INT -> MbHBITMAP -> INT -> INT -> IO ()
plgBlt hdDest p1 p2 p3 hdSrc x y w h mb_bm xMask yMask =
  withPOINTArray [p1,p2,p3] $ \ vertices _ ->
  failIfFalse_ "PlgBlt" $
    c_PlgBlt hdDest vertices hdSrc x y w h (maybePtr mb_bm) xMask yMask
foreign import WINDOWS_CCONV unsafe "windows.h PlgBlt"
  c_PlgBlt :: HDC -> Ptr POINT -> HDC -> INT -> INT -> INT -> INT -> HBITMAP -> INT -> INT -> IO Bool

----------------------------------------------------------------
-- Fonts and Text
----------------------------------------------------------------

textOut :: HDC -> INT -> INT -> String -> IO ()
textOut dc x y str =
  withTStringLen str $ \ (c_str, len) ->
  failIfFalse_ "TextOut" $ c_TextOut dc x y c_str len
foreign import WINDOWS_CCONV unsafe "windows.h TextOutW"
  c_TextOut :: HDC -> INT -> INT -> LPCTSTR -> Int -> IO Bool

-- missing TabbedTextOut from WinFonts.ss; GSL ???

getTextExtentPoint32 :: HDC -> String -> IO SIZE
getTextExtentPoint32 dc str =
  withTStringLen str $ \ (c_str, len) ->
  allocaSIZE $ \ p_size -> do
  failIfFalse_ "GetTextExtentPoint32" $
    c_GetTextExtentPoint32 dc c_str len p_size
  peekSIZE p_size
foreign import WINDOWS_CCONV unsafe "windows.h GetTextExtentPoint32W"
  c_GetTextExtentPoint32 :: HDC -> LPCTSTR -> Int -> Ptr SIZE -> IO Bool

-- missing getTabbedTextExtent from WinFonts.ss; GSL ???
-- missing SetTextJustification from WinFonts.ss; GSL ???
-- missing a whole family of techandfamily functionality; GSL ???
-- missing DrawText and DrawTextFormat in WinFonts.ss; GSL ???

----------------------------------------------------------------
-- End
----------------------------------------------------------------

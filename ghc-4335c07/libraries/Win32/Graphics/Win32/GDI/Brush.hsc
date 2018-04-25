#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.Brush
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

module Graphics.Win32.GDI.Brush where

import System.Win32.Types
import Graphics.Win32.GDI.Types

##include "windows_cconv.h"

#include <windows.h>

----------------------------------------------------------------
-- Brush
----------------------------------------------------------------

createSolidBrush :: COLORREF -> IO HBRUSH
createSolidBrush color =
  failIfNull "CreateSolidBrush" $ c_CreateSolidBrush color
foreign import WINDOWS_CCONV unsafe "windows.h CreateSolidBrush"
  c_CreateSolidBrush :: COLORREF -> IO HBRUSH

createHatchBrush :: HatchStyle -> COLORREF -> IO HBRUSH
createHatchBrush style color =
  failIfNull "CreateHatchBrush" $ c_CreateHatchBrush style color
foreign import WINDOWS_CCONV unsafe "windows.h CreateHatchBrush"
  c_CreateHatchBrush :: HatchStyle -> COLORREF -> IO HBRUSH

createPatternBrush :: HBITMAP -> IO HBRUSH
createPatternBrush bitmap =
  failIfNull "CreatePatternBrush" $ c_CreatePatternBrush bitmap
foreign import WINDOWS_CCONV unsafe "windows.h CreatePatternBrush"
  c_CreatePatternBrush :: HBITMAP -> IO HBRUSH

deleteBrush :: HBRUSH -> IO ()
deleteBrush brush =
  failIfFalse_ "DeleteBrush" $ c_DeleteBrush brush
foreign import WINDOWS_CCONV unsafe "windows.h DeleteObject"
  c_DeleteBrush :: HBRUSH -> IO Bool

----------------------------------------------------------------

type StockBrush   = INT

#{enum StockBrush,
 , wHITE_BRUSH  = WHITE_BRUSH
 , lTGRAY_BRUSH = LTGRAY_BRUSH
 , gRAY_BRUSH   = GRAY_BRUSH
 , dKGRAY_BRUSH = DKGRAY_BRUSH
 , bLACK_BRUSH  = BLACK_BRUSH
 , nULL_BRUSH   = NULL_BRUSH
 , hOLLOW_BRUSH = HOLLOW_BRUSH
 }

getStockBrush :: StockBrush -> IO HBRUSH
getStockBrush sb =
  failIfNull "GetStockBrush" $ c_GetStockBrush sb
foreign import WINDOWS_CCONV unsafe "windows.h GetStockObject"
  c_GetStockBrush :: StockBrush -> IO HBRUSH

----------------------------------------------------------------
-- End
----------------------------------------------------------------

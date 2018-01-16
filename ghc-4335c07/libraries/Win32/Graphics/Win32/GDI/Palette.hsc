#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.Palette
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

module Graphics.Win32.GDI.Palette where

import System.Win32.Types
import Graphics.Win32.GDI.Types

##include "windows_cconv.h"
#include <windows.h>

----------------------------------------------------------------
-- Palettes
----------------------------------------------------------------

type StockPalette   = WORD

#{enum StockPalette,
 , dEFAULT_PALETTE = DEFAULT_PALETTE
 }

getStockPalette :: StockPalette -> IO HPALETTE
getStockPalette sp =
  failIfNull "GetStockPalette" $ c_GetStockPalette sp
foreign import WINDOWS_CCONV unsafe "windows.h GetStockObject"
  c_GetStockPalette :: StockPalette -> IO HPALETTE

deletePalette :: HPALETTE -> IO ()
deletePalette p =
  failIfFalse_ "DeletePalette" $ c_DeletePalette p
foreign import WINDOWS_CCONV unsafe "windows.h DeleteObject"
  c_DeletePalette :: HPALETTE -> IO Bool

----------------------------------------------------------------
-- End
----------------------------------------------------------------

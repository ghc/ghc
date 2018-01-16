#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.Path
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

module Graphics.Win32.GDI.Path
        ( beginPath, closeFigure, endPath, fillPath, flattenPath
        , pathToRegion, strokeAndFillPath, strokePath, widenPath
        ) where

import Graphics.Win32.GDI.Types
import System.Win32.Types

#include "windows_cconv.h"

----------------------------------------------------------------
-- Paths
----------------------------------------------------------------

-- AbortPath       :: HDC -> IO ()

beginPath :: HDC -> IO ()
beginPath dc =
  failIfFalse_ "BeginPath" $ c_BeginPath dc
foreign import WINDOWS_CCONV unsafe "windows.h BeginPath"
  c_BeginPath :: HDC -> IO Bool

closeFigure :: HDC -> IO ()
closeFigure dc =
  failIfFalse_ "CloseFigure" $ c_CloseFigure dc
foreign import WINDOWS_CCONV unsafe "windows.h CloseFigure"
  c_CloseFigure :: HDC -> IO Bool

endPath :: HDC -> IO ()
endPath dc =
  failIfFalse_ "EndPath" $ c_EndPath dc
foreign import WINDOWS_CCONV unsafe "windows.h EndPath"
  c_EndPath :: HDC -> IO Bool

fillPath :: HDC -> IO ()
fillPath dc =
  failIfFalse_ "FillPath" $ c_FillPath dc
foreign import WINDOWS_CCONV unsafe "windows.h FillPath"
  c_FillPath :: HDC -> IO Bool

flattenPath :: HDC -> IO ()
flattenPath dc =
  failIfFalse_ "FlattenPath" $ c_FlattenPath dc
foreign import WINDOWS_CCONV unsafe "windows.h FlattenPath"
  c_FlattenPath :: HDC -> IO Bool

pathToRegion :: HDC -> IO HRGN
pathToRegion dc = do
  ptr <- failIfNull "PathToRegion" $ c_PathToRegion dc
  newForeignHANDLE ptr
foreign import WINDOWS_CCONV unsafe "windows.h PathToRegion"
  c_PathToRegion :: HDC -> IO PRGN

strokeAndFillPath :: HDC -> IO ()
strokeAndFillPath dc =
  failIfFalse_ "StrokeAndFillPath" $ c_StrokeAndFillPath dc
foreign import WINDOWS_CCONV unsafe "windows.h StrokeAndFillPath"
  c_StrokeAndFillPath :: HDC -> IO Bool

strokePath :: HDC -> IO ()
strokePath dc =
  failIfFalse_ "StrokePath" $ c_StrokePath dc
foreign import WINDOWS_CCONV unsafe "windows.h StrokePath"
  c_StrokePath :: HDC -> IO Bool

widenPath :: HDC -> IO ()
widenPath dc =
  failIfFalse_ "WidenPath" $ c_WidenPath dc
foreign import WINDOWS_CCONV unsafe "windows.h WidenPath"
  c_WidenPath :: HDC -> IO Bool

----------------------------------------------------------------
-- End
----------------------------------------------------------------

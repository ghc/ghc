#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- An interface to the Microsoft Windows graphics device interface (GDI).
-- See <http://msdn.microsoft.com/library/> under /Graphics and Multimedia/
-- for more details of the underlying library.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
-- for details

module Graphics.Win32.GDI (
        module Graphics.Win32.GDI.Bitmap,
        module Graphics.Win32.GDI.Brush,
        module Graphics.Win32.GDI.Clip,
        module Graphics.Win32.GDI.Font,
        module Graphics.Win32.GDI.Graphics2D,
        module Graphics.Win32.GDI.HDC,
        module Graphics.Win32.GDI.Palette,
        module Graphics.Win32.GDI.Path,
        module Graphics.Win32.GDI.Pen,
        module Graphics.Win32.GDI.Region,
        module Graphics.Win32.GDI.Types
        ) where

import Graphics.Win32.GDI.Bitmap
import Graphics.Win32.GDI.Brush
import Graphics.Win32.GDI.Clip
import Graphics.Win32.GDI.Font
import Graphics.Win32.GDI.Graphics2D
import Graphics.Win32.GDI.HDC
import Graphics.Win32.GDI.Palette
import Graphics.Win32.GDI.Path
import Graphics.Win32.GDI.Pen
import Graphics.Win32.GDI.Region
import Graphics.Win32.GDI.Types

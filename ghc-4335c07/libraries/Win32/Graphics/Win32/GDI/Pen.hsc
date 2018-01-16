#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.Pen
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

module Graphics.Win32.GDI.Pen where

import System.Win32.Types
import Graphics.Win32.GDI.Types

##include "windows_cconv.h"

#include <windows.h>

----------------------------------------------------------------
-- Stock Objects
----------------------------------------------------------------

type StockPen   = INT

#{enum StockPen,
 , wHITE_PEN    = WHITE_PEN
 , bLACK_PEN    = BLACK_PEN
 , nULL_PEN     = NULL_PEN
 }

getStockPen :: StockPen -> IO HPEN
getStockPen stockpen =
  failIfNull "GetStockPen" $ c_GetStockPen stockpen
foreign import WINDOWS_CCONV unsafe "windows.h GetStockObject"
  c_GetStockPen :: StockPen -> IO HPEN

deletePen :: HPEN -> IO ()
deletePen pen =
  failIfFalse_ "DeletePen" $ c_DeletePen pen
foreign import WINDOWS_CCONV unsafe "windows.h DeleteObject"
  c_DeletePen :: HPEN -> IO Bool

----------------------------------------------------------------
-- Creating pens
----------------------------------------------------------------

type PenStyle   = INT

#{enum PenStyle,
 , pS_SOLID             = PS_SOLID            // default
 , pS_DASH              = PS_DASH             // -------
 , pS_DOT               = PS_DOT              // .......
 , pS_DASHDOT           = PS_DASHDOT          // _._._._
 , pS_DASHDOTDOT        = PS_DASHDOTDOT       // _.._.._
 , pS_NULL              = PS_NULL
 , pS_INSIDEFRAME       = PS_INSIDEFRAME
 , pS_USERSTYLE         = PS_USERSTYLE
 , pS_ALTERNATE         = PS_ALTERNATE
 , pS_STYLE_MASK        = PS_STYLE_MASK       // all the above
 }

#{enum PenStyle,
 , pS_ENDCAP_ROUND      = PS_ENDCAP_ROUND     // default
 , pS_ENDCAP_SQUARE     = PS_ENDCAP_SQUARE
 , pS_ENDCAP_FLAT       = PS_ENDCAP_FLAT
 , pS_ENDCAP_MASK       = PS_ENDCAP_MASK      // all the above
 }

#{enum PenStyle,
 , pS_JOIN_ROUND        = PS_JOIN_ROUND       // default
 , pS_JOIN_BEVEL        = PS_JOIN_BEVEL
 , pS_JOIN_MITER        = PS_JOIN_MITER
 }
-- , pS_JOIN_MASK         = PS_JOIN_MASK
{-
If PS_JOIN_MASK is not defined with your GNU Windows32 header files,
you'll have to define it.
-}

#{enum PenStyle,
 , pS_COSMETIC          = PS_COSMETIC         // default
 , pS_GEOMETRIC         = PS_GEOMETRIC
 , pS_TYPE_MASK         = PS_TYPE_MASK        // all the above
 }

createPen :: PenStyle -> INT -> COLORREF -> IO HPEN
createPen style n color =
  failIfNull "CreatePen" $ c_CreatePen style n color
foreign import WINDOWS_CCONV unsafe "windows.h CreatePen"
  c_CreatePen :: PenStyle -> INT -> COLORREF -> IO HPEN

-- Not very well supported on Win'95
-- %fun NullHANDLE ExtCreatePen :: PenStyle -> INT -> LOGBRUSH -> [StyleBit] -> IO HPEN

-- ToDo: CreatePenIndirect

----------------------------------------------------------------
-- End
----------------------------------------------------------------

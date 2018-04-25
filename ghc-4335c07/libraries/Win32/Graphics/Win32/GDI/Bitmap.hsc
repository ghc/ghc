#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.Bitmap
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

module Graphics.Win32.GDI.Bitmap(
        RasterOp3,
        RasterOp4,
        sRCCOPY,
        sRCPAINT,
        sRCAND,
        sRCINVERT,
        sRCERASE,
        nOTSRCCOPY,
        nOTSRCERASE,
        mERGECOPY,
        mERGEPAINT,
        pATCOPY,
        pATPAINT,
        pATINVERT,
        dSTINVERT,
        bLACKNESS,
        wHITENESS,

        mAKEROP4,

        BITMAP,
        LPBITMAP,
        setBITMAP,
        deleteBitmap,
        createCompatibleBitmap,
        createBitmap,
        createBitmapIndirect,
        createDIBPatternBrushPt,
        getBitmapDimensionEx,
        setBitmapDimensionEx,
        getBitmapInfo,

        BitmapCompression,
        bI_RGB,
        bI_RLE8,
        bI_RLE4,
        bI_BITFIELDS,

        ColorFormat,
        dIB_PAL_COLORS,
        dIB_RGB_COLORS,

        LPBITMAPINFO,
        BITMAPINFOHEADER,
        LPBITMAPINFOHEADER,
        getBITMAPINFOHEADER_,

        BITMAPFILEHEADER,
        LPBITMAPFILEHEADER,
        getBITMAPFILEHEADER,

        sizeofBITMAP,
        sizeofBITMAPINFO,
        sizeofBITMAPINFOHEADER,
        sizeofBITMAPFILEHEADER,
        sizeofLPBITMAPFILEHEADER,

        createBMPFile,
        cBM_INIT,
        getDIBits,
        setDIBits,
        createDIBitmap

        ) where

import System.Win32.Types
import Graphics.Win32.GDI.Types

import Control.Monad (liftM)
import Foreign
import Foreign.C

#include "Win32Aux.h"
##include "windows_cconv.h"

#include <windows.h>

----------------------------------------------------------------
-- Resources
----------------------------------------------------------------

-- Yoiks - name clash
-- %dis bitmap x = ptr ({LPTSTR} x)
--
-- type Bitmap = LPCTSTR
--
-- intToBitmap :: Int -> Bitmap
-- intToBitmap i = makeIntResource (toWord i)
--
-- %fun LoadBitmap :: MbHINSTANCE -> Bitmap -> IO HBITMAP
-- %fail { res1 == 0 } { ErrorString("LoadBitmap") }
--
-- %const Bitmap
-- % [ OBM_CLOSE        = { MAKEINTRESOURCE(OBM_CLOSE)       }
-- % , OBM_UPARROW      = { MAKEINTRESOURCE(OBM_UPARROW)     }
-- % , OBM_DNARROW      = { MAKEINTRESOURCE(OBM_DNARROW)     }
-- % , OBM_RGARROW      = { MAKEINTRESOURCE(OBM_RGARROW)     }
-- % , OBM_LFARROW      = { MAKEINTRESOURCE(OBM_LFARROW)     }
-- % , OBM_REDUCE       = { MAKEINTRESOURCE(OBM_REDUCE)      }
-- % , OBM_ZOOM         = { MAKEINTRESOURCE(OBM_ZOOM)        }
-- % , OBM_RESTORE      = { MAKEINTRESOURCE(OBM_RESTORE)     }
-- % , OBM_REDUCED      = { MAKEINTRESOURCE(OBM_REDUCED)     }
-- % , OBM_ZOOMD        = { MAKEINTRESOURCE(OBM_ZOOMD)       }
-- % , OBM_RESTORED     = { MAKEINTRESOURCE(OBM_RESTORED)    }
-- % , OBM_UPARROWD     = { MAKEINTRESOURCE(OBM_UPARROWD)    }
-- % , OBM_DNARROWD     = { MAKEINTRESOURCE(OBM_DNARROWD)    }
-- % , OBM_RGARROWD     = { MAKEINTRESOURCE(OBM_RGARROWD)    }
-- % , OBM_LFARROWD     = { MAKEINTRESOURCE(OBM_LFARROWD)    }
-- % , OBM_MNARROW      = { MAKEINTRESOURCE(OBM_MNARROW)     }
-- % , OBM_COMBO        = { MAKEINTRESOURCE(OBM_COMBO)       }
-- % , OBM_UPARROWI     = { MAKEINTRESOURCE(OBM_UPARROWI)    }
-- % , OBM_DNARROWI     = { MAKEINTRESOURCE(OBM_DNARROWI)    }
-- % , OBM_RGARROWI     = { MAKEINTRESOURCE(OBM_RGARROWI)    }
-- % , OBM_LFARROWI     = { MAKEINTRESOURCE(OBM_LFARROWI)    }
-- % , OBM_OLD_CLOSE    = { MAKEINTRESOURCE(OBM_OLD_CLOSE)   }
-- % , OBM_SIZE         = { MAKEINTRESOURCE(OBM_SIZE)        }
-- % , OBM_OLD_UPARROW  = { MAKEINTRESOURCE(OBM_OLD_UPARROW) }
-- % , OBM_OLD_DNARROW  = { MAKEINTRESOURCE(OBM_OLD_DNARROW) }
-- % , OBM_OLD_RGARROW  = { MAKEINTRESOURCE(OBM_OLD_RGARROW) }
-- % , OBM_OLD_LFARROW  = { MAKEINTRESOURCE(OBM_OLD_LFARROW) }
-- % , OBM_BTSIZE       = { MAKEINTRESOURCE(OBM_BTSIZE)      }
-- % , OBM_CHECK        = { MAKEINTRESOURCE(OBM_CHECK)       }
-- % , OBM_CHECKBOXES   = { MAKEINTRESOURCE(OBM_CHECKBOXES)  }
-- % , OBM_BTNCORNERS   = { MAKEINTRESOURCE(OBM_BTNCORNERS)  }
-- % , OBM_OLD_REDUCE   = { MAKEINTRESOURCE(OBM_OLD_REDUCE)  }
-- % , OBM_OLD_ZOOM     = { MAKEINTRESOURCE(OBM_OLD_ZOOM)    }
-- % , OBM_OLD_RESTORE  = { MAKEINTRESOURCE(OBM_OLD_RESTORE) }
-- % ]

----------------------------------------------------------------
-- Raster Ops
----------------------------------------------------------------

#{enum RasterOp3,
 , sRCCOPY      = SRCCOPY
 , sRCPAINT     = SRCPAINT
 , sRCAND       = SRCAND
 , sRCINVERT    = SRCINVERT
 , sRCERASE     = SRCERASE
 , nOTSRCCOPY   = NOTSRCCOPY
 , nOTSRCERASE  = NOTSRCERASE
 , mERGECOPY    = MERGECOPY
 , mERGEPAINT   = MERGEPAINT
 , pATCOPY      = PATCOPY
 , pATPAINT     = PATPAINT
 , pATINVERT    = PATINVERT
 , dSTINVERT    = DSTINVERT
 , bLACKNESS    = BLACKNESS
 , wHITENESS    = WHITENESS
 }

----------------------------------------------------------------
-- BITMAP
----------------------------------------------------------------

type BITMAP =
  ( INT     -- bmType
  , INT     -- bmWidth
  , INT     -- bmHeight
  , INT     -- bmWidthBytes
  , WORD    -- bmPlanes
  , WORD    -- bmBitsPixel
  , LPVOID  -- bmBits
  )

peekBITMAP :: Ptr BITMAP -> IO BITMAP
peekBITMAP p = do
  ty     <- #{peek BITMAP,bmType} p
  width  <- #{peek BITMAP,bmWidth} p
  height <- #{peek BITMAP,bmHeight} p
  wbytes <- #{peek BITMAP,bmWidthBytes} p
  planes <- #{peek BITMAP,bmPlanes} p
  pixel  <- #{peek BITMAP,bmBitsPixel} p
  bits   <- #{peek BITMAP,bmBits} p
  return (ty, width, height, wbytes, planes, pixel, bits)

pokeBITMAP :: Ptr BITMAP -> BITMAP -> IO ()
pokeBITMAP p (ty, width, height, wbytes, planes, pixel, bits) = do
  #{poke BITMAP,bmType}       p ty
  #{poke BITMAP,bmWidth}      p width
  #{poke BITMAP,bmHeight}     p height
  #{poke BITMAP,bmWidthBytes} p wbytes
  #{poke BITMAP,bmPlanes}     p planes
  #{poke BITMAP,bmBitsPixel}  p pixel
  #{poke BITMAP,bmBits}       p bits

type LPBITMAP = Ptr BITMAP

setBITMAP :: LPBITMAP -> BITMAP -> IO ()
setBITMAP = pokeBITMAP

----------------------------------------------------------------
-- Misc
----------------------------------------------------------------

deleteBitmap :: HBITMAP -> IO ()
deleteBitmap bitmap =
  failIfFalse_ "DeleteBitmap" $ c_DeleteBitmap bitmap
foreign import WINDOWS_CCONV unsafe "windows.h DeleteObject"
  c_DeleteBitmap :: HBITMAP -> IO Bool

createBitmap :: INT -> INT -> UINT -> UINT -> Maybe LPVOID -> IO HBITMAP
createBitmap w h planes bits mb_color_data =
  failIfNull "CreateBitmap" $
    c_CreateBitmap w h planes bits (maybePtr mb_color_data)
foreign import WINDOWS_CCONV unsafe "windows.h CreateBitmap"
  c_CreateBitmap :: INT -> INT -> UINT -> UINT -> LPVOID -> IO HBITMAP

createBitmapIndirect :: LPBITMAP -> IO HBITMAP
createBitmapIndirect p_bm =
  failIfNull "CreateBitmapIndirect" $ c_CreateBitmapIndirect p_bm
foreign import WINDOWS_CCONV unsafe "windows.h CreateBitmapIndirect"
  c_CreateBitmapIndirect :: LPBITMAP -> IO HBITMAP

createCompatibleBitmap :: HDC -> Int32 -> Int32 -> IO HBITMAP
createCompatibleBitmap dc w h =
  failIfNull "CreateCompatibleBitmap" $ c_CreateCompatibleBitmap dc w h
foreign import WINDOWS_CCONV unsafe "windows.h CreateCompatibleBitmap"
  c_CreateCompatibleBitmap :: HDC -> Int32 -> Int32 -> IO HBITMAP

createDIBPatternBrushPt :: LPVOID -> ColorFormat -> IO HBRUSH
createDIBPatternBrushPt bm usage =
  failIfNull "CreateDIBPatternBrushPt" $ c_CreateDIBPatternBrushPt bm usage
foreign import WINDOWS_CCONV unsafe "windows.h CreateDIBPatternBrushPt"
  c_CreateDIBPatternBrushPt :: LPVOID -> ColorFormat -> IO HBRUSH

----------------------------------------------------------------
-- Querying
----------------------------------------------------------------

getBitmapDimensionEx :: HBITMAP -> IO SIZE
getBitmapDimensionEx bm =
  allocaSIZE $ \ p_size -> do
  failIfFalse_ "GetBitmapDimensionEx" $ c_GetBitmapDimensionEx bm p_size
  peekSIZE p_size
foreign import WINDOWS_CCONV unsafe "windows.h GetBitmapDimensionEx"
  c_GetBitmapDimensionEx :: HBITMAP -> Ptr SIZE -> IO Bool

setBitmapDimensionEx :: HBITMAP -> SIZE -> IO SIZE
setBitmapDimensionEx bm (cx,cy) =
  allocaSIZE $ \ p_size -> do
  failIfFalse_ "SetBitmapDimensionEx" $ do
    c_SetBitmapDimensionEx bm cx cy p_size
  peekSIZE p_size
foreign import WINDOWS_CCONV unsafe "windows.h SetBitmapDimensionEx"
  c_SetBitmapDimensionEx :: HBITMAP -> LONG -> LONG -> Ptr SIZE -> IO Bool

getBitmapInfo :: HBITMAP -> IO BITMAP
getBitmapInfo bm =
  allocaBytes (fromIntegral sizeofBITMAP) $ \ p_bm -> do
  failIfFalse_ "GetBitmapInfo" $ c_GetBitmapInfo bm sizeofBITMAP p_bm
  peekBITMAP p_bm
foreign import WINDOWS_CCONV unsafe "windows.h GetObjectW"
  c_GetBitmapInfo :: HBITMAP -> DWORD -> LPBITMAP -> IO Bool

----------------------------------------------------------------
--
----------------------------------------------------------------

type BitmapCompression = DWORD

#{enum BitmapCompression,
 , bI_RGB       = BI_RGB
 , bI_RLE8      = BI_RLE8
 , bI_RLE4      = BI_RLE4
 , bI_BITFIELDS = BI_BITFIELDS
 }

type ColorFormat = DWORD

#{enum ColorFormat,
 , dIB_PAL_COLORS = DIB_PAL_COLORS
 , dIB_RGB_COLORS = DIB_RGB_COLORS
 }

----------------------------------------------------------------
-- BITMAPINFO
----------------------------------------------------------------

type LPBITMAPINFO = Ptr ()

----------------------------------------------------------------
-- BITMAPINFOHEADER
----------------------------------------------------------------

type BITMAPINFOHEADER =
 ( DWORD              -- biSize      -- sizeof(BITMAPINFOHEADER)
 , LONG               -- biWidth
 , LONG               -- biHeight
 , WORD               -- biPlanes
 , WORD               -- biBitCount  -- 1, 4, 8, 16, 24 or 32
 , BitmapCompression  -- biCompression
 , DWORD              -- biSizeImage
 , LONG               -- biXPelsPerMeter
 , LONG               -- biYPelsPerMeter
 , Maybe DWORD        -- biClrUsed
 , Maybe DWORD        -- biClrImportant
 )

peekBITMAPINFOHEADER :: Ptr BITMAPINFOHEADER -> IO BITMAPINFOHEADER
peekBITMAPINFOHEADER p = do
  size     <- #{peek BITMAPINFOHEADER,biSize} p
  width    <- #{peek BITMAPINFOHEADER,biWidth} p
  height   <- #{peek BITMAPINFOHEADER,biHeight} p
  planes   <- #{peek BITMAPINFOHEADER,biPlanes} p
  nbits    <- #{peek BITMAPINFOHEADER,biBitCount} p
  comp     <- #{peek BITMAPINFOHEADER,biCompression} p
  imsize   <- #{peek BITMAPINFOHEADER,biSizeImage} p
  xDensity <- #{peek BITMAPINFOHEADER,biXPelsPerMeter} p
  yDensity <- #{peek BITMAPINFOHEADER,biYPelsPerMeter} p
  clrUsed  <- liftM numToMaybe $ #{peek BITMAPINFOHEADER,biClrUsed} p
  clrImp   <- liftM numToMaybe $ #{peek BITMAPINFOHEADER,biClrImportant} p
  return (size, width, height, planes, nbits, comp, imsize,
          xDensity, yDensity, clrUsed, clrImp)

type LPBITMAPINFOHEADER   = Ptr BITMAPINFOHEADER

getBITMAPINFOHEADER_ :: LPBITMAPINFOHEADER -> IO BITMAPINFOHEADER
getBITMAPINFOHEADER_ = peekBITMAPINFOHEADER

----------------------------------------------------------------
-- BITMAPFILEHEADER
----------------------------------------------------------------

type BITMAPFILEHEADER =
 ( WORD   -- bfType      -- "BM" == 0x4d42
 , DWORD  -- bfSize      -- number of bytes in file
 , WORD   -- bfReserved1 -- == 0
 , WORD   -- bfReserved2 -- == 0
 , DWORD  -- bfOffBits   -- == (char*) bits - (char*) filehdr
 )

peekBITMAPFILEHEADER :: Ptr BITMAPFILEHEADER -> IO BITMAPFILEHEADER
peekBITMAPFILEHEADER p = do
  ty     <- #{peek BITMAPFILEHEADER,bfType} p
  size   <- #{peek BITMAPFILEHEADER,bfSize} p
  res1   <- #{peek BITMAPFILEHEADER,bfReserved1} p
  res2   <- #{peek BITMAPFILEHEADER,bfReserved2} p
  offset <- #{peek BITMAPFILEHEADER,bfOffBits} p
  return (ty, size, res1, res2, offset)

type LPBITMAPFILEHEADER = Ptr BITMAPFILEHEADER

getBITMAPFILEHEADER :: LPBITMAPFILEHEADER -> IO BITMAPFILEHEADER
getBITMAPFILEHEADER = peekBITMAPFILEHEADER

sizeofBITMAP             :: Word32
sizeofBITMAP             = #{size BITMAP}
sizeofBITMAPINFO         :: Word32
sizeofBITMAPINFO         = #{size BITMAPINFO}
sizeofBITMAPINFOHEADER   :: Word32
sizeofBITMAPINFOHEADER   = #{size BITMAPINFOHEADER}
sizeofBITMAPFILEHEADER   :: Word32
sizeofBITMAPFILEHEADER   = #{size BITMAPFILEHEADER}
sizeofLPBITMAPFILEHEADER :: Word32
sizeofLPBITMAPFILEHEADER = #{size BITMAPFILEHEADER}

----------------------------------------------------------------
-- CreateBMPFile
----------------------------------------------------------------

-- A (large) helper function - courtesy of Microsoft

createBMPFile :: String -> HBITMAP -> HDC -> IO ()
createBMPFile name bm dc =
  withCWString name $ \ c_name ->
  c_CreateBMPFile c_name bm dc
foreign import ccall unsafe "dumpBMP.h CreateBMPFile"
  c_CreateBMPFile :: LPCTSTR -> HBITMAP -> HDC -> IO ()

{-# CFILES cbits/dumpBMP.c #-}

----------------------------------------------------------------
-- Device Independent Bitmaps
----------------------------------------------------------------

#{enum DWORD,
 , cBM_INIT = CBM_INIT
 }

getDIBits :: HDC -> HBITMAP -> INT -> INT -> Maybe LPVOID -> LPBITMAPINFO -> ColorFormat -> IO INT
getDIBits dc bm start nlines mb_bits info usage =
  failIfZero "GetDIBits" $
    c_GetDIBits dc bm start nlines (maybePtr mb_bits) info usage
foreign import WINDOWS_CCONV unsafe "windows.h GetDIBits"
  c_GetDIBits :: HDC -> HBITMAP -> INT -> INT -> LPVOID -> LPBITMAPINFO -> ColorFormat -> IO INT

setDIBits :: HDC -> HBITMAP -> INT -> INT -> LPVOID -> LPBITMAPINFO -> ColorFormat -> IO INT
setDIBits dc bm start nlines bits info use =
  failIfZero "SetDIBits" $ c_SetDIBits dc bm start nlines bits info use
foreign import WINDOWS_CCONV unsafe "windows.h SetDIBits"
  c_SetDIBits :: HDC -> HBITMAP -> INT -> INT -> LPVOID -> LPBITMAPINFO -> ColorFormat -> IO INT

createDIBitmap :: HDC -> LPBITMAPINFOHEADER -> DWORD -> LPVOID -> LPBITMAPINFO -> ColorFormat -> IO HBITMAP
createDIBitmap dc hdr option init_val info usage =
  failIfNull "CreateDIBitmap" $
    c_CreateDIBitmap dc hdr option init_val info usage
foreign import WINDOWS_CCONV unsafe "windows.h CreateDIBitmap"
  c_CreateDIBitmap :: HDC -> LPBITMAPINFOHEADER -> DWORD -> LPVOID -> LPBITMAPINFO -> ColorFormat -> IO HBITMAP

----------------------------------------------------------------
-- End
----------------------------------------------------------------

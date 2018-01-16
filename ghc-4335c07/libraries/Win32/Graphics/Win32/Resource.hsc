#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.Resource
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

module Graphics.Win32.Resource where

import System.Win32.Types

import Foreign

##include "windows_cconv.h"

#include <windows.h>

beginUpdateResource :: String -> Bool -> IO HANDLE
beginUpdateResource name del =
  withTString name $ \ c_name ->
  failIfNull "BeginUpdateResource" $ c_BeginUpdateResource c_name del
foreign import WINDOWS_CCONV unsafe "windows.h BeginUpdateResourceW"
  c_BeginUpdateResource :: LPCTSTR -> Bool -> IO HANDLE

type ResourceImageType = UINT

type   HRSRC      = Ptr ()

type   HGLOBAL    = Ptr ()

#{enum ResourceImageType,
 , iMAGE_BITMAP = IMAGE_BITMAP
 , iMAGE_ICON   = IMAGE_ICON
 , iMAGE_CURSOR = IMAGE_CURSOR
 }

copyImage :: HANDLE -> ResourceImageType -> Int -> Int -> UINT -> IO HANDLE
copyImage h ty x y flags =
  failIfNull "CopyImage" $ c_CopyImage h ty x y flags
foreign import WINDOWS_CCONV unsafe "windows.h CopyImage"
  c_CopyImage :: HANDLE -> ResourceImageType -> Int -> Int -> UINT -> IO HANDLE

endUpdateResource :: HANDLE -> BOOL -> IO ()
endUpdateResource h discard =
  failIfFalse_ "EndUpdateResource" $ c_EndUpdateResource h discard
foreign import WINDOWS_CCONV unsafe "windows.h EndUpdateResourceW"
  c_EndUpdateResource :: HANDLE -> BOOL -> IO Bool

type ResourceType = LPCTSTR

#{enum ResourceType, castUINTPtrToPtr
 , rT_ACCELERATOR  = (UINT_PTR)RT_ACCELERATOR // Accelerator table
 , rT_ANICURSOR    = (UINT_PTR)RT_ANICURSOR // Animated cursor
 , rT_ANIICON      = (UINT_PTR)RT_ANIICON   // Animated icon
 , rT_BITMAP       = (UINT_PTR)RT_BITMAP    // Bitmap resource
 , rT_CURSOR       = (UINT_PTR)RT_CURSOR    // Hardware-dependent cursor resource
 , rT_DIALOG       = (UINT_PTR)RT_DIALOG    // Dialog box
 , rT_FONT         = (UINT_PTR)RT_FONT      // Font resource
 , rT_FONTDIR      = (UINT_PTR)RT_FONTDIR    // Font directory resource
 , rT_GROUP_CURSOR = (UINT_PTR)RT_GROUP_CURSOR // Hardware-independent cursor resource
 , rT_GROUP_ICON   = (UINT_PTR)RT_GROUP_ICON // Hardware-independent icon resource
 , rT_HTML         = (UINT_PTR)RT_HTML      // HTML document
 , rT_ICON         = (UINT_PTR)RT_ICON      // Hardware-dependent icon resource
 , rT_MENU         = (UINT_PTR)RT_MENU      // Menu resource
 , rT_MESSAGETABLE = (UINT_PTR)RT_MESSAGETABLE // Message-table entry
 , rT_RCDATA       = (UINT_PTR)RT_RCDATA    // Application-defined resource (raw data)
 , rT_STRING       = (UINT_PTR)RT_STRING    // String-table entry
 , rT_VERSION      = (UINT_PTR)RT_VERSION   // Version resource
 }

findResource :: HMODULE -> String -> ResourceType -> IO HRSRC
findResource hmod name ty =
  withTString name $ \ c_name ->
  failIfNull "FindResource" $ c_FindResource hmod c_name ty
foreign import WINDOWS_CCONV unsafe "windows.h FindResourceW"
  c_FindResource :: HMODULE -> LPCTSTR -> LPCTSTR -> IO HRSRC

-- was: LPCTSTR_
findResourceEx :: HMODULE -> String -> ResourceType -> WORD -> IO HRSRC
findResourceEx hmod name ty lang =
  withTString name $ \ c_name ->
  failIfNull "FindResourceEx" $ c_FindResourceEx hmod c_name ty lang
foreign import WINDOWS_CCONV unsafe "windows.h FindResourceExW"
  c_FindResourceEx :: HMODULE -> LPCTSTR -> LPCTSTR -> WORD -> IO HRSRC

type ResourceSize = Int

lR_DEFAULTSIZE :: ResourceSize
lR_DEFAULTSIZE = #{const LR_DEFAULTSIZE}

type LoadImageFlags = UINT

#{enum LoadImageFlags,
 , lR_DEFAULTCOLOR      = LR_DEFAULTCOLOR
 , lR_CREATEDIBSECTION  = LR_CREATEDIBSECTION
 , lR_LOADFROMFILE      = LR_LOADFROMFILE
 , lR_LOADMAP3DCOLORS   = LR_LOADMAP3DCOLORS
 , lR_LOADTRANSPARENT   = LR_LOADTRANSPARENT
 , lR_MONOCHROME        = LR_MONOCHROME
 , lR_SHARED            = LR_SHARED
 }

-- , LR_VGACOLOR (Not in mingw-20001111 headers)

-- was: LPCTSTR_
loadImage :: HINSTANCE -> String -> ResourceImageType -> ResourceSize -> ResourceSize -> LoadImageFlags -> IO HANDLE
loadImage inst name ty x y load =
  withTString name $ \ c_name ->
  failIfNull "LoadImage" $ c_LoadImage inst c_name ty x y load
foreign import WINDOWS_CCONV unsafe "windows.h LoadImageW"
  c_LoadImage :: HINSTANCE -> LPCTSTR -> ResourceImageType -> ResourceSize -> ResourceSize -> LoadImageFlags -> IO HANDLE

loadResource :: HMODULE -> HRSRC -> IO HGLOBAL
loadResource hmod res =
  failIfNull "LoadResource" $ c_LoadResource hmod res
foreign import WINDOWS_CCONV unsafe "windows.h LoadResource"
  c_LoadResource :: HMODULE -> HRSRC -> IO HGLOBAL

lockResource :: HGLOBAL -> IO Addr
lockResource res =
  failIfNull "LockResource" $ c_LockResource res
foreign import WINDOWS_CCONV unsafe "windows.h LockResource"
  c_LockResource :: HGLOBAL -> IO Addr

sizeofResource :: HMODULE -> HRSRC -> IO DWORD
sizeofResource hmod res =
  failIfZero "SizeofResource" $ c_SizeofResource hmod res
foreign import WINDOWS_CCONV unsafe "windows.h SizeofResource"
  c_SizeofResource :: HMODULE -> HRSRC -> IO DWORD

-- was: LPCTSTR_
updateResource :: HANDLE -> ResourceType -> String -> WORD -> Addr -> DWORD -> IO ()
updateResource h ty name lang p_data data_len =
  withTString name $ \ c_name ->
  failIfFalse_ "UpdateResource" $
    c_UpdateResource h ty c_name lang p_data data_len
foreign import WINDOWS_CCONV unsafe "windows.h UpdateResourceW"
  c_UpdateResource :: HANDLE -> LPCTSTR -> LPCTSTR -> WORD -> Addr -> DWORD -> IO Bool

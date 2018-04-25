#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.Dialogue
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

module Graphics.Win32.Dialogue where

import Graphics.Win32.GDI.Types
import Graphics.Win32.Control
import Graphics.Win32.Message
import Graphics.Win32.Window
import System.Win32.Types

import Foreign
import Foreign.C

##include "windows_cconv.h"

#include <windows.h>

type DTemplate = LPCTSTR

type DTemplateMem = Ptr Stub_DTM
newtype Stub_DTM = Stub_DTM DTemplateMem

newtype DIA_TEMPLATE = DIA_TEMPLATE (Ptr DIA_TEMPLATE)

type DialogStyle = WindowStyle

mkDialogTemplate :: String -> IO DTemplate
mkDialogTemplate = newTString

type ResourceID = Int

mkResource :: ResourceID -> IO (Ptr a)
mkResource res = return (castUINTPtrToPtr (fromIntegral res))

mkDialogTemplateFromResource :: Int -> IO DTemplate
mkDialogTemplateFromResource = mkResource

type DialogProc = HWND -> WindowMessage -> WPARAM -> LPARAM -> IO Int

marshall_dialogProc_ :: DialogProc -> IO (FunPtr DialogProc)
marshall_dialogProc_ cl = mkDialogClosure cl

-- ToDo: this was declared as a stdcall not a ccall - let's
-- hope and pray that it makes no difference - ADR
foreign import ccall "wrapper"
  mkDialogClosure :: DialogProc -> IO (FunPtr DialogProc)

dialogBox :: HINSTANCE -> DTemplate -> Maybe HWND -> DialogProc -> IO Int
dialogBox inst template mb_parent dia_fn =
  dialogBoxParam inst template mb_parent dia_fn 0

dialogBoxParam :: HINSTANCE -> DTemplate -> Maybe HWND -> DialogProc -> LPARAM -> IO Int
dialogBoxParam inst template mb_parent dia_fn init_val = do
  c_dia_fn <- mkDialogClosure dia_fn
  failIf (== -1) "DialogBoxParam" $
    c_DialogBoxParam inst template (maybePtr mb_parent) c_dia_fn init_val
foreign import WINDOWS_CCONV "windows.h DialogBoxParamW"
  c_DialogBoxParam :: HINSTANCE -> DTemplate -> HWND -> FunPtr DialogProc -> LPARAM -> IO Int

dialogBoxIndirect :: HINSTANCE -> DTemplateMem -> Maybe HWND -> DialogProc -> IO Int
dialogBoxIndirect inst template mb_parent dia_fn =
  dialogBoxIndirectParam inst template mb_parent dia_fn 0

dialogBoxIndirectParam :: HINSTANCE -> DTemplateMem -> Maybe HWND -> DialogProc -> LPARAM -> IO Int
dialogBoxIndirectParam inst template mb_parent dia_fn init_val = do
  c_dia_fn <- mkDialogClosure dia_fn
  failIf (== -1) "DialogBoxIndirectParam" $
    c_DialogBoxIndirectParam inst template (maybePtr mb_parent) c_dia_fn init_val
foreign import WINDOWS_CCONV "windows.h DialogBoxIndirectParamW"
  c_DialogBoxIndirectParam :: HINSTANCE -> DTemplateMem -> HWND -> FunPtr DialogProc -> LPARAM -> IO Int


data DialogTemplate
 = DialogTemplate
      Int Int Int Int  -- x, y, cx, cy
      WindowStyle
      DWORD
      (Either ResourceID String)  -- menu
      (Either ResourceID String)  -- class
      (Either ResourceID String)  -- caption
      (Either ResourceID String)  -- fontname
      Int                         -- font height
      [DialogControl]

data DialogControl
 = DialogControl
      Int Int Int Int -- x,y, cx, cy
      (Either ResourceID String) -- text
      (Either ResourceID String) -- classname
      WindowStyle
      DWORD
      Int                        -- dia_id

mkDialogFromTemplate :: DialogTemplate -> IO DTemplateMem
mkDialogFromTemplate (DialogTemplate x y cx cy
                                     wstyle extstyle
                                     mb_menu mb_class caption
                                     font font_height
                                     controls) = do
  prim_hmenu    <- marshall_res mb_menu
  prim_class    <- marshall_res mb_class
  prim_caption  <- marshall_res caption
  prim_font     <- marshall_res font
  dtemp <- mkDiaTemplate 0 x y cx cy wstyle extstyle
                         prim_hmenu prim_class
                         prim_caption prim_font
                         font_height
  mapM_ (addControl dtemp) controls
  getFinalDialog dtemp

pushButtonControl :: Int -> Int -> Int -> Int
                  -> DWORD -> DWORD -> Int
                  -> String
                  -> DialogControl
pushButtonControl x y cx cy style estyle dia_id lab =
  DialogControl x y cx cy (Left 0x0080) (Right lab)
                (style + bS_DEFPUSHBUTTON) estyle dia_id

labelControl :: Int -> Int -> Int -> Int
             -> DWORD -> DWORD -> Int
             -> String
             -> DialogControl
labelControl x y cx cy style estyle dia_id lab =
  DialogControl x y cx cy (Left 0x0082) (Right lab)
                (style + sS_LEFT) estyle dia_id

listBoxControl :: Int -> Int -> Int -> Int
               -> DWORD -> DWORD -> Int
               -> String
               -> DialogControl
listBoxControl x y cx cy style estyle dia_id lab =
  DialogControl x y cx cy (Left 0x0083) (Right lab)
                (style) estyle dia_id

comboBoxControl :: Int -> Int -> Int -> Int
               -> DWORD -> DWORD -> Int
               -> String
               -> DialogControl
comboBoxControl x y cx cy style estyle dia_id lab =
  DialogControl x y cx cy (Left 0x0085) (Right lab)
                (style) estyle dia_id

editControl :: Int -> Int -> Int -> Int
               -> DWORD -> DWORD -> Int
               -> String
               -> DialogControl
editControl x y cx cy style estyle dia_id lab =
  DialogControl x y cx cy (Left 0x0081) (Right lab)
                (style + eS_LEFT) estyle dia_id

scrollBarControl :: Int -> Int -> Int -> Int
               -> DWORD -> DWORD -> Int
               -> String
               -> DialogControl
scrollBarControl x y cx cy style estyle dia_id lab =
  DialogControl x y cx cy (Left 0x0084) (Right lab)
                (style) estyle dia_id

foreign import ccall unsafe "diatemp.h getFinalDialog"
  getFinalDialog :: Ptr DIA_TEMPLATE -> IO DTemplateMem

foreign import ccall unsafe "diatemp.h mkDiaTemplate"
  mkDiaTemplate :: Int -> Int -> Int -> Int -> Int -> WindowStyle -> DWORD ->
        LPCWSTR -> LPCWSTR -> LPCWSTR -> LPCWSTR -> Int -> IO (Ptr DIA_TEMPLATE)

addControl :: Ptr DIA_TEMPLATE -> DialogControl -> IO ()
addControl dtemp (DialogControl x y cx cy mb_text mb_class
                                style exstyle
                                dia_id) = do
   prim_text  <- marshall_res mb_text
   prim_class <- marshall_res mb_class
   _ <- addDiaControl dtemp prim_text dia_id prim_class style
                 x y cx cy exstyle
   return ()

foreign import ccall unsafe "diatemp.h addDiaControl"
  addDiaControl :: Ptr DIA_TEMPLATE -> LPCWSTR -> Int -> LPCWSTR -> DWORD ->
        Int -> Int -> Int -> Int -> DWORD -> IO (Ptr DIA_TEMPLATE)

{-# CFILES cbits/diatemp.c #-}

marshall_res :: Either ResourceID String -> IO LPCWSTR
marshall_res (Left r)  = mkResource r
marshall_res (Right s) = newCWString s

-- modeless dialogs

createDialog :: HINSTANCE -> DTemplate -> Maybe HWND -> DialogProc -> IO HWND
createDialog inst template mb_parent dia_fn =
  createDialogParam inst template mb_parent dia_fn 0

createDialogParam :: HINSTANCE -> DTemplate -> Maybe HWND -> DialogProc -> LPARAM -> IO HWND
createDialogParam inst template mb_parent dia_fn init_val = do
  c_dia_fn <- mkDialogClosure dia_fn
  failIfNull "CreateDialogParam" $
    c_CreateDialogParam inst template (maybePtr mb_parent) c_dia_fn init_val
foreign import WINDOWS_CCONV "windows.h CreateDialogParamW"
  c_CreateDialogParam :: HINSTANCE -> DTemplate -> HWND -> FunPtr DialogProc -> LPARAM -> IO HWND

createDialogIndirect :: HINSTANCE -> DTemplateMem -> Maybe HWND -> DialogProc -> IO HWND
createDialogIndirect inst template mb_parent dia_fn =
  createDialogIndirectParam inst template mb_parent dia_fn 0

createDialogIndirectParam :: HINSTANCE -> DTemplateMem -> Maybe HWND -> DialogProc -> LPARAM -> IO HWND
createDialogIndirectParam inst template mb_parent dia_fn init_val = do
  c_dia_fn <- mkDialogClosure dia_fn
  failIfNull "CreateDialogIndirectParam" $
    c_CreateDialogIndirectParam inst template (maybePtr mb_parent) c_dia_fn init_val
foreign import WINDOWS_CCONV "windows.h CreateDialogIndirectParamW"
  c_CreateDialogIndirectParam :: HINSTANCE -> DTemplateMem -> HWND -> FunPtr DialogProc -> LPARAM -> IO HWND

foreign import WINDOWS_CCONV "windows.h DefDlgProcW"
  defDlgProc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT

endDialog :: HWND -> Int -> IO ()
endDialog dlg res =
  failIfFalse_ "EndDialog" $ c_EndDialog dlg res
foreign import WINDOWS_CCONV "windows.h EndDialog"
  c_EndDialog :: HWND -> Int -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h GetDialogBaseUnits"
  getDialogBaseUnits :: IO LONG

getDlgCtrlID :: HWND -> IO Int
getDlgCtrlID ctl =
  failIfZero "GetDlgCtrlID" $ c_GetDlgCtrlID ctl
foreign import WINDOWS_CCONV unsafe "windows.h GetDlgCtrlID"
  c_GetDlgCtrlID :: HWND -> IO Int

getDlgItem :: HWND -> Int -> IO HWND
getDlgItem dlg item =
  failIfNull "GetDlgItem" $ c_GetDlgItem dlg item
foreign import WINDOWS_CCONV unsafe "windows.h GetDlgItem"
  c_GetDlgItem :: HWND -> Int -> IO HWND

getDlgItemInt :: HWND -> Int -> Bool -> IO Int
getDlgItemInt dlg item signed =
  alloca $ \ p_trans -> do
  res <- c_GetDlgItemInt dlg item p_trans signed
  failIfFalse_ "GetDlgItemInt" $ peek p_trans
  return (fromIntegral res)
foreign import WINDOWS_CCONV "windows.h GetDlgItemInt"
  c_GetDlgItemInt :: HWND -> Int -> Ptr Bool -> Bool -> IO UINT

getDlgItemText :: HWND -> Int -> Int -> IO String
getDlgItemText dlg item size =
  allocaArray size $ \ p_buf -> do
  _ <- failIfZero "GetDlgItemInt" $ c_GetDlgItemText dlg item p_buf size
  peekTString p_buf
foreign import WINDOWS_CCONV "windows.h GetDlgItemTextW"
  c_GetDlgItemText :: HWND -> Int -> LPTSTR -> Int -> IO Int

getNextDlgGroupItem :: HWND -> HWND -> BOOL -> IO HWND
getNextDlgGroupItem dlg ctl previous =
  failIfNull "GetNextDlgGroupItem" $ c_GetNextDlgGroupItem dlg ctl previous
foreign import WINDOWS_CCONV unsafe "windows.h GetNextDlgGroupItem"
  c_GetNextDlgGroupItem :: HWND -> HWND -> BOOL -> IO HWND

getNextDlgTabItem :: HWND -> HWND -> BOOL -> IO HWND
getNextDlgTabItem dlg ctl previous =
  failIfNull "GetNextDlgTabItem" $ c_GetNextDlgTabItem dlg ctl previous
foreign import WINDOWS_CCONV unsafe "windows.h GetNextDlgTabItem"
  c_GetNextDlgTabItem :: HWND -> HWND -> BOOL -> IO HWND

foreign import WINDOWS_CCONV "windows.h IsDialogMessageW"
  isDialogMessage :: HWND -> LPMSG -> IO BOOL

mapDialogRect :: HWND -> LPRECT -> IO ()
mapDialogRect dlg p_rect =
  failIfFalse_ "MapDialogRect" $ c_MapDialogRect dlg p_rect
foreign import WINDOWS_CCONV unsafe "windows.h MapDialogRect"
  c_MapDialogRect :: HWND -> LPRECT -> IO Bool

-- No MessageBox* funs in here just yet.

foreign import WINDOWS_CCONV "windows.h SendDlgItemMessageW"
  sendDlgItemMessage :: HWND -> Int -> WindowMessage -> WPARAM -> LPARAM -> IO LONG

setDlgItemInt :: HWND -> Int -> UINT -> BOOL -> IO ()
setDlgItemInt dlg item value signed =
  failIfFalse_ "SetDlgItemInt" $ c_SetDlgItemInt dlg item value signed
foreign import WINDOWS_CCONV "windows.h SetDlgItemInt"
  c_SetDlgItemInt :: HWND -> Int -> UINT -> BOOL -> IO Bool

setDlgItemText :: HWND -> Int -> String -> IO ()
setDlgItemText dlg item str =
  withTString str $ \ c_str ->
  failIfFalse_ "SetDlgItemText" $ c_SetDlgItemText dlg item c_str
foreign import WINDOWS_CCONV "windows.h SetDlgItemTextW"
  c_SetDlgItemText :: HWND -> Int -> LPCTSTR -> IO Bool

#{enum WindowStyle,
 , dS_3DLOOK            = DS_3DLOOK
 , dS_ABSALIGN          = DS_ABSALIGN
 , dS_CENTER            = DS_CENTER
 , dS_CENTERMOUSE       = DS_CENTERMOUSE
 , dS_CONTEXTHELP       = DS_CONTEXTHELP
 , dS_CONTROL           = DS_CONTROL
 , dS_FIXEDSYS          = DS_FIXEDSYS
 , dS_LOCALEDIT         = DS_LOCALEDIT
 , dS_MODALFRAME        = DS_MODALFRAME
 , dS_NOFAILCREATE      = DS_NOFAILCREATE
 , dS_NOIDLEMSG         = DS_NOIDLEMSG
 , dS_SETFONT           = DS_SETFONT
 , dS_SETFOREGROUND     = DS_SETFOREGROUND
 , dS_SYSMODAL          = DS_SYSMODAL
 }

#{enum WindowMessage,
 , dM_GETDEFID          = DM_GETDEFID
 , dM_REPOSITION        = DM_REPOSITION
 , dM_SETDEFID          = DM_SETDEFID
 , wM_CTLCOLORDLG       = WM_CTLCOLORDLG
 , wM_CTLCOLORMSGBOX    = WM_CTLCOLORMSGBOX
 }

----------------------------------------------------------------
-- End
----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.Message
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

module Graphics.Win32.Message where

import System.Win32.Types

##include "windows_cconv.h"

#include <windows.h>

type WindowMessage   = DWORD

#{enum WindowMessage,
 , wM_COMPACTING        = WM_COMPACTING
 , wM_WININICHANGE      = WM_WININICHANGE
 , wM_SYSCOLORCHANGE    = WM_SYSCOLORCHANGE
 , wM_QUERYNEWPALETTE   = WM_QUERYNEWPALETTE
 , wM_PALETTEISCHANGING = WM_PALETTEISCHANGING
 , wM_PALETTECHANGED    = WM_PALETTECHANGED
 , wM_FONTCHANGE        = WM_FONTCHANGE
 , wM_SPOOLERSTATUS     = WM_SPOOLERSTATUS
 , wM_DEVMODECHANGE     = WM_DEVMODECHANGE
 , wM_TIMECHANGE        = WM_TIMECHANGE
 , wM_POWER             = WM_POWER
 , wM_QUERYENDSESSION   = WM_QUERYENDSESSION
 , wM_ENDSESSION        = WM_ENDSESSION
 , wM_QUIT              = WM_QUIT
 , wM_CREATE            = WM_CREATE
 , wM_NCCREATE          = WM_NCCREATE
 , wM_DESTROY           = WM_DESTROY
 , wM_NCDESTROY         = WM_NCDESTROY
 , wM_SHOWWINDOW        = WM_SHOWWINDOW
 , wM_SETREDRAW         = WM_SETREDRAW
 , wM_ENABLE            = WM_ENABLE
 , wM_SETTEXT           = WM_SETTEXT
 , wM_GETTEXT           = WM_GETTEXT
 , wM_GETTEXTLENGTH     = WM_GETTEXTLENGTH
 , wM_WINDOWPOSCHANGING = WM_WINDOWPOSCHANGING
 , wM_WINDOWPOSCHANGED  = WM_WINDOWPOSCHANGED
 , wM_MOVE              = WM_MOVE
 , wM_SIZE              = WM_SIZE
 , wM_QUERYOPEN         = WM_QUERYOPEN
 , wM_CLOSE             = WM_CLOSE
 , wM_GETMINMAXINFO     = WM_GETMINMAXINFO
 , wM_PAINT             = WM_PAINT
 , wM_ERASEBKGND        = WM_ERASEBKGND
 , wM_ICONERASEBKGND    = WM_ICONERASEBKGND
 , wM_NCPAINT           = WM_NCPAINT
 , wM_NCCALCSIZE        = WM_NCCALCSIZE
 , wM_QUERYDRAGICON     = WM_QUERYDRAGICON
 , wM_DROPFILES         = WM_DROPFILES
 , wM_ACTIVATE          = WM_ACTIVATE
 , wM_ACTIVATEAPP       = WM_ACTIVATEAPP
 , wM_NCACTIVATE        = WM_NCACTIVATE
 , wM_SETFOCUS          = WM_SETFOCUS
 , wM_KILLFOCUS         = WM_KILLFOCUS
 , wM_KEYDOWN           = WM_KEYDOWN
 , wM_KEYUP             = WM_KEYUP
 , wM_CHAR              = WM_CHAR
 , wM_DEADCHAR          = WM_DEADCHAR
 , wM_SYSKEYDOWN        = WM_SYSKEYDOWN
 , wM_SYSKEYUP          = WM_SYSKEYUP
 , wM_SYSCHAR           = WM_SYSCHAR
 , wM_SYSDEADCHAR       = WM_SYSDEADCHAR
 , wM_KEYFIRST          = WM_KEYFIRST
 , wM_KEYLAST           = WM_KEYLAST
 , wM_MOUSEMOVE         = WM_MOUSEMOVE
 , wM_LBUTTONDOWN       = WM_LBUTTONDOWN
 , wM_LBUTTONUP         = WM_LBUTTONUP
 , wM_LBUTTONDBLCLK     = WM_LBUTTONDBLCLK
 , wM_RBUTTONDOWN       = WM_RBUTTONDOWN
 , wM_RBUTTONUP         = WM_RBUTTONUP
 , wM_RBUTTONDBLCLK     = WM_RBUTTONDBLCLK
 , wM_MBUTTONDOWN       = WM_MBUTTONDOWN
 , wM_MBUTTONUP         = WM_MBUTTONUP
 , wM_MBUTTONDBLCLK     = WM_MBUTTONDBLCLK
 , wM_MOUSEFIRST        = WM_MOUSEFIRST
 , wM_MOUSELAST         = WM_MOUSELAST
 , wM_NCMOUSEMOVE       = WM_NCMOUSEMOVE
 , wM_NCLBUTTONDOWN     = WM_NCLBUTTONDOWN
 , wM_NCLBUTTONUP       = WM_NCLBUTTONUP
 , wM_NCLBUTTONDBLCLK   = WM_NCLBUTTONDBLCLK
 , wM_NCRBUTTONDOWN     = WM_NCRBUTTONDOWN
 , wM_NCRBUTTONUP       = WM_NCRBUTTONUP
 , wM_NCRBUTTONDBLCLK   = WM_NCRBUTTONDBLCLK
 , wM_NCMBUTTONDOWN     = WM_NCMBUTTONDOWN
 , wM_NCMBUTTONUP       = WM_NCMBUTTONUP
 , wM_NCMBUTTONDBLCLK   = WM_NCMBUTTONDBLCLK
 , wM_MOUSEACTIVATE     = WM_MOUSEACTIVATE
 , wM_CANCELMODE        = WM_CANCELMODE
 , wM_TIMER             = WM_TIMER
 , wM_INITMENU          = WM_INITMENU
 , wM_INITMENUPOPUP     = WM_INITMENUPOPUP
 , wM_MENUSELECT        = WM_MENUSELECT
 , wM_MENUCHAR          = WM_MENUCHAR
 , wM_COMMAND           = WM_COMMAND
 , wM_HSCROLL           = WM_HSCROLL
 , wM_VSCROLL           = WM_VSCROLL
 , wM_CUT               = WM_CUT
 , wM_COPY              = WM_COPY
 , wM_PASTE             = WM_PASTE
 , wM_CLEAR             = WM_CLEAR
 , wM_UNDO              = WM_UNDO
 , wM_RENDERFORMAT      = WM_RENDERFORMAT
 , wM_RENDERALLFORMATS  = WM_RENDERALLFORMATS
 , wM_DESTROYCLIPBOARD  = WM_DESTROYCLIPBOARD
 , wM_DRAWCLIPBOARD     = WM_DRAWCLIPBOARD
 , wM_PAINTCLIPBOARD    = WM_PAINTCLIPBOARD
 , wM_SIZECLIPBOARD     = WM_SIZECLIPBOARD
 , wM_VSCROLLCLIPBOARD  = WM_VSCROLLCLIPBOARD
 , wM_HSCROLLCLIPBOARD  = WM_HSCROLLCLIPBOARD
 , wM_ASKCBFORMATNAME   = WM_ASKCBFORMATNAME
 , wM_CHANGECBCHAIN     = WM_CHANGECBCHAIN
 , wM_SETCURSOR         = WM_SETCURSOR
 , wM_SYSCOMMAND        = WM_SYSCOMMAND
 , wM_MDICREATE         = WM_MDICREATE
 , wM_MDIDESTROY        = WM_MDIDESTROY
 , wM_MDIACTIVATE       = WM_MDIACTIVATE
 , wM_MDIRESTORE        = WM_MDIRESTORE
 , wM_MDINEXT           = WM_MDINEXT
 , wM_MDIMAXIMIZE       = WM_MDIMAXIMIZE
 , wM_MDITILE           = WM_MDITILE
 , wM_MDICASCADE        = WM_MDICASCADE
 , wM_MDIICONARRANGE    = WM_MDIICONARRANGE
 , wM_MDIGETACTIVE      = WM_MDIGETACTIVE
 , wM_MDISETMENU        = WM_MDISETMENU
 , wM_CHILDACTIVATE     = WM_CHILDACTIVATE
 , wM_INITDIALOG        = WM_INITDIALOG
 , wM_NEXTDLGCTL        = WM_NEXTDLGCTL
 , wM_PARENTNOTIFY      = WM_PARENTNOTIFY
 , wM_ENTERIDLE         = WM_ENTERIDLE
 , wM_GETDLGCODE        = WM_GETDLGCODE
 , wM_SETFONT           = WM_SETFONT
 , wM_GETFONT           = WM_GETFONT
 , wM_DRAWITEM          = WM_DRAWITEM
 , wM_MEASUREITEM       = WM_MEASUREITEM
 , wM_DELETEITEM        = WM_DELETEITEM
 , wM_COMPAREITEM       = WM_COMPAREITEM
 , wM_VKEYTOITEM        = WM_VKEYTOITEM
 , wM_CHARTOITEM        = WM_CHARTOITEM
 , wM_QUEUESYNC         = WM_QUEUESYNC
 , wM_USER              = WM_USER
 , wM_APP               = WM_APP
 }

registerWindowMessage :: String -> IO WindowMessage
registerWindowMessage msg =
  withTString msg c_RegisterWindowMessage
foreign import WINDOWS_CCONV unsafe "windows.h RegisterWindowMessageW"
  c_RegisterWindowMessage :: LPCTSTR -> IO WindowMessage

-- These are WM_SIZE specific
#{enum WPARAM,
 , sIZE_RESTORED        = SIZE_RESTORED
 , sIZE_MINIMIZED       = SIZE_MINIMIZED
 , sIZE_MAXIMIZED       = SIZE_MAXIMIZED
 , sIZE_MAXSHOW         = SIZE_MAXSHOW
 , sIZE_MAXHIDE         = SIZE_MAXHIDE
 }

----------------------------------------------------------------
-- Phew!
----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.Control
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- FFI bindings to the various standard Win32 controls.
--
-----------------------------------------------------------------------------

module Graphics.Win32.Control where

import Data.Bits ((.|.))
import Graphics.Win32.GDI.Types (HMENU, HWND)
import Graphics.Win32.Message (WindowMessage)
import Graphics.Win32.Window (ClassName, Pos, WindowStyle, maybePos)
import Graphics.Win32.Window (c_CreateWindowEx)
import System.IO.Unsafe (unsafePerformIO)
import System.Win32.Types (HANDLE, UINT, maybePtr, newTString, withTString)
import System.Win32.Types (failIfFalse_, failIfNull, failIfZero)
import Foreign.Ptr (nullPtr)

##include "windows_cconv.h"

#include <windows.h>
#include <commctrl.h>

-- == Command buttons

type ButtonStyle   = WindowStyle

#{enum ButtonStyle,
 , bS_PUSHBUTTON        = BS_PUSHBUTTON
 , bS_DEFPUSHBUTTON     = BS_DEFPUSHBUTTON
 , bS_CHECKBOX          = BS_CHECKBOX
 , bS_AUTOCHECKBOX      = BS_AUTOCHECKBOX
 , bS_RADIOBUTTON       = BS_RADIOBUTTON
 , bS_3STATE            = BS_3STATE
 , bS_AUTO3STATE        = BS_AUTO3STATE
 , bS_GROUPBOX          = BS_GROUPBOX
 , bS_AUTORADIOBUTTON   = BS_AUTORADIOBUTTON
 , bS_OWNERDRAW         = BS_OWNERDRAW
 , bS_LEFTTEXT          = BS_LEFTTEXT
 , bS_USERBUTTON        = BS_USERBUTTON
 }

createButton
  :: String -> WindowStyle -> ButtonStyle
  -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos
  -> Maybe HWND -> Maybe HMENU -> HANDLE
  -> IO HWND
createButton nm wstyle bstyle mb_x mb_y mb_w mb_h mb_parent mb_menu h =
  withTString nm $ \ c_nm ->
  failIfNull "CreateButton" $
    c_CreateWindowEx 0 buttonStyle c_nm (wstyle .|. bstyle)
      (maybePos mb_x) (maybePos mb_y) (maybePos mb_w) (maybePos mb_h)
      (maybePtr mb_parent) (maybePtr mb_menu) h nullPtr

buttonStyle :: ClassName
buttonStyle = unsafePerformIO (newTString "BUTTON")

type ButtonState = UINT

#{enum ButtonState,
 , bST_CHECKED          = BST_CHECKED
 , bST_INDETERMINATE    = BST_INDETERMINATE
 , bST_UNCHECKED        = BST_UNCHECKED
 }

checkDlgButton :: HWND -> Int -> ButtonState -> IO ()
checkDlgButton dialog button check =
  failIfFalse_ "CheckDlgButton" $ c_CheckDlgButton dialog button check
foreign import WINDOWS_CCONV unsafe "windows.h CheckDlgButton"
  c_CheckDlgButton :: HWND -> Int -> ButtonState -> IO Bool

checkRadioButton :: HWND -> Int -> Int -> Int -> IO ()
checkRadioButton dialog first_button last_button check =
  failIfFalse_ "CheckRadioButton" $
    c_CheckRadioButton dialog first_button last_button check
foreign import WINDOWS_CCONV unsafe "windows.h CheckRadioButton"
  c_CheckRadioButton :: HWND -> Int -> Int -> Int -> IO Bool

isDlgButtonChecked :: HWND -> Int -> IO ButtonState
isDlgButtonChecked wnd button =
  failIfZero "IsDlgButtonChecked" $ c_IsDlgButtonChecked wnd button
foreign import WINDOWS_CCONV unsafe "windows.h IsDlgButtonChecked"
  c_IsDlgButtonChecked :: HWND -> Int -> IO ButtonState


-- == ComboBoxes aka. pop up list boxes/selectors.

type ComboBoxStyle = WindowStyle

#{enum ComboBoxStyle,
 , cBS_SIMPLE           = CBS_SIMPLE
 , cBS_DROPDOWN         = CBS_DROPDOWN
 , cBS_DROPDOWNLIST     = CBS_DROPDOWNLIST
 , cBS_OWNERDRAWFIXED   = CBS_OWNERDRAWFIXED
 , cBS_OWNERDRAWVARIABLE = CBS_OWNERDRAWVARIABLE
 , cBS_AUTOHSCROLL      = CBS_AUTOHSCROLL
 , cBS_OEMCONVERT       = CBS_OEMCONVERT
 , cBS_SORT             = CBS_SORT
 , cBS_HASSTRINGS       = CBS_HASSTRINGS
 , cBS_NOINTEGRALHEIGHT = CBS_NOINTEGRALHEIGHT
 , cBS_DISABLENOSCROLL  = CBS_DISABLENOSCROLL
 }

createComboBox
  :: String -> WindowStyle -> ComboBoxStyle
  -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos
  -> HWND -> Maybe HMENU -> HANDLE
  -> IO HWND
createComboBox nm wstyle cstyle mb_x mb_y mb_w mb_h parent mb_menu h =
  withTString nm $ \ c_nm ->
  failIfNull "CreateComboBox" $
    c_CreateWindowEx 0 comboBoxStyle c_nm (wstyle .|. cstyle)
      (maybePos mb_x) (maybePos mb_y) (maybePos mb_w) (maybePos mb_h)
      parent (maybePtr mb_menu) h nullPtr

comboBoxStyle :: ClassName
comboBoxStyle = unsafePerformIO (newTString "COMBOBOX")

-- see comment about freeing windowNames in System.Win32.Window.createWindow
-- %end free(nm)


--- == Edit controls

----------------------------------------------------------------

type EditStyle = WindowStyle

#{enum EditStyle,
 , eS_LEFT              = ES_LEFT
 , eS_CENTER            = ES_CENTER
 , eS_RIGHT             = ES_RIGHT
 , eS_MULTILINE         = ES_MULTILINE
 , eS_UPPERCASE         = ES_UPPERCASE
 , eS_LOWERCASE         = ES_LOWERCASE
 , eS_PASSWORD          = ES_PASSWORD
 , eS_AUTOVSCROLL       = ES_AUTOVSCROLL
 , eS_AUTOHSCROLL       = ES_AUTOHSCROLL
 , eS_NOHIDESEL         = ES_NOHIDESEL
 , eS_OEMCONVERT        = ES_OEMCONVERT
 , eS_READONLY          = ES_READONLY
 , eS_WANTRETURN        = ES_WANTRETURN
 }

createEditWindow
  :: String -> WindowStyle -> EditStyle
  -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos
  -> HWND -> Maybe HMENU -> HANDLE
  -> IO HWND
createEditWindow nm wstyle estyle mb_x mb_y mb_w mb_h parent mb_menu h =
  withTString nm $ \ c_nm ->
  failIfNull "CreateEditWindow" $
    c_CreateWindowEx 0 editStyle c_nm (wstyle .|. estyle)
      (maybePos mb_x) (maybePos mb_y) (maybePos mb_w) (maybePos mb_h)
      parent (maybePtr mb_menu) h nullPtr

editStyle :: ClassName
editStyle = unsafePerformIO (newTString "EDIT")

-- see comment about freeing windowNames in System.Win32.Window.createWindow
-- %end free(nm)

-- == List boxes


----------------------------------------------------------------

type ListBoxStyle   = WindowStyle

#{enum ListBoxStyle,
 , lBS_NOTIFY           = LBS_NOTIFY
 , lBS_SORT             = LBS_SORT
 , lBS_NOREDRAW         = LBS_NOREDRAW
 , lBS_MULTIPLESEL      = LBS_MULTIPLESEL
 , lBS_OWNERDRAWFIXED   = LBS_OWNERDRAWFIXED
 , lBS_OWNERDRAWVARIABLE = LBS_OWNERDRAWVARIABLE
 , lBS_HASSTRINGS       = LBS_HASSTRINGS
 , lBS_USETABSTOPS      = LBS_USETABSTOPS
 , lBS_NOINTEGRALHEIGHT = LBS_NOINTEGRALHEIGHT
 , lBS_MULTICOLUMN      = LBS_MULTICOLUMN
 , lBS_WANTKEYBOARDINPUT = LBS_WANTKEYBOARDINPUT
 , lBS_DISABLENOSCROLL  = LBS_DISABLENOSCROLL
 , lBS_STANDARD         = LBS_STANDARD
 }

createListBox
  :: String -> WindowStyle -> ListBoxStyle
  -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos
  -> HWND -> Maybe HMENU -> HANDLE
  -> IO HWND
createListBox nm wstyle lstyle mb_x mb_y mb_w mb_h parent mb_menu h =
  withTString nm $ \ c_nm ->
  failIfNull "CreateListBox" $
    c_CreateWindowEx 0 listBoxStyle c_nm (wstyle .|. lstyle)
      (maybePos mb_x) (maybePos mb_y) (maybePos mb_w) (maybePos mb_h)
      parent (maybePtr mb_menu) h nullPtr

listBoxStyle :: ClassName
listBoxStyle = unsafePerformIO (newTString "LISTBOX")

-- see comment about freeing windowNames in System.Win32.Window.createWindow
-- %end free(nm)

-- == Scrollbars


----------------------------------------------------------------

type ScrollbarStyle = WindowStyle

#{enum ScrollbarStyle,
 , sBS_HORZ                     = SBS_HORZ
 , sBS_TOPALIGN                 = SBS_TOPALIGN
 , sBS_BOTTOMALIGN              = SBS_BOTTOMALIGN
 , sBS_VERT                     = SBS_VERT
 , sBS_LEFTALIGN                = SBS_LEFTALIGN
 , sBS_RIGHTALIGN               = SBS_RIGHTALIGN
 , sBS_SIZEBOX                  = SBS_SIZEBOX
 , sBS_SIZEBOXTOPLEFTALIGN      = SBS_SIZEBOXTOPLEFTALIGN
 , sBS_SIZEBOXBOTTOMRIGHTALIGN  = SBS_SIZEBOXBOTTOMRIGHTALIGN
 }

createScrollbar
  :: String -> WindowStyle -> ScrollbarStyle
  -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos
  -> HWND -> Maybe HMENU -> HANDLE
  -> IO HWND
createScrollbar nm wstyle sstyle mb_x mb_y mb_w mb_h parent mb_menu h =
  withTString nm $ \ c_nm ->
  failIfNull "CreateScrollbar" $
    c_CreateWindowEx 0 scrollBarStyle c_nm (wstyle .|. sstyle)
      (maybePos mb_x) (maybePos mb_y) (maybePos mb_w) (maybePos mb_h)
      parent (maybePtr mb_menu) h nullPtr

scrollBarStyle :: ClassName
scrollBarStyle = unsafePerformIO (newTString "SCROLLBAR")

-- see comment about freeing windowNames in System.Win32.Window.createWindow
-- %end free(nm)

-- == Static controls aka. labels


----------------------------------------------------------------

type StaticControlStyle = WindowStyle

#{enum StaticControlStyle,
 , sS_LEFT              = SS_LEFT
 , sS_CENTER            = SS_CENTER
 , sS_RIGHT             = SS_RIGHT
 , sS_ICON              = SS_ICON
 , sS_BLACKRECT         = SS_BLACKRECT
 , sS_GRAYRECT          = SS_GRAYRECT
 , sS_WHITERECT         = SS_WHITERECT
 , sS_BLACKFRAME        = SS_BLACKFRAME
 , sS_GRAYFRAME         = SS_GRAYFRAME
 , sS_WHITEFRAME        = SS_WHITEFRAME
 , sS_SIMPLE            = SS_SIMPLE
 , sS_LEFTNOWORDWRAP    = SS_LEFTNOWORDWRAP
 , sS_NOPREFIX          = SS_NOPREFIX
 }

createStaticWindow
  :: String -> WindowStyle -> StaticControlStyle
  -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos
  -> HWND -> Maybe HMENU -> HANDLE
  -> IO HWND
createStaticWindow nm wstyle sstyle mb_x mb_y mb_w mb_h parent mb_menu h =
  withTString nm $ \ c_nm ->
  failIfNull "CreateStaticWindow" $
    c_CreateWindowEx 0 staticStyle c_nm (wstyle .|. sstyle)
      (maybePos mb_x) (maybePos mb_y) (maybePos mb_w) (maybePos mb_h)
      parent (maybePtr mb_menu) h nullPtr

staticStyle :: ClassName
staticStyle = unsafePerformIO (newTString "STATIC")

-- see comment about freeing windowNames in System.Win32.Window.createWindow
-- %end free(nm)

#if 0
UNTESTED - leave out

type CommonControl   = Ptr ()

#{enum CommonControl,
 , toolTipsControl      = TOOLTIPS_CLASS
 , trackBarControl      = TRACKBAR_CLASS
 , upDownControl        = UPDOWN_CLASS
 , progressBarControl   = PROGRESS_CLASS
 , hotKeyControl        = HOTKEY_CLASS
 , animateControl       = ANIMATE_CLASS
 , statusControl        = STATUSCLASSNAME
 , headerControl        = WC_HEADER
 , listViewControl      = WC_LISTVIEW
 , tabControl           = WC_TABCONTROL
 , treeViewControl      = WC_TREEVIEW
 , monthCalControl      = MONTHCAL_CLASS
 , dateTimePickControl  = DATETIMEPICK_CLASS
 , reBarControl         = REBARCLASSNAME
 }
-- Not supplied in mingw-20001111
-- , comboBoxExControl    = WC_COMBOBOXEX
-- , iPAddressControl     = WC_IPADDRESS
-- , pageScrollerControl  = WC_PAGESCROLLER

createCommonControl
  :: CommonControl -> WindowStyle -> String -> WindowStyle
  -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos
  -> Maybe HWND -> Maybe HMENU -> HANDLE
  -> IO HWND
createCommonControl c estyle nm wstyle mb_x mb_y mb_w mb_h mb_parent mb_menu h =
  withTString nm $ \ c_nm -> do
  failIfNull "CreateCommonControl" $
    c_CreateWindowEx c estyle c_nm wstyle
      (maybePos mb_x) (maybePos mb_y) (maybePos mb_w) (maybePos mb_h)
      (maybePtr mb_parent) (maybePtr mb_menu) h nullPtr

foreign import WINDOWS_CCONV unsafe "windows.h InitCommonControls"
  initCommonControls :: IO ()

#endif

#{enum WindowMessage,
 , pBM_DELTAPOS = PBM_DELTAPOS
 , pBM_SETPOS   = PBM_SETPOS
 , pBM_SETRANGE = PBM_SETRANGE
 , pBM_SETSTEP  = PBM_SETSTEP
 , pBM_STEPIT   = PBM_STEPIT
 }

-- % , PBM_GETRANGE
-- % , PBM_GETPOS
-- % , PBM_SETBARCOLOR
-- % , PBM_SETBKCOLOR
-- % , PBM_SETRANGE32

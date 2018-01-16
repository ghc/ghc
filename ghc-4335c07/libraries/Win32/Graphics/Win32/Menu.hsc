#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.Menu
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

module Graphics.Win32.Menu
{-
       (
         MenuName
       , checkMenuItem
       , checkMenuRadioItem
       , createMenu
       , createPopupMenu
       , deleteMenu
       , destroyMenu
       , drawMenuBar
       , enableMenuItem
       , getMenu
       , getMenuDefaultItem
       , getMenuItemCount
       , getMenuItemID
       , getMenuItemInfo
       , getMenuItemRect
       , getMenuState
       , getSubMenu
       , getSystemMenu
       , hiliteMenuItem
       , insertMenuItem
       , isMenu
       , loadMenu
       , menuItemFromPoint
       , setMenu
       , setMenuDefaultItem
       , setMenuItemBitmaps
       , setMenuItemInfo
       , trackPopupMenu
       , trackPopupMenuEx

       , GMDIFlag
       , MenuItem
       , MenuFlag
       , MenuState
       , TrackMenuFlag
       , SystemMenuCommand

         -- Obsolete:
       , appendMenu
       , insertMenu
       , modifyMenu
       , removeMenu

       ) -} where

import Graphics.Win32.GDI.Types
import System.Win32.Types

import Foreign
import Control.Monad (liftM)

##include "windows_cconv.h"

#include <windows.h>

type MenuName = LPCTSTR

checkMenuItem :: HMENU -> MenuItem -> MenuFlag -> IO Bool
checkMenuItem menu item check = do
  rv <- failIf (== maxBound) "CheckMenuItem" $ c_CheckMenuItem menu item check
  return (rv == mF_CHECKED)
foreign import WINDOWS_CCONV unsafe "windows.h CheckMenuItem"
  c_CheckMenuItem :: HMENU -> UINT -> UINT -> IO DWORD

checkMenuRadioItem :: HMENU -> MenuItem -> MenuItem -> MenuItem -> MenuFlag -> IO ()
checkMenuRadioItem menu first_id last_id check flags =
  failIfFalse_ "CheckMenuRadioItem" $
    c_CheckMenuRadioItem menu first_id last_id check flags
foreign import WINDOWS_CCONV unsafe "windows.h CheckMenuRadioItem"
  c_CheckMenuRadioItem :: HMENU -> UINT -> UINT -> UINT -> UINT -> IO Bool

createMenu :: IO HMENU
createMenu =
  failIfNull "CreateMenu" $ c_CreateMenu
foreign import WINDOWS_CCONV unsafe "windows.h CreateMenu"
  c_CreateMenu :: IO HMENU

createPopupMenu :: IO HMENU
createPopupMenu =
  failIfNull "CreatePopupMenu" $ c_CreatePopupMenu
foreign import WINDOWS_CCONV unsafe "windows.h CreatePopupMenu"
  c_CreatePopupMenu :: IO HMENU

drawMenuBar :: HWND -> IO ()
drawMenuBar wnd =
  failIfFalse_ "DrawMenuBar" $ c_DrawMenuBar wnd
foreign import WINDOWS_CCONV unsafe "windows.h DrawMenuBar"
  c_DrawMenuBar :: HWND -> IO Bool

type MenuState = MenuFlag

enableMenuItem :: HMENU -> MenuItem -> MenuFlag -> IO MenuState
enableMenuItem menu item flag =
  failIf (== 0xffffffff) "EnableMenuItem" $ c_EnableMenuItem menu item flag
foreign import WINDOWS_CCONV unsafe "windows.h EnableMenuItem"
  c_EnableMenuItem :: HMENU -> UINT -> UINT -> IO MenuState

type GMDIFlag = UINT

type MenuFlag = UINT

#{enum GMDIFlag,
 , gMDI_USEDISABLED     = GMDI_USEDISABLED
 , gMDI_GOINTOPOPUPS    = GMDI_GOINTOPOPUPS
 }

#{enum MenuFlag,
 , mF_BYCOMMAND         = MF_BYCOMMAND
 , mF_BYPOSITION        = MF_BYPOSITION
 , mF_CHECKED           = MF_CHECKED
 }

type MenuItem = UINT

#{enum MenuItem,
 , mF_INSERT            = MF_INSERT
 , mF_CHANGE            = MF_CHANGE
 , mF_APPEND            = MF_APPEND
 , mF_DELETE            = MF_DELETE
 , mF_REMOVE            = MF_REMOVE
 , mF_USECHECKBITMAPS   = MF_USECHECKBITMAPS
 , mF_POPUP             = MF_POPUP
 , mF_SYSMENU           = MF_SYSMENU
 , mF_HELP              = MF_HELP
 , mF_MOUSESELECT       = MF_MOUSESELECT
 , mF_END               = MF_END     // Obsolete -- only used by old RES files
 }

#{enum MenuFlag,
 , mFT_STRING           = MFT_STRING
 , mFT_BITMAP           = MFT_BITMAP
 , mFT_MENUBARBREAK     = MFT_MENUBARBREAK
 , mFT_MENUBREAK        = MFT_MENUBREAK
 , mFT_OWNERDRAW        = MFT_OWNERDRAW
 , mFT_RADIOCHECK       = MFT_RADIOCHECK
 , mFT_SEPARATOR        = MFT_SEPARATOR
 , mFT_RIGHTORDER       = MFT_RIGHTORDER
 , mFT_RIGHTJUSTIFY     = MFT_RIGHTJUSTIFY
 }


#{enum MenuState,
 , mFS_GRAYED           = MFS_GRAYED
 , mFS_DISABLED         = MFS_DISABLED        // == MFS_GRAYED
 , mFS_CHECKED          = MFS_CHECKED
 , mFS_HILITE           = MFS_HILITE
 , mFS_ENABLED          = MFS_ENABLED
 , mFS_UNCHECKED        = MFS_UNCHECKED
 , mFS_UNHILITE         = MFS_UNHILITE
 , mFS_DEFAULT          = MFS_DEFAULT
 }

type TrackMenuFlag = UINT

#{enum TrackMenuFlag,
 , tPM_LEFTBUTTON       = TPM_LEFTBUTTON
 , tPM_RIGHTBUTTON      = TPM_RIGHTBUTTON
 , tPM_LEFTALIGN        = TPM_LEFTALIGN
 , tPM_CENTERALIGN      = TPM_CENTERALIGN
 , tPM_RIGHTALIGN       = TPM_RIGHTALIGN
 , tPM_TOPALIGN         = TPM_TOPALIGN
 , tPM_VCENTERALIGN     = TPM_VCENTERALIGN
 , tPM_BOTTOMALIGN      = TPM_BOTTOMALIGN
 , tPM_HORIZONTAL       = TPM_HORIZONTAL     // Horz alignment matters more
 , tPM_VERTICAL         = TPM_VERTICAL       // Vert alignment matters more
 , tPM_NONOTIFY         = TPM_NONOTIFY       // Don't send any notification msgs
 , tPM_RETURNCMD        = TPM_RETURNCMD
 }

type SystemMenuCommand = UINT

#{enum SystemMenuCommand,
 , sC_SIZE              = SC_SIZE
 , sC_MOVE              = SC_MOVE
 , sC_MINIMIZE          = SC_MINIMIZE
 , sC_MAXIMIZE          = SC_MAXIMIZE
 , sC_NEXTWINDOW        = SC_NEXTWINDOW
 , sC_PREVWINDOW        = SC_PREVWINDOW
 , sC_CLOSE             = SC_CLOSE
 , sC_VSCROLL           = SC_VSCROLL
 , sC_HSCROLL           = SC_HSCROLL
 , sC_MOUSEMENU         = SC_MOUSEMENU
 , sC_KEYMENU           = SC_KEYMENU
 , sC_ARRANGE           = SC_ARRANGE
 , sC_RESTORE           = SC_RESTORE
 , sC_TASKLIST          = SC_TASKLIST
 , sC_SCREENSAVE        = SC_SCREENSAVE
 , sC_HOTKEY            = SC_HOTKEY
 , sC_DEFAULT           = SC_DEFAULT
 , sC_MONITORPOWER      = SC_MONITORPOWER
 , sC_CONTEXTHELP       = SC_CONTEXTHELP
 , sC_SEPARATOR         = SC_SEPARATOR
 }

foreign import WINDOWS_CCONV unsafe "windows.h IsMenu" isMenu :: HMENU -> IO Bool

getSystemMenu :: HWND  -> Bool ->     IO (Maybe HMENU)
getSystemMenu wnd revert =
  liftM ptrToMaybe $ c_GetSystemMenu wnd revert
foreign import WINDOWS_CCONV unsafe "windows.h GetSystemMenu"
  c_GetSystemMenu :: HWND  -> Bool ->     IO HMENU

getMenu :: HWND  ->             IO (Maybe HMENU)
getMenu wnd =
  liftM ptrToMaybe $ c_GetMenu wnd
foreign import WINDOWS_CCONV unsafe "windows.h GetMenu"
  c_GetMenu :: HWND  ->             IO HMENU

getMenuDefaultItem :: HMENU -> Bool -> GMDIFlag -> IO MenuItem
getMenuDefaultItem menu bypos flags =
  failIf (== maxBound) "GetMenuDefaultItem" $ c_GetMenuDefaultItem menu bypos flags
foreign import WINDOWS_CCONV unsafe "windows.h GetMenuDefaultItem"
  c_GetMenuDefaultItem :: HMENU -> Bool -> UINT -> IO UINT

getMenuState :: HMENU -> MenuItem -> MenuFlag -> IO MenuState
getMenuState menu item flags =
  failIf (== maxBound) "GetMenuState" $ c_GetMenuState menu item flags
foreign import WINDOWS_CCONV unsafe "windows.h GetMenuState"
  c_GetMenuState :: HMENU -> UINT -> UINT -> IO MenuState

getSubMenu :: HMENU -> MenuItem -> IO (Maybe HMENU)
getSubMenu menu pos =
  liftM ptrToMaybe $ c_GetSubMenu menu pos
foreign import WINDOWS_CCONV unsafe "windows.h GetSubMenu"
  c_GetSubMenu :: HMENU -> UINT -> IO HMENU

setMenu :: HWND -> HMENU -> IO ()
setMenu wnd menu =
  failIfFalse_ "SetMenu" $ c_SetMenu wnd menu
foreign import WINDOWS_CCONV unsafe "windows.h SetMenu"
  c_SetMenu :: HWND -> HMENU -> IO Bool

getMenuItemCount :: HMENU -> IO Int
getMenuItemCount menu =
  failIf (== maxBound) "GetMenuItemCount" $ c_GetMenuItemCount menu
foreign import WINDOWS_CCONV unsafe "windows.h GetMenuItemCount"
  c_GetMenuItemCount :: HMENU -> IO Int

type MenuID = UINT

getMenuItemID :: HMENU -> MenuItem -> IO MenuID
getMenuItemID menu item =
  failIf (== maxBound) "GetMenuItemID" $ c_GetMenuItemID menu item
foreign import WINDOWS_CCONV unsafe "windows.h GetMenuItemID"
  c_GetMenuItemID :: HMENU -> UINT -> IO MenuID

data MenuItemInfo
 = MenuItemInfo  {
      menuItemType    :: MenuFlag,
      menuItemState   :: MenuState,
      menuItemID      :: UINT,
      menuItemSubMenu :: HMENU,
      menuItemBitmapChecked :: HBITMAP,
      menuItemBitmapUnchecked :: HBITMAP,
      menuItemData    :: DWORD,
      menuItemTypeData :: String
   }

-- Don't make this an instance of Storable, because poke isn't what we want.

peekMenuItemInfo :: Ptr MenuItemInfo -> IO MenuItemInfo
peekMenuItemInfo p = do
  itemType <- #{peek MENUITEMINFO,fType} p
  itemState <- #{peek MENUITEMINFO,fState} p
  itemID <- #{peek MENUITEMINFO,wID} p
  itemSubMenu <- #{peek MENUITEMINFO,hSubMenu} p
  itemBitmapChecked <- #{peek MENUITEMINFO,hbmpChecked} p
  itemBitmapUnchecked <- #{peek MENUITEMINFO,hbmpUnchecked} p
  itemData <- #{peek MENUITEMINFO,dwItemData} p
  nchars <- #{peek MENUITEMINFO,cch} p
  c_str <- #{peek MENUITEMINFO,dwTypeData} p
  itemTypeData <- peekTStringLen (c_str, fromIntegral (nchars::UINT))
  return MenuItemInfo
    { menuItemType = itemType
    , menuItemState = itemState
    , menuItemID = itemID
    , menuItemSubMenu = itemSubMenu
    , menuItemBitmapChecked = itemBitmapChecked
    , menuItemBitmapUnchecked = itemBitmapUnchecked
    , menuItemData = itemData
    , menuItemTypeData = itemTypeData
    }

allocaMenuItemInfo :: (Ptr MenuItemInfo -> IO a) -> IO a
allocaMenuItemInfo f =
  let size = #{size MENUITEMINFO} in
  allocaBytes size $ \ p -> do
  #{poke MENUITEMINFO,cbSize} p (fromIntegral size::DWORD)
  f p

withMenuItemInfo :: MenuItemInfo -> (Ptr MenuItemInfo -> IO a) -> IO a
withMenuItemInfo info f =
  allocaMenuItemInfo $ \ p ->
  withTStringLen (menuItemTypeData info) $ \ (c_str, nchars) -> do
  #{poke MENUITEMINFO,fType} p (menuItemType info)
  #{poke MENUITEMINFO,fState} p (menuItemState info)
  #{poke MENUITEMINFO,wID} p (menuItemID info)
  #{poke MENUITEMINFO,hSubMenu} p (menuItemSubMenu info)
  #{poke MENUITEMINFO,hbmpChecked} p (menuItemBitmapChecked info)
  #{poke MENUITEMINFO,hbmpUnchecked} p (menuItemBitmapUnchecked info)
  #{poke MENUITEMINFO,dwItemData} p c_str
  #{poke MENUITEMINFO,cch} p (fromIntegral nchars::UINT)
  f p

type MenuItemMask = UINT

#{enum MenuItemMask,
 , mIIM_CHECKMARKS      = MIIM_CHECKMARKS
 , mIIM_DATA            = MIIM_DATA
 , mIIM_ID              = MIIM_ID
 , mIIM_STATE           = MIIM_STATE
 , mIIM_SUBMENU         = MIIM_SUBMENU
 , mIIM_TYPE            = MIIM_TYPE
 }

pokeFMask :: Ptr MenuItemInfo -> MenuItemMask -> IO ()
pokeFMask p_info mask =
  #{poke MENUITEMINFO,fMask} p_info mask

getMenuItemInfo :: HMENU -> MenuItem -> Bool -> MenuItemMask -> IO MenuItemInfo
getMenuItemInfo menu item bypos mask =
  allocaMenuItemInfo $ \ p_info -> do
  pokeFMask p_info mask
  failIfFalse_ "GetMenuItemInfo" $ c_GetMenuItemInfo menu item bypos p_info
  peekMenuItemInfo p_info
foreign import WINDOWS_CCONV unsafe "windows.h GetMenuItemInfoW"
  c_GetMenuItemInfo :: HMENU -> UINT -> Bool -> Ptr MenuItemInfo -> IO Bool

getMenuItemRect :: HWND -> HMENU -> MenuItem -> IO RECT
getMenuItemRect wnd menu item =
  allocaRECT $ \ p_rect -> do
  failIfFalse_ "GetMenuItemRect" $ c_GetMenuItemRect wnd menu item p_rect
  peekRECT p_rect
foreign import WINDOWS_CCONV unsafe "windows.h GetMenuItemRect"
  c_GetMenuItemRect :: HWND -> HMENU -> UINT -> LPRECT -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h HiliteMenuItem"
  hiliteMenuItem :: HWND  -> HMENU -> MenuItem -> MenuFlag -> IO Bool

insertMenuItem :: HMENU -> MenuItem -> Bool -> MenuItemInfo -> IO ()
insertMenuItem menu item bypos info =
  withMenuItemInfo info $ \ p_info ->
  failIfFalse_ "InsertMenuItem" $ c_InsertMenuItem menu item bypos p_info
foreign import WINDOWS_CCONV unsafe "windows.h InsertMenuItemW"
  c_InsertMenuItem :: HMENU -> UINT -> Bool -> Ptr MenuItemInfo -> IO Bool

type Menu = LPCTSTR
-- intToMenu :: Int -> Menu
-- intToMenu i = makeIntResource (toWord i)

loadMenu :: Maybe HINSTANCE -> Menu -> IO HMENU
loadMenu mb_inst menu =
  failIfNull "LoadMenu" $ c_LoadMenu (maybePtr mb_inst) menu
foreign import WINDOWS_CCONV unsafe "windows.h LoadMenuW"
  c_LoadMenu :: HINSTANCE -> Menu -> IO HMENU

-- Dealing with mappings to/from structs is a pain in GC,
-- so we'll leave this one out for now.
-- %fun LoadMenuIndirect :: MenuTemplate -> IO HMENU

-- Can't pass structs with current FFI, so use a C wrapper (from Types)
menuItemFromPoint :: HWND -> HMENU -> POINT -> IO UINT
menuItemFromPoint wnd menu pt =
  withPOINT pt $ \ p_pt ->
  prim_MenuItemFromPoint wnd menu p_pt

setMenuDefaultItem :: HMENU -> MenuItem -> Bool -> IO ()
setMenuDefaultItem menu item bypos =
  failIfFalse_ "SetMenuDefaultItem" $ c_SetMenuDefaultItem menu item bypos
foreign import WINDOWS_CCONV unsafe "windows.h SetMenuDefaultItem"
  c_SetMenuDefaultItem :: HMENU -> MenuItem -> Bool -> IO Bool

setMenuItemBitmaps :: HMENU -> MenuItem -> MenuFlag -> HBITMAP -> HBITMAP -> IO ()
setMenuItemBitmaps menu pos flags bm_unchecked bm_checked =
  failIfFalse_ "SetMenuItemBitmaps" $
    c_SetMenuItemBitmaps menu pos flags bm_unchecked bm_checked
foreign import WINDOWS_CCONV unsafe "windows.h SetMenuItemBitmaps"
  c_SetMenuItemBitmaps :: HMENU -> UINT -> UINT -> HBITMAP -> HBITMAP -> IO Bool

destroyMenu :: HMENU -> IO ()
destroyMenu menu =
  failIfFalse_ "DestroyMenu" $ c_DestroyMenu menu
foreign import WINDOWS_CCONV unsafe "windows.h DestroyMenu"
  c_DestroyMenu :: HMENU -> IO Bool

deleteMenu :: HMENU -> MenuItem -> MenuFlag -> IO ()
deleteMenu menu item flag =
  failIfFalse_ "DeleteMenu" $ c_DeleteMenu menu item flag
foreign import WINDOWS_CCONV unsafe "windows.h DeleteMenu"
  c_DeleteMenu :: HMENU -> UINT -> UINT -> IO Bool

setMenuItemInfo :: HMENU -> MenuItem -> Bool -> MenuItemMask -> MenuItemInfo -> IO ()
setMenuItemInfo menu item bypos mask info =
  withMenuItemInfo info $ \ p_info -> do
  pokeFMask p_info mask
  failIfFalse_ "SetMenuItemInfo" $ c_SetMenuItemInfo menu item bypos p_info
foreign import WINDOWS_CCONV unsafe "windows.h SetMenuItemInfoW"
  c_SetMenuItemInfo :: HMENU -> UINT -> Bool -> Ptr MenuItemInfo -> IO Bool

trackPopupMenu :: HMENU -> TrackMenuFlag -> Int -> Int -> HWND -> RECT -> IO ()
trackPopupMenu menu flags x y wnd rect =
  withRECT rect $ \ p_rect ->
  failIfFalse_ "TrackPopupMenu" $ c_TrackPopupMenu menu flags x y 0 wnd p_rect
foreign import WINDOWS_CCONV unsafe "windows.h TrackPopupMenu"
  c_TrackPopupMenu :: HMENU -> TrackMenuFlag -> Int -> Int -> Int -> HWND -> LPRECT -> IO Bool

type TPMPARAMS = ()

withTPMPARAMS :: Ptr RECT -> (Ptr TPMPARAMS -> IO a) -> IO a
withTPMPARAMS p_rect f =
  let size = #{size TPMPARAMS} in
  allocaBytes size $ \ p -> do
  #{poke TPMPARAMS,cbSize} p (fromIntegral size::UINT)
  copyBytes (#{ptr TPMPARAMS,rcExclude} p) p_rect size
  f p

trackPopupMenuEx :: HMENU -> TrackMenuFlag -> Int -> Int -> HWND -> Maybe (Ptr RECT) -> IO ()
trackPopupMenuEx menu flags x y wnd mb_p_rect =
  maybeWith withTPMPARAMS mb_p_rect $ \ p_ptmp ->
  failIfFalse_ "TrackPopupMenuEx" $ c_TrackPopupMenuEx menu flags x y wnd p_ptmp
foreign import WINDOWS_CCONV unsafe "windows.h TrackPopupMenuEx"
  c_TrackPopupMenuEx :: HMENU -> TrackMenuFlag -> Int -> Int -> HWND -> Ptr TPMPARAMS -> IO Bool

-- Note: these 3 assume the flags don't include MF_BITMAP or MF_OWNERDRAW
-- (which are hidden by this interface)

appendMenu :: HMENU -> MenuFlag -> MenuID -> Maybe String -> IO ()
appendMenu menu flags id_item name =
  maybeWith withTString name $ \ c_name ->
  failIfFalse_ "AppendMenu" $ c_AppendMenu menu flags id_item c_name
foreign import WINDOWS_CCONV unsafe "windows.h AppendMenuW"
  c_AppendMenu :: HMENU -> UINT -> MenuID -> LPCTSTR -> IO Bool

insertMenu :: HMENU -> MenuItem -> MenuFlag -> MenuID -> Maybe String -> IO ()
insertMenu menu item flags id_item name =
  maybeWith withTString name $ \ c_name ->
  failIfFalse_ "InsertMenu" $ c_InsertMenu menu item flags id_item c_name
foreign import WINDOWS_CCONV unsafe "windows.h InsertMenuW"
  c_InsertMenu :: HMENU -> UINT -> UINT -> MenuID -> LPCTSTR -> IO Bool

modifyMenu :: HMENU -> MenuItem -> MenuFlag -> MenuID -> Maybe String -> IO ()
modifyMenu menu item flags id_item name =
  maybeWith withTString name $ \ c_name ->
  failIfFalse_ "ModifyMenu" $ c_ModifyMenu menu item flags id_item c_name
foreign import WINDOWS_CCONV unsafe "windows.h ModifyMenuW"
  c_ModifyMenu :: HMENU -> UINT -> UINT -> MenuID -> LPCTSTR -> IO Bool

removeMenu :: HMENU -> MenuItem -> MenuFlag -> IO ()
removeMenu menu pos flags =
  failIfFalse_ "RemoveMenu" $ c_RemoveMenu menu pos flags
foreign import WINDOWS_CCONV unsafe "windows.h RemoveMenu"
  c_RemoveMenu :: HMENU -> UINT -> UINT -> IO Bool

----------------------------------------------------------------
-- End
----------------------------------------------------------------

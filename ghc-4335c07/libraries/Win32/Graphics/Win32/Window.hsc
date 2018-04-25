{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NegativeLiterals #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.Window
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

module Graphics.Win32.Window where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (maybeWith)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (FunPtr, Ptr, castFunPtrToPtr, castPtr, nullPtr)
import Foreign.Storable (pokeByteOff)
import Foreign.C.Types (CIntPtr(..))
import Graphics.Win32.GDI.Types (HBITMAP, HCURSOR, HDC, HDWP, HRGN, HWND, PRGN)
import Graphics.Win32.GDI.Types (HBRUSH, HICON, HMENU, prim_ChildWindowFromPoint)
import Graphics.Win32.GDI.Types (LPRECT, RECT, allocaRECT, peekRECT, withRECT)
import Graphics.Win32.GDI.Types (POINT, allocaPOINT, peekPOINT, withPOINT)
import Graphics.Win32.GDI.Types (prim_ChildWindowFromPointEx)
import Graphics.Win32.Message (WindowMessage)
import System.IO.Unsafe (unsafePerformIO)
import System.Win32.Types (ATOM, maybePtr, newTString, ptrToMaybe, numToMaybe)
import System.Win32.Types (Addr, BOOL, DWORD, INT, LONG, LRESULT, UINT, WPARAM)
import System.Win32.Types (HINSTANCE, LPARAM, LPCTSTR, LPVOID, withTString)
import System.Win32.Types (failIf, failIf_, failIfFalse_, failIfNull, maybeNum)

##include "windows_cconv.h"

#include <windows.h>

----------------------------------------------------------------
-- Window Class
----------------------------------------------------------------

-- The classname must not be deallocated until the corresponding class
-- is deallocated.  For this reason, we represent classnames by pointers
-- and explicitly allocate the className.

type ClassName   = LPCTSTR

-- Note: this is one of those rare functions which doesnt free all
-- its String arguments.

mkClassName :: String -> ClassName
mkClassName name = unsafePerformIO (newTString name)

type ClassStyle   = UINT

#{enum ClassStyle,
 , cS_VREDRAW           = CS_VREDRAW
 , cS_HREDRAW           = CS_HREDRAW
 , cS_OWNDC             = CS_OWNDC
 , cS_CLASSDC           = CS_CLASSDC
 , cS_PARENTDC          = CS_PARENTDC
 , cS_SAVEBITS          = CS_SAVEBITS
 , cS_DBLCLKS           = CS_DBLCLKS
 , cS_BYTEALIGNCLIENT   = CS_BYTEALIGNCLIENT
 , cS_BYTEALIGNWINDOW   = CS_BYTEALIGNWINDOW
 , cS_NOCLOSE           = CS_NOCLOSE
 , cS_GLOBALCLASS       = CS_GLOBALCLASS
 }

type WNDCLASS =
 (ClassStyle,    -- style
  HINSTANCE,     -- hInstance
  Maybe HICON,   -- hIcon
  Maybe HCURSOR, -- hCursor
  Maybe HBRUSH,  -- hbrBackground
  Maybe LPCTSTR, -- lpszMenuName
  ClassName)     -- lpszClassName

--ToDo!
--To avoid confusion with NULL, WNDCLASS requires you to add 1 to a SystemColor
--(which can be NULL)
-- %fun mkMbHBRUSH :: SystemColor -> MbHBRUSH
-- %code
-- %result ((HBRUSH)($0+1));

withWNDCLASS :: WNDCLASS -> (Ptr WNDCLASS -> IO a) -> IO a
withWNDCLASS (style, inst, mb_icon, mb_cursor, mb_bg, mb_menu, cls) f =
  allocaBytes #{size WNDCLASS} $ \ p -> do
  #{poke WNDCLASS,style} p style
  #{poke WNDCLASS,lpfnWndProc} p genericWndProc_p
  #{poke WNDCLASS,cbClsExtra} p (0::INT)
  #{poke WNDCLASS,cbWndExtra} p (0::INT)
  #{poke WNDCLASS,hInstance} p inst
  #{poke WNDCLASS,hIcon} p (maybePtr mb_icon)
  #{poke WNDCLASS,hCursor} p (maybePtr mb_cursor)
  #{poke WNDCLASS,hbrBackground} p (maybePtr mb_bg)
  #{poke WNDCLASS,lpszMenuName} p (maybePtr mb_menu)
  #{poke WNDCLASS,lpszClassName} p cls
  f p

foreign import WINDOWS_CCONV unsafe "WndProc.h &genericWndProc"
  genericWndProc_p :: FunPtr WindowClosure

{-# CFILES cbits/WndProc.c #-}

registerClass :: WNDCLASS -> IO (Maybe ATOM)
registerClass cls =
  withWNDCLASS cls $ \ p ->
  liftM numToMaybe $ c_RegisterClass p
foreign import WINDOWS_CCONV unsafe "windows.h RegisterClassW"
  c_RegisterClass :: Ptr WNDCLASS -> IO ATOM

foreign import WINDOWS_CCONV unsafe "windows.h UnregisterClassW"
  unregisterClass :: ClassName -> HINSTANCE -> IO ()

----------------------------------------------------------------
-- Window Style
----------------------------------------------------------------

type WindowStyle   = DWORD

#{enum WindowStyle,
 , wS_OVERLAPPED        = WS_OVERLAPPED
 , wS_POPUP             = WS_POPUP
 , wS_CHILD             = WS_CHILD
 , wS_CLIPSIBLINGS      = WS_CLIPSIBLINGS
 , wS_CLIPCHILDREN      = WS_CLIPCHILDREN
 , wS_VISIBLE           = WS_VISIBLE
 , wS_DISABLED          = WS_DISABLED
 , wS_MINIMIZE          = WS_MINIMIZE
 , wS_MAXIMIZE          = WS_MAXIMIZE
 , wS_CAPTION           = WS_CAPTION
 , wS_BORDER            = WS_BORDER
 , wS_DLGFRAME          = WS_DLGFRAME
 , wS_VSCROLL           = WS_VSCROLL
 , wS_HSCROLL           = WS_HSCROLL
 , wS_SYSMENU           = WS_SYSMENU
 , wS_THICKFRAME        = WS_THICKFRAME
 , wS_MINIMIZEBOX       = WS_MINIMIZEBOX
 , wS_MAXIMIZEBOX       = WS_MAXIMIZEBOX
 , wS_GROUP             = WS_GROUP
 , wS_TABSTOP           = WS_TABSTOP
 , wS_OVERLAPPEDWINDOW  = WS_OVERLAPPEDWINDOW
 , wS_POPUPWINDOW       = WS_POPUPWINDOW
 , wS_CHILDWINDOW       = WS_CHILDWINDOW
 , wS_TILED             = WS_TILED
 , wS_ICONIC            = WS_ICONIC
 , wS_SIZEBOX           = WS_SIZEBOX
 , wS_TILEDWINDOW       = WS_TILEDWINDOW
 }

type WindowStyleEx   = DWORD

#{enum WindowStyleEx,
 , wS_EX_DLGMODALFRAME  = WS_EX_DLGMODALFRAME
 , wS_EX_NOPARENTNOTIFY = WS_EX_NOPARENTNOTIFY
 , wS_EX_TOPMOST        = WS_EX_TOPMOST
 , wS_EX_ACCEPTFILES    = WS_EX_ACCEPTFILES
 , wS_EX_TRANSPARENT    = WS_EX_TRANSPARENT
 , wS_EX_MDICHILD       = WS_EX_MDICHILD
 , wS_EX_TOOLWINDOW     = WS_EX_TOOLWINDOW
 , wS_EX_WINDOWEDGE     = WS_EX_WINDOWEDGE
 , wS_EX_CLIENTEDGE     = WS_EX_CLIENTEDGE
 , wS_EX_CONTEXTHELP    = WS_EX_CONTEXTHELP
 , wS_EX_RIGHT          = WS_EX_RIGHT
 , wS_EX_LEFT           = WS_EX_LEFT
 , wS_EX_RTLREADING     = WS_EX_RTLREADING
 , wS_EX_LTRREADING     = WS_EX_LTRREADING
 , wS_EX_LEFTSCROLLBAR  = WS_EX_LEFTSCROLLBAR
 , wS_EX_RIGHTSCROLLBAR = WS_EX_RIGHTSCROLLBAR
 , wS_EX_CONTROLPARENT  = WS_EX_CONTROLPARENT
 , wS_EX_STATICEDGE     = WS_EX_STATICEDGE
 , wS_EX_APPWINDOW      = WS_EX_APPWINDOW
 , wS_EX_OVERLAPPEDWINDOW = WS_EX_OVERLAPPEDWINDOW
 , wS_EX_PALETTEWINDOW  = WS_EX_PALETTEWINDOW
 }


cW_USEDEFAULT :: Pos
-- See Note [Overflow checking and fromIntegral] in Graphics/Win32/GDI/HDC.hs
-- Weird way to essentially get a value with the top bit set. But GHC 7.8.4 was
-- rejecting all other sane attempts.
cW_USEDEFAULT = let val = negate (#{const CW_USEDEFAULT}) :: Integer
                in fromIntegral (fromIntegral val :: Int32) :: Pos

type Pos = Int

type MbPos = Maybe Pos

maybePos :: Maybe Pos -> Pos
maybePos = fromMaybe cW_USEDEFAULT

type WindowClosure = HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT

foreign import WINDOWS_CCONV "wrapper"
  mkWindowClosure :: WindowClosure -> IO (FunPtr WindowClosure)

setWindowClosure :: HWND -> WindowClosure -> IO ()
setWindowClosure wnd closure = do
  fp <- mkWindowClosure closure
  _ <- c_SetWindowLongPtr wnd (#{const GWLP_USERDATA})
                              (castPtr (castFunPtrToPtr fp))
  return ()

{- Note [SetWindowLongPtrW]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Windows.h defines SetWindowLongPtrW as SetWindowLongW for 32-bit platforms
(i386_HOST_ARCH for our mingw32 environment). Unfortunately since the foreign
import name is given inside of a string, the macro will not be expanded.

Until a better solution is presented each version is provided explicitly here.

-}
#if defined(i386_HOST_ARCH)
foreign import WINDOWS_CCONV unsafe "windows.h SetWindowLongW"
#elif defined(x86_64_HOST_ARCH)
foreign import WINDOWS_CCONV unsafe "windows.h SetWindowLongPtrW"
#else
# error Unknown mingw32 arch
#endif
  c_SetWindowLongPtr :: HWND -> INT -> Ptr LONG -> IO (Ptr LONG)

createWindow
  :: ClassName -> String -> WindowStyle ->
     Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos ->
     Maybe HWND -> Maybe HMENU -> HINSTANCE -> WindowClosure ->
     IO HWND
createWindow = createWindowEx 0
-- apparently CreateWindowA/W are just macros for CreateWindowExA/W

createWindowEx
  :: WindowStyle -> ClassName -> String -> WindowStyle
  -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos
  -> Maybe HWND -> Maybe HMENU -> HINSTANCE -> WindowClosure
  -> IO HWND
createWindowEx estyle cname wname wstyle mb_x mb_y mb_w mb_h mb_parent mb_menu inst closure = do
  -- Freeing the title/window name has been reported
  -- to cause a crash, so let's not do it.
  -- withTString wname $ \ c_wname -> do
  c_wname <- newTString wname
  wnd <- failIfNull "CreateWindowEx" $
    c_CreateWindowEx estyle cname c_wname wstyle
      (maybePos mb_x) (maybePos mb_y) (maybePos mb_w) (maybePos mb_h)
      (maybePtr mb_parent) (maybePtr mb_menu) inst nullPtr
  setWindowClosure wnd closure
  return wnd
foreign import WINDOWS_CCONV "windows.h CreateWindowExW"
  c_CreateWindowEx
    :: WindowStyle -> ClassName -> LPCTSTR -> WindowStyle
    -> Pos -> Pos -> Pos -> Pos
    -> HWND -> HMENU -> HINSTANCE -> LPVOID
    -> IO HWND

----------------------------------------------------------------

defWindowProc :: Maybe HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
defWindowProc mb_wnd msg w l =
  c_DefWindowProc (maybePtr mb_wnd) msg w l
foreign import WINDOWS_CCONV "windows.h DefWindowProcW"
  c_DefWindowProc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT

----------------------------------------------------------------

getClientRect :: HWND -> IO RECT
getClientRect wnd =
  allocaRECT $ \ p_rect -> do
  failIfFalse_ "GetClientRect" $ c_GetClientRect wnd p_rect
  peekRECT p_rect
foreign import WINDOWS_CCONV unsafe "windows.h GetClientRect"
  c_GetClientRect :: HWND -> Ptr RECT -> IO Bool

getWindowRect :: HWND -> IO RECT
getWindowRect wnd =
  allocaRECT $ \ p_rect -> do
  failIfFalse_ "GetWindowRect" $ c_GetWindowRect wnd p_rect
  peekRECT p_rect
foreign import WINDOWS_CCONV unsafe "windows.h GetWindowRect"
  c_GetWindowRect :: HWND -> Ptr RECT -> IO Bool

-- Should it be Maybe RECT instead?

invalidateRect :: Maybe HWND -> Maybe LPRECT -> Bool -> IO ()
invalidateRect wnd p_mb_rect erase =
  failIfFalse_ "InvalidateRect" $
    c_InvalidateRect (maybePtr wnd) (maybePtr p_mb_rect) erase
foreign import WINDOWS_CCONV "windows.h InvalidateRect"
  c_InvalidateRect :: HWND -> LPRECT -> Bool -> IO Bool

screenToClient :: HWND -> POINT -> IO POINT
screenToClient wnd pt =
  withPOINT pt $ \ p_pt -> do
  failIfFalse_ "ScreenToClient" $ c_ScreenToClient wnd p_pt
  peekPOINT p_pt
foreign import WINDOWS_CCONV unsafe "windows.h ScreenToClient"
  c_ScreenToClient :: HWND -> Ptr POINT -> IO Bool

clientToScreen :: HWND -> POINT -> IO POINT
clientToScreen wnd pt =
  withPOINT pt $ \ p_pt -> do
  failIfFalse_ "ClientToScreen" $ c_ClientToScreen wnd p_pt
  peekPOINT p_pt
foreign import WINDOWS_CCONV unsafe "windows.h ClientToScreen"
  c_ClientToScreen :: HWND -> Ptr POINT -> IO Bool

----------------------------------------------------------------
-- Setting window text/label
----------------------------------------------------------------
-- For setting the title bar text.  But inconvenient to make the LPCTSTR

setWindowText :: HWND -> String -> IO ()
setWindowText wnd text =
  withTString text $ \ c_text ->
  failIfFalse_ "SetWindowText" $ c_SetWindowText wnd c_text
foreign import WINDOWS_CCONV "windows.h SetWindowTextW"
  c_SetWindowText :: HWND -> LPCTSTR -> IO Bool

----------------------------------------------------------------
-- Paint struct
----------------------------------------------------------------

type PAINTSTRUCT =
 ( HDC   -- hdc
 , Bool  -- fErase
 , RECT  -- rcPaint
 )

type LPPAINTSTRUCT   = Addr

sizeofPAINTSTRUCT :: DWORD
sizeofPAINTSTRUCT = #{size PAINTSTRUCT}

allocaPAINTSTRUCT :: (LPPAINTSTRUCT -> IO a) -> IO a
allocaPAINTSTRUCT = allocaBytes #{size PAINTSTRUCT}

beginPaint :: HWND -> LPPAINTSTRUCT -> IO HDC
beginPaint wnd paint =
  failIfNull "BeginPaint" $ c_BeginPaint wnd paint
foreign import WINDOWS_CCONV "windows.h BeginPaint"
  c_BeginPaint :: HWND -> LPPAINTSTRUCT -> IO HDC

foreign import WINDOWS_CCONV "windows.h EndPaint"
  endPaint :: HWND -> LPPAINTSTRUCT -> IO ()
-- Apparently always succeeds (return non-zero)

----------------------------------------------------------------
-- ShowWindow
----------------------------------------------------------------

type ShowWindowControl   = DWORD

#{enum ShowWindowControl,
 , sW_HIDE              = SW_HIDE
 , sW_SHOWNORMAL        = SW_SHOWNORMAL
 , sW_SHOWMINIMIZED     = SW_SHOWMINIMIZED
 , sW_SHOWMAXIMIZED     = SW_SHOWMAXIMIZED
 , sW_MAXIMIZE          = SW_MAXIMIZE
 , sW_SHOWNOACTIVATE    = SW_SHOWNOACTIVATE
 , sW_SHOW              = SW_SHOW
 , sW_MINIMIZE          = SW_MINIMIZE
 , sW_SHOWMINNOACTIVE   = SW_SHOWMINNOACTIVE
 , sW_SHOWNA            = SW_SHOWNA
 , sW_RESTORE           = SW_RESTORE
 }

foreign import WINDOWS_CCONV "windows.h ShowWindow"
  showWindow :: HWND  -> ShowWindowControl  -> IO Bool

----------------------------------------------------------------
-- Misc
----------------------------------------------------------------

adjustWindowRect :: RECT -> WindowStyle -> Bool -> IO RECT
adjustWindowRect rect style menu =
  withRECT rect $ \ p_rect -> do
  failIfFalse_ "AdjustWindowRect" $ c_AdjustWindowRect p_rect style menu
  peekRECT p_rect
foreign import WINDOWS_CCONV unsafe "windows.h AdjustWindowRect"
  c_AdjustWindowRect :: Ptr RECT -> WindowStyle -> Bool -> IO Bool

adjustWindowRectEx :: RECT -> WindowStyle -> Bool -> WindowStyleEx -> IO RECT
adjustWindowRectEx rect style menu exstyle =
  withRECT rect $ \ p_rect -> do
  failIfFalse_ "AdjustWindowRectEx" $
    c_AdjustWindowRectEx p_rect style menu exstyle
  peekRECT p_rect
foreign import WINDOWS_CCONV unsafe "windows.h AdjustWindowRectEx"
  c_AdjustWindowRectEx :: Ptr RECT -> WindowStyle -> Bool -> WindowStyleEx -> IO Bool

-- Win2K and later:
-- %fun AllowSetForegroundWindow :: DWORD -> IO ()

-- %
-- %dis animateWindowType x = dWORD x
-- type AnimateWindowType   = DWORD

-- %const AnimateWindowType
--        [ AW_SLIDE
--        , AW_ACTIVATE
--        , AW_BLEND
--        , AW_HIDE
--        , AW_CENTER
--        , AW_HOR_POSITIVE
--        , AW_HOR_NEGATIVE
--        , AW_VER_POSITIVE
--        , AW_VER_NEGATIVE
--        ]

-- Win98 or Win2K:
-- %fun AnimateWindow :: HWND -> DWORD -> AnimateWindowType -> IO ()
-- %code BOOL success = AnimateWindow(arg1,arg2,arg3)
-- %fail { !success } { ErrorWin("AnimateWindow") }

foreign import WINDOWS_CCONV unsafe "windows.h AnyPopup"
  anyPopup :: IO Bool

arrangeIconicWindows :: HWND -> IO ()
arrangeIconicWindows wnd =
  failIfFalse_ "ArrangeIconicWindows" $ c_ArrangeIconicWindows wnd
foreign import WINDOWS_CCONV unsafe "windows.h ArrangeIconicWindows"
  c_ArrangeIconicWindows :: HWND -> IO Bool

beginDeferWindowPos :: Int -> IO HDWP
beginDeferWindowPos n =
  failIfNull "BeginDeferWindowPos" $ c_BeginDeferWindowPos n
foreign import WINDOWS_CCONV unsafe "windows.h BeginDeferWindowPos"
  c_BeginDeferWindowPos :: Int -> IO HDWP

bringWindowToTop :: HWND -> IO ()
bringWindowToTop wnd =
  failIfFalse_ "BringWindowToTop" $ c_BringWindowToTop wnd
foreign import WINDOWS_CCONV "windows.h BringWindowToTop"
  c_BringWindowToTop :: HWND -> IO Bool

-- Can't pass structs with current FFI, so use a C wrapper (in Types)
childWindowFromPoint :: HWND -> POINT -> IO (Maybe HWND)
childWindowFromPoint wnd pt =
  withPOINT pt $ \ p_pt ->
  liftM ptrToMaybe $ prim_ChildWindowFromPoint wnd p_pt

-- Can't pass structs with current FFI, so use a C wrapper (in Types)
childWindowFromPointEx :: HWND -> POINT -> DWORD -> IO (Maybe HWND)
childWindowFromPointEx parent pt flags =
  withPOINT pt $ \ p_pt ->
  liftM ptrToMaybe $ prim_ChildWindowFromPointEx parent p_pt flags

closeWindow :: HWND -> IO ()
closeWindow wnd =
  failIfFalse_ "CloseWindow" $ c_DestroyWindow wnd

deferWindowPos :: HDWP -> HWND -> HWND -> Int -> Int -> Int -> Int -> SetWindowPosFlags -> IO HDWP
deferWindowPos wp wnd after x y cx cy flags =
  failIfNull "DeferWindowPos" $ c_DeferWindowPos wp wnd after x y cx cy flags
foreign import WINDOWS_CCONV unsafe "windows.h DeferWindowPos"
  c_DeferWindowPos :: HDWP -> HWND -> HWND -> Int -> Int -> Int -> Int -> SetWindowPosFlags -> IO HDWP

destroyWindow :: HWND -> IO ()
destroyWindow wnd =
  failIfFalse_ "DestroyWindow" $ c_DestroyWindow wnd
foreign import WINDOWS_CCONV "windows.h DestroyWindow"
  c_DestroyWindow :: HWND -> IO Bool

endDeferWindowPos :: HDWP -> IO ()
endDeferWindowPos pos =
  failIfFalse_ "EndDeferWindowPos" $ c_EndDeferWindowPos pos
foreign import WINDOWS_CCONV unsafe "windows.h EndDeferWindowPos"
  c_EndDeferWindowPos :: HDWP -> IO Bool

findWindow :: Maybe String -> Maybe String -> IO (Maybe HWND)
findWindow cname wname =
  maybeWith withTString cname $ \ c_cname ->
  maybeWith withTString wname $ \ c_wname ->
  liftM ptrToMaybe $ c_FindWindow c_cname c_wname

{-# DEPRECATED findWindowByName "Use 'findWindow Nothing' instead." #-}
findWindowByName :: String -> IO (Maybe HWND)
findWindowByName wname = findWindow Nothing $ Just wname

foreign import WINDOWS_CCONV unsafe "windows.h FindWindowW"
  c_FindWindow :: LPCTSTR -> LPCTSTR -> IO HWND

findWindowEx :: Maybe HWND -> Maybe HWND -> Maybe String -> Maybe String -> IO (Maybe HWND)
findWindowEx parent after cname wname =
  maybeWith withTString cname $ \ c_cname ->
  maybeWith withTString wname $ \ c_wname ->
  liftM ptrToMaybe $ c_FindWindowEx (maybePtr parent) (maybePtr after) c_cname c_wname
foreign import WINDOWS_CCONV unsafe "windows.h FindWindowExW"
  c_FindWindowEx :: HWND -> HWND -> LPCTSTR -> LPCTSTR -> IO HWND

foreign import WINDOWS_CCONV unsafe "windows.h FlashWindow"
  flashWindow :: HWND -> Bool -> IO Bool
-- No error code

moveWindow :: HWND -> Int -> Int -> Int -> Int -> Bool -> IO ()
moveWindow wnd x y w h repaint =
  failIfFalse_ "MoveWindow" $ c_MoveWindow wnd x y w h repaint
foreign import WINDOWS_CCONV "windows.h MoveWindow"
  c_MoveWindow :: HWND -> Int -> Int -> Int -> Int -> Bool -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h GetDesktopWindow"
  getDesktopWindow :: IO HWND

foreign import WINDOWS_CCONV unsafe "windows.h GetForegroundWindow"
  getForegroundWindow :: IO HWND

getParent :: HWND -> IO HWND
getParent wnd =
  failIfNull "GetParent" $ c_GetParent wnd
foreign import WINDOWS_CCONV unsafe "windows.h GetParent"
  c_GetParent :: HWND -> IO HWND

getTopWindow :: HWND -> IO HWND
getTopWindow wnd =
  failIfNull "GetTopWindow" $ c_GetTopWindow wnd
foreign import WINDOWS_CCONV unsafe "windows.h GetTopWindow"
  c_GetTopWindow :: HWND -> IO HWND


type SetWindowPosFlags = UINT

#{enum SetWindowPosFlags,
 , sWP_NOSIZE           = SWP_NOSIZE
 , sWP_NOMOVE           = SWP_NOMOVE
 , sWP_NOZORDER         = SWP_NOZORDER
 , sWP_NOREDRAW         = SWP_NOREDRAW
 , sWP_NOACTIVATE       = SWP_NOACTIVATE
 , sWP_FRAMECHANGED     = SWP_FRAMECHANGED
 , sWP_SHOWWINDOW       = SWP_SHOWWINDOW
 , sWP_HIDEWINDOW       = SWP_HIDEWINDOW
 , sWP_NOCOPYBITS       = SWP_NOCOPYBITS
 , sWP_NOOWNERZORDER    = SWP_NOOWNERZORDER
 , sWP_NOSENDCHANGING   = SWP_NOSENDCHANGING
 , sWP_DRAWFRAME        = SWP_DRAWFRAME
 , sWP_NOREPOSITION     = SWP_NOREPOSITION
 }

----------------------------------------------------------------
-- HDCs
----------------------------------------------------------------

type GetDCExFlags   = DWORD

#{enum GetDCExFlags,
 , dCX_WINDOW           = DCX_WINDOW
 , dCX_CACHE            = DCX_CACHE
 , dCX_CLIPCHILDREN     = DCX_CLIPCHILDREN
 , dCX_CLIPSIBLINGS     = DCX_CLIPSIBLINGS
 , dCX_PARENTCLIP       = DCX_PARENTCLIP
 , dCX_EXCLUDERGN       = DCX_EXCLUDERGN
 , dCX_INTERSECTRGN     = DCX_INTERSECTRGN
 , dCX_LOCKWINDOWUPDATE = DCX_LOCKWINDOWUPDATE
 }

-- apparently mostly fails if you use invalid hwnds

getDCEx :: HWND -> HRGN -> GetDCExFlags -> IO HDC
getDCEx wnd rgn flags =
  withForeignPtr rgn $ \ p_rgn ->
  failIfNull "GetDCEx" $ c_GetDCEx wnd p_rgn flags
foreign import WINDOWS_CCONV unsafe "windows.h GetDCEx"
  c_GetDCEx :: HWND -> PRGN -> GetDCExFlags -> IO HDC

getDC :: Maybe HWND -> IO HDC
getDC mb_wnd =
  failIfNull "GetDC" $ c_GetDC (maybePtr mb_wnd)
foreign import WINDOWS_CCONV unsafe "windows.h GetDC"
  c_GetDC :: HWND -> IO HDC

getWindowDC :: Maybe HWND -> IO HDC
getWindowDC mb_wnd =
  failIfNull "GetWindowDC" $ c_GetWindowDC (maybePtr mb_wnd)
foreign import WINDOWS_CCONV unsafe "windows.h GetWindowDC"
  c_GetWindowDC :: HWND -> IO HDC

releaseDC :: Maybe HWND -> HDC -> IO ()
releaseDC mb_wnd dc =
  failIfFalse_ "ReleaseDC" $ c_ReleaseDC (maybePtr mb_wnd) dc
foreign import WINDOWS_CCONV unsafe "windows.h ReleaseDC"
  c_ReleaseDC :: HWND -> HDC -> IO Bool

getDCOrgEx :: HDC -> IO POINT
getDCOrgEx dc =
  allocaPOINT $ \ p_pt -> do
  failIfFalse_ "GetDCOrgEx" $ c_GetDCOrgEx dc p_pt
  peekPOINT p_pt
foreign import WINDOWS_CCONV unsafe "windows.h GetDCOrgEx"
  c_GetDCOrgEx :: HDC -> Ptr POINT -> IO Bool

----------------------------------------------------------------
-- Caret
----------------------------------------------------------------

hideCaret :: HWND -> IO ()
hideCaret wnd =
  failIfFalse_ "HideCaret" $ c_HideCaret wnd
foreign import WINDOWS_CCONV unsafe "windows.h HideCaret"
  c_HideCaret :: HWND -> IO Bool

showCaret :: HWND -> IO ()
showCaret wnd =
  failIfFalse_ "ShowCaret" $ c_ShowCaret wnd
foreign import WINDOWS_CCONV unsafe "windows.h ShowCaret"
  c_ShowCaret :: HWND -> IO Bool

-- ToDo: allow arg2 to be NULL or {(HBITMAP)1}

createCaret :: HWND -> HBITMAP -> Maybe INT -> Maybe INT -> IO ()
createCaret wnd bm mb_w mb_h =
  failIfFalse_ "CreateCaret" $
    c_CreateCaret wnd bm (maybeNum mb_w) (maybeNum mb_h)
foreign import WINDOWS_CCONV unsafe "windows.h CreateCaret"
  c_CreateCaret :: HWND -> HBITMAP -> INT -> INT -> IO Bool

destroyCaret :: IO ()
destroyCaret =
  failIfFalse_ "DestroyCaret" $ c_DestroyCaret
foreign import WINDOWS_CCONV unsafe "windows.h DestroyCaret"
  c_DestroyCaret :: IO Bool

getCaretPos :: IO POINT
getCaretPos =
  allocaPOINT $ \ p_pt -> do
  failIfFalse_ "GetCaretPos" $ c_GetCaretPos p_pt
  peekPOINT p_pt
foreign import WINDOWS_CCONV unsafe "windows.h GetCaretPos"
  c_GetCaretPos :: Ptr POINT -> IO Bool

setCaretPos :: POINT -> IO ()
setCaretPos (x,y) =
  failIfFalse_ "SetCaretPos" $ c_SetCaretPos x y
foreign import WINDOWS_CCONV unsafe "windows.h SetCaretPos"
  c_SetCaretPos :: LONG -> LONG -> IO Bool

-- The remarks on SetCaretBlinkTime are either highly risible or very sad -
-- depending on whether you plan to use this function.

----------------------------------------------------------------
-- MSGs and event loops
--
-- Note that the following functions have to be reentrant:
--
--   DispatchMessage
--   SendMessage
--   UpdateWindow   (I think)
--   RedrawWindow   (I think)
--
-- The following dont have to be reentrant (according to documentation)
--
--   GetMessage
--   PeekMessage
--   TranslateMessage
--
-- For Hugs (and possibly NHC too?) this is no big deal.
-- For GHC, you have to use casm_GC instead of casm.
-- (It might be simpler to just put all this code in another
-- file and build it with the appropriate command line option...)
----------------------------------------------------------------

-- type MSG =
--   ( HWND   -- hwnd;
--   , UINT   -- message;
--   , WPARAM -- wParam;
--   , LPARAM -- lParam;
--   , DWORD  -- time;
--   , POINT  -- pt;
--   )

type LPMSG   = Addr

allocaMessage :: (LPMSG -> IO a) -> IO a
allocaMessage = allocaBytes #{size MSG}

-- A NULL window requests messages for any window belonging to this thread.
-- a "success" value of 0 indicates that WM_QUIT was received

getMessage :: LPMSG -> Maybe HWND -> IO Bool
getMessage msg mb_wnd = do
  res <- failIf (== -1) "GetMessage" $
    c_GetMessage msg (maybePtr mb_wnd) 0 0
  return (res /= 0)
foreign import WINDOWS_CCONV "windows.h GetMessageW"
  c_GetMessage :: LPMSG -> HWND -> UINT -> UINT -> IO LONG

-- A NULL window requests messages for any window belonging to this thread.
-- Arguably the code block shouldn't be a 'safe' one, but it shouldn't really
-- hurt.

peekMessage :: LPMSG -> Maybe HWND -> UINT -> UINT -> UINT -> IO ()
peekMessage msg mb_wnd filterMin filterMax remove = do
  failIf_ (== -1) "PeekMessage" $
    c_PeekMessage msg (maybePtr mb_wnd) filterMin filterMax remove
foreign import WINDOWS_CCONV "windows.h PeekMessageW"
  c_PeekMessage :: LPMSG -> HWND -> UINT -> UINT -> UINT -> IO LONG

-- Note: you're not supposed to call this if you're using accelerators

foreign import WINDOWS_CCONV "windows.h TranslateMessage"
  translateMessage :: LPMSG -> IO BOOL

updateWindow :: HWND -> IO ()
updateWindow wnd =
  failIfFalse_ "UpdateWindow" $ c_UpdateWindow wnd
foreign import WINDOWS_CCONV "windows.h UpdateWindow"
  c_UpdateWindow :: HWND -> IO Bool

-- Return value of DispatchMessage is usually ignored

foreign import WINDOWS_CCONV "windows.h DispatchMessageW"
  dispatchMessage :: LPMSG -> IO LONG

foreign import WINDOWS_CCONV "windows.h SendMessageW"
  sendMessage :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT

----------------------------------------------------------------

-- ToDo: figure out reentrancy stuff
-- ToDo: catch error codes
--
-- ToDo: how to send HWND_BROADCAST to PostMessage
-- %fun PostMessage       :: MbHWND -> WindowMessage -> WPARAM -> LPARAM -> IO ()
-- %fun PostQuitMessage   :: Int -> IO ()
-- %fun PostThreadMessage :: DWORD -> WindowMessage -> WPARAM -> LPARAM -> IO ()
-- %fun InSendMessage     :: IO Bool

----------------------------------------------------------------
-- End
----------------------------------------------------------------

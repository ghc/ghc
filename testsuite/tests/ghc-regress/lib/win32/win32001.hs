-- Haskell version of "Hello, World" using the Win32 library.
-- Demonstrates how the Win32 library can be put to use.
-- (c) sof 1999


module Main(main) where

import qualified Win32
import Addr 

-- Toplevel main just creates a window and pumps messages.
-- The window procedure (wndProc) we pass in is partially
-- applied with the user action that takes care of responding
-- to repaint messages (WM_PAINT).

main :: IO ()
main = do
  lpps <- Win32.malloc Win32.sizeofPAINTSTRUCT
  hwnd <- createWindow 200 200 (wndProc lpps onPaint)
  messagePump hwnd

-- OnPaint handler for a window - draw a string centred
-- inside it.
onPaint :: Win32.RECT -> Win32.HDC -> IO ()
onPaint (_,_,w,h) hdc = do
   Win32.setBkMode hdc Win32.tRANSPARENT
   Win32.setTextColor hdc (Win32.rgb 255 255 0)
   let y | h==10     = 0
         | otherwise = ((h-10) `div` 2)
       x | w==50     = 0
         | otherwise = (w-50) `div` 2
   Win32.textOut hdc x y "Hello, world"
   return ()

-- Simple window procedure - one way to improve and generalise
-- it would be to pass it a message map (represented as a 
-- finite map from WindowMessages to actions, perhaps).

wndProc :: Win32.LPPAINTSTRUCT
	-> (Win32.RECT -> Win32.HDC -> IO ()) -- on paint action
        -> Win32.HWND
        -> Win32.WindowMessage
	-> Win32.WPARAM
	-> Win32.LPARAM
	-> IO Win32.LRESULT
wndProc lpps onPaint hwnd wmsg wParam lParam
 | wmsg == Win32.wM_DESTROY = do
     Win32.sendMessage hwnd Win32.wM_QUIT 1 0
     return 0
 | wmsg == Win32.wM_PAINT && hwnd /= nullAddr = do
     r <- Win32.getClientRect hwnd
     paintWith lpps hwnd (onPaint r)
     return 0
 | otherwise = 
     Win32.defWindowProc (Just hwnd) wmsg wParam lParam

createWindow :: Int -> Int -> Win32.WindowClosure -> IO Win32.HWND
createWindow width height wndProc = do
  let winClass = Win32.mkClassName "Hello"
  icon         <- Win32.loadIcon   Nothing Win32.iDI_APPLICATION
  cursor       <- Win32.loadCursor Nothing Win32.iDC_ARROW
  bgBrush      <- Win32.createSolidBrush (Win32.rgb 0 0 255)
  mainInstance <- Win32.getModuleHandle Nothing
  Win32.registerClass
  	  ( Win32.cS_VREDRAW + Win32.cS_HREDRAW
	  , mainInstance
	  , Just icon
	  , Just cursor
	  , Just bgBrush
	  , Nothing
	  , winClass
	  )
  w <- Win32.createWindow 
  		 winClass
		 "Hello, World example"
		 Win32.wS_OVERLAPPEDWINDOW
		 Nothing Nothing -- leave it to the shell to decide the position
		 		 -- at where to put the window initially
                 (Just width)
		 (Just height)
		 Nothing      -- no parent, i.e, root window is the parent.
		 Nothing      -- no menu handle
		 mainInstance
		 wndProc
  Win32.showWindow w Win32.sW_SHOWNORMAL
  Win32.updateWindow w
  return w

messagePump :: Win32.HWND -> IO ()
messagePump hwnd = do
  msg  <- Win32.getMessage (Just hwnd) `catch` \ _ -> return nullAddr
  if msg == nullAddr then
    return ()
   else do
    Win32.translateMessage msg
    Win32.dispatchMessage msg
    messagePump hwnd

paintWith :: Win32.LPPAINTSTRUCT -> Win32.HWND -> (Win32.HDC -> IO a) -> IO a
paintWith lpps hwnd p = do
  hdc  <- Win32.beginPaint hwnd lpps
  a    <- p hdc
  Win32.endPaint hwnd lpps
  return a

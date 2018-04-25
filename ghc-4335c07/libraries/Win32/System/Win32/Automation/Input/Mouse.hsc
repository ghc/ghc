{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.Automation.Input.Mouse
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Mouse input events
-}
module System.Win32.Automation.Input.Mouse where
import Foreign.Ptr               ( Ptr )
import Foreign.Storable          ( Storable(..) )
import System.Win32.Types        ( LONG, ULONG_PTR )
import System.Win32.Word         ( DWORD )

#include <windows.h>
#include "winuser_compat.h"
#include "alignment.h"

type PMOUSEINPUT = Ptr MOUSEINPUT

data MOUSEINPUT = MOUSEINPUT
     { dx           :: LONG
     , dy           :: LONG
     , mouseData    :: DWORD
     , dwFlags      :: DWORD
     , time         :: DWORD
     , dwExtraInfo :: ULONG_PTR
     } deriving Show

instance Storable MOUSEINPUT where
    sizeOf = const #{size MOUSEINPUT}
    alignment _ = #alignment MOUSEINPUT
    poke buf input = do
        (#poke MOUSEINPUT, dx) buf (dx input)
        (#poke MOUSEINPUT, dx) buf (dx input)
        (#poke MOUSEINPUT, mouseData)   buf (mouseData input)
        (#poke MOUSEINPUT, dwFlags)     buf (dwFlags input)
        (#poke MOUSEINPUT, time)        buf (time input)
        (#poke MOUSEINPUT, dwExtraInfo) buf (dwExtraInfo input)
    peek buf = do
        dx' <- (#peek MOUSEINPUT, dx) buf
        dy' <- (#peek MOUSEINPUT, dy) buf
        mouseData'   <- (#peek MOUSEINPUT, mouseData) buf
        dwFlags'     <- (#peek MOUSEINPUT, dwFlags) buf
        time'        <- (#peek MOUSEINPUT, time) buf
        dwExtraInfo' <- (#peek MOUSEINPUT, dwExtraInfo) buf
        return $ MOUSEINPUT dx' dy' mouseData' dwFlags' time' dwExtraInfo'

#{enum DWORD,
 , xBUTTON1 = XBUTTON1
 , xBUTTON2 = XBUTTON2
 }

#{enum DWORD,
 , mOUSEEVENTF_ABSOLUTE    = MOUSEEVENTF_ABSOLUTE
 , mOUSEEVENTF_MOVE        = MOUSEEVENTF_MOVE
 , mOUSEEVENTF_LEFTDOWN    = MOUSEEVENTF_LEFTDOWN
 , mOUSEEVENTF_LEFTUP      = MOUSEEVENTF_LEFTUP
 , mOUSEEVENTF_RIGHTDOWN   = MOUSEEVENTF_RIGHTDOWN
 , mOUSEEVENTF_RIGHTUP     = MOUSEEVENTF_RIGHTUP
 , mOUSEEVENTF_MIDDLEDOWN  = MOUSEEVENTF_MIDDLEDOWN
 , mOUSEEVENTF_MIDDLEUP    = MOUSEEVENTF_MIDDLEUP
 , mOUSEEVENTF_WHEEL       = MOUSEEVENTF_WHEEL
 , mOUSEEVENTF_XDOWN       = MOUSEEVENTF_XDOWN
 , mOUSEEVENTF_XUP         = MOUSEEVENTF_XUP
 }

{-
 , mOUSEEVENTF_VIRTUALDESK = MOUSEEVENTF_VIRTUALDESK -- I don't know why we can't find this
 , mOUSEEVENTF_HWHEEL      = MOUSEEVENTF_HWHEEL
 , mOUSEEVENTF_MOVE_NOCOALESCE = MOUSEEVENTF_MOVE_NOCOALESCE
-}

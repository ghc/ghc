{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.Automation.Input.Key
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Keyboard input events
-}
module System.Win32.Automation.Input.Key where
import Foreign.Ptr        ( Ptr )
import Foreign.Storable   ( Storable(..) )
import System.Win32.Types ( ULONG_PTR )
import System.Win32.Word  ( DWORD, WORD )

#include <windows.h>
#include "winuser_compat.h"
#include "alignment.h"

type PKEYBDINPUT = Ptr KEYBDINPUT

data KEYBDINPUT = KEYBDINPUT
     { wVk         :: WORD
     , wScan       :: WORD
     , dwFlags     :: DWORD
     , time        :: DWORD
     , dwExtraInfo :: ULONG_PTR
     } deriving Show

instance Storable KEYBDINPUT where
    sizeOf = const #{size KEYBDINPUT}
    alignment _ = #alignment KEYBDINPUT
    poke buf input = do
        (#poke KEYBDINPUT, wVk)     buf (wVk input)
        (#poke KEYBDINPUT, wScan)   buf (wScan input)
        (#poke KEYBDINPUT, dwFlags) buf (dwFlags input)
        (#poke KEYBDINPUT, time)    buf (time input)
        (#poke KEYBDINPUT, dwExtraInfo) buf (dwExtraInfo input)
    peek buf = do
        wVk'         <- (#peek KEYBDINPUT, wVk) buf
        wScan'       <- (#peek KEYBDINPUT, wScan) buf
        dwFlags'     <- (#peek KEYBDINPUT, dwFlags) buf
        time'        <- (#peek KEYBDINPUT, time) buf
        dwExtraInfo' <- (#peek KEYBDINPUT, dwExtraInfo) buf
        return $ KEYBDINPUT wVk' wScan' dwFlags' time' dwExtraInfo'

#{enum DWORD,
 , kEYEVENTF_EXTENDEDKEY = KEYEVENTF_EXTENDEDKEY
 , kEYEVENTF_KEYUP       = KEYEVENTF_KEYUP
 , kEYEVENTF_SCANCODE    = KEYEVENTF_SCANCODE
 , kEYEVENTF_UNICODE     = KEYEVENTF_UNICODE
 }

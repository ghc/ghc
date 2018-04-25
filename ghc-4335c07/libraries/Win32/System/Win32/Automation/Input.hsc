{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.Automation.Input
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Provide sendInput function and INPUT types.
-}
module System.Win32.Automation.Input
  ( module System.Win32.Automation.Input
  , module System.Win32.Automation.Input.Key
  , module System.Win32.Automation.Input.Mouse
  ) where

import Data.Bits                 ( (.|.) )
import Foreign.Ptr               ( Ptr )
import Foreign.Storable          ( Storable(..) )
import Foreign.Marshal.Array     ( withArrayLen )
import Foreign.C.Types           ( CIntPtr(..) )
import Graphics.Win32.Key        ( VKey, c_MapVirtualKey )
import System.Win32.Automation.Input.Key
import System.Win32.Automation.Input.Mouse ( MOUSEINPUT )
import System.Win32.Automation.Input.Mouse hiding ( MOUSEINPUT(..) )
import System.Win32.Types        ( UINT, LPARAM, failIfZero )
import System.Win32.Word         ( DWORD, WORD )

#include <windows.h>
#include "alignment.h"
##include "windows_cconv.h"
#include "winuser_compat.h"

sendInput :: [INPUT] -> IO UINT
sendInput input
  = withArrayLen input $ \len c_input ->
      sendInputPtr len c_input

{-# INLINE sendInputPtr #-}
-- | Raw pointer of array version of 'sendInput'.
-- Use this function to support non-list sequence.
sendInputPtr :: Int -> Ptr INPUT -> IO UINT
sendInputPtr len c_input
  = failIfZero "SendInput" $
      c_SendInput (fromIntegral len) c_input $ sizeOf (undefined :: INPUT)

foreign import WINDOWS_CCONV unsafe "windows.h SendInput"
    c_SendInput :: UINT -> LPINPUT -> Int -> IO UINT

makeKeyboardInput :: VKey -> Maybe DWORD -> IO INPUT
makeKeyboardInput vkey flag = do
    let flag' = maybe kEYEVENTF_EXTENDEDKEY (kEYEVENTF_EXTENDEDKEY .|.) flag
    scan         <- c_MapVirtualKey vkey 0
    dwExtraInfo' <- getMessageExtraInfo
    return $ Keyboard
           $ KEYBDINPUT {
                 wVk   = fromIntegral vkey
               , wScan = fromIntegral scan
               , dwFlags = flag'
               , time = 0
               , dwExtraInfo = fromIntegral $ dwExtraInfo'
               }

type PINPUT = Ptr INPUT
type LPINPUT = Ptr INPUT

data INPUT = Mouse MOUSEINPUT | Keyboard KEYBDINPUT | OtherHardware HARDWAREINPUT
     deriving Show

instance Storable INPUT where
    sizeOf = const #{size INPUT}
    alignment _ = #alignment INPUT

    poke buf (Mouse mouse) = do
        (#poke INPUT, type) buf (#{const INPUT_MOUSE}:: DWORD)
        (#poke INPUT, mi) buf mouse
    poke buf (Keyboard key) = do
        (#poke INPUT, type) buf (#{const INPUT_KEYBOARD} :: DWORD)
        (#poke INPUT, ki) buf key
    poke buf (OtherHardware hard) = do
        (#poke INPUT, type) buf (#{const INPUT_HARDWARE} :: DWORD)
        (#poke INPUT, hi) buf hard

    peek buf = do
        type'  <- (#peek INPUT, type) buf :: IO DWORD
        case type' of
          #{const INPUT_MOUSE} ->
              Mouse `fmap` (#peek INPUT, mi) buf
          #{const INPUT_KEYBOARD} ->
              Keyboard `fmap` (#peek INPUT, ki) buf
          _ -> OtherHardware `fmap` (#peek INPUT, hi) buf


type PHARDWAREINPUT = Ptr HARDWAREINPUT

data HARDWAREINPUT = HARDWAREINPUT
     { uMsg    :: DWORD
     , wParamL :: WORD
     , wParamH :: WORD
     } deriving Show

instance Storable HARDWAREINPUT where
    sizeOf = const #{size HARDWAREINPUT}
    alignment _ = #alignment HARDWAREINPUT
    poke buf input = do
        (#poke HARDWAREINPUT, uMsg)    buf (uMsg input)
        (#poke HARDWAREINPUT, wParamL) buf (wParamL input)
        (#poke HARDWAREINPUT, wParamH) buf (wParamH input)
    peek buf = do
        uMsg'    <- (#peek HARDWAREINPUT, uMsg) buf
        wParamL' <- (#peek HARDWAREINPUT, wParamL) buf
        wParamH' <- (#peek HARDWAREINPUT, wParamH) buf
        return $ HARDWAREINPUT uMsg' wParamL' wParamH'

foreign import WINDOWS_CCONV unsafe "windows.h GetMessageExtraInfo"
    getMessageExtraInfo :: IO LPARAM

foreign import WINDOWS_CCONV unsafe "windows.h SetMessageExtraInfo"
    setMessageExtraInfo :: LPARAM -> IO LPARAM

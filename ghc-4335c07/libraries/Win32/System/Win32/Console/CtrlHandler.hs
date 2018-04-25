{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.Console.CtrlHandler
   Copyright   :  2008-2013 Judah Jacobson, 2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Set handlers of console Ctrl events.
-}
module System.Win32.Console.CtrlHandler 
  ( CtrlEvent, Handler, PHANDLER_ROUTINE
  , withConsoleCtrlHandler
  , setConsoleCtrlHandler, c_SetConsoleCtrlHandler
  , mkHandler
  , cTRL_C_EVENT, cTRL_BREAK_EVENT
  ) where

import Control.Exception    ( bracket )
import Control.Monad        ( void )
import Foreign.Ptr          ( FunPtr )
import System.Win32.Console ( CtrlEvent, cTRL_C_EVENT, cTRL_BREAK_EVENT )
import System.Win32.Types   ( BOOL, failIfFalse_ )

#include "windows_cconv.h"

type Handler = CtrlEvent -> IO BOOL
-- type HandlerRoutine = Handler
type PHANDLER_ROUTINE = FunPtr Handler

withConsoleCtrlHandler :: Handler -> IO a -> IO a
withConsoleCtrlHandler handler io
  = bracket (do hd <- mkHandler handler
                -- don't fail if we can't set the Ctrl-C handler
                -- for example, we might not be attached to a console?
                void $ c_SetConsoleCtrlHandler hd True
                return hd)
            (\hd -> void $ c_SetConsoleCtrlHandler hd False)
            $ const io

-- | This function isn't suitable when we want to set the cTRL_C_EVENT handler.
-- If you want to set the cTRL_C_EVENT handler, use 'c_SetConsoleCtrlHandler' instead.
setConsoleCtrlHandler :: PHANDLER_ROUTINE -> BOOL -> IO ()
setConsoleCtrlHandler handler flag
  = failIfFalse_ "SetConsoleCtrlHandler"
      $ c_SetConsoleCtrlHandler handler flag

foreign import WINDOWS_CCONV "wrapper" mkHandler :: Handler -> IO PHANDLER_ROUTINE
foreign import WINDOWS_CCONV "windows.h SetConsoleCtrlHandler"
  c_SetConsoleCtrlHandler :: PHANDLER_ROUTINE -> BOOL -> IO BOOL

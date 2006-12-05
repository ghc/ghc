{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ConsoleHandler
-- Copyright   :  (c) The University of Glasgow
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- NB. the contents of this module are only available on Windows.
--
-- Installing Win32 console handlers.
-- 
-----------------------------------------------------------------------------

module GHC.ConsoleHandler
#if !defined(mingw32_HOST_OS) && !defined(__HADDOCK__)
	where
import Prelude -- necessary to get dependencies right
#else /* whole file */
	( Handler(..)
	, installHandler
	, ConsoleEvent(..)
	, flushConsole
	) where

{-
#include "Signals.h"
-}

import Prelude -- necessary to get dependencies right

import Foreign
import Foreign.C
import GHC.IOBase
import GHC.Handle
import Data.Typeable

data Handler
 = Default
 | Ignore
 | Catch (ConsoleEvent -> IO ())

data ConsoleEvent
 = ControlC
 | Break
 | Close
    -- these are sent to Services only.
 | Logoff
 | Shutdown
 deriving (Eq, Ord, Enum, Show, Read, Typeable)

-- | Allows Windows console events to be caught and handled.  To
-- handle a console event, call 'installHandler' passing the
-- appropriate 'Handler' value.  When the event is received, if the
-- 'Handler' value is @Catch f@, then a new thread will be spawned by
-- the system to execute @f e@, where @e@ is the 'ConsoleEvent' that
-- was received.
--
-- Note that console events can only be received by an application
-- running in a Windows console.  Certain environments that look like consoles
-- do not support console events, these include:
--
--  * Cygwin shells with @CYGWIN=tty@ set (if you don't set @CYGWIN=tty@,
--    then a Cygwin shell behaves like a Windows console).
--  * Cygwin xterm and rxvt windows
--  * MSYS rxvt windows
--
-- In order for your application to receive console events, avoid running
-- it in one of these environments.
--
installHandler :: Handler -> IO Handler
installHandler handler = 
  alloca $ \ p_sp -> do
   rc <- 
    case handler of
     Default -> rts_installHandler STG_SIG_DFL p_sp
     Ignore  -> rts_installHandler STG_SIG_IGN p_sp
     Catch h -> do
        v <- newStablePtr (toHandler h)
	poke p_sp v
	rts_installHandler STG_SIG_HAN p_sp
   case rc of
     STG_SIG_DFL -> return Default
     STG_SIG_IGN -> return Ignore
     STG_SIG_HAN -> do
        osptr <- peek p_sp
        oldh  <- deRefStablePtr osptr
	 -- stable pointer is no longer in use, free it.
	freeStablePtr osptr
	return (Catch (\ ev -> oldh (fromConsoleEvent ev)))
  where
   toConsoleEvent ev = 
     case ev of
       0 {- CTRL_C_EVENT-}        -> Just ControlC
       1 {- CTRL_BREAK_EVENT-}    -> Just Break
       2 {- CTRL_CLOSE_EVENT-}    -> Just Close
       5 {- CTRL_LOGOFF_EVENT-}   -> Just Logoff
       6 {- CTRL_SHUTDOWN_EVENT-} -> Just Shutdown
       _ -> Nothing
   fromConsoleEvent ev = 
     case ev of
       ControlC -> 0 {- CTRL_C_EVENT-}
       Break    -> 1 {- CTRL_BREAK_EVENT-}
       Close    -> 2 {- CTRL_CLOSE_EVENT-}
       Logoff   -> 5 {- CTRL_LOGOFF_EVENT-}
       Shutdown -> 6 {- CTRL_SHUTDOWN_EVENT-}

   toHandler hdlr ev = do
      case toConsoleEvent ev of
	 -- see rts/win32/ConsoleHandler.c for comments as to why
	 -- rts_ConsoleHandlerDone is called here.
        Just x  -> hdlr x >> rts_ConsoleHandlerDone ev
	Nothing -> return () -- silently ignore..

foreign import ccall unsafe "RtsExternal.h rts_InstallConsoleEvent" 
  rts_installHandler :: CInt -> Ptr (StablePtr (CInt -> IO ())) -> IO CInt
foreign import ccall unsafe "RtsExternal.h rts_ConsoleHandlerDone"
  rts_ConsoleHandlerDone :: CInt -> IO ()


flushConsole :: Handle -> IO ()
flushConsole h = 
  wantReadableHandle "flushConsole" h $ \ h_ -> 
     throwErrnoIfMinus1Retry_ "flushConsole"
      (flush_console_fd (fromIntegral (haFD h_)))

foreign import ccall unsafe "consUtils.h flush_input_console__"
	flush_console_fd :: CInt -> IO CInt
#endif /* mingw32_HOST_OS */

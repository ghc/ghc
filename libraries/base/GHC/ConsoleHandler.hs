{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ConsoleHandler
-- Copyright   :  whatevah
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Installing Win32 console handlers.
-- 
-----------------------------------------------------------------------------

module GHC.ConsoleHandler
#ifndef mingw32_HOST_OS
	where
import Prelude -- necessary to get dependencies right
#else /* whole file */
	( Handler(..)
	, installHandler
	, ConsoleEvent(..)
	) where

{-
#include "Signals.h"
-}

import Prelude -- necessary to get dependencies right

import Foreign
import Foreign.C

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
        Just x  -> hdlr x
	Nothing -> return () -- silently ignore..

foreign import ccall unsafe "Signals.h stg_InstallConsoleEvent" 
  rts_installHandler :: CInt -> Ptr (StablePtr (CInt -> IO ())) -> IO CInt
#endif /* mingw32_HOST_OS */

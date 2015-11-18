{-# LANGUAGE CPP #-}
module GHCi.Signals (installSignalHandlers) where

import Control.Concurrent
import Control.Exception
import System.Mem.Weak  ( deRefWeak )

#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler
#endif

-- | Install standard signal handlers for catching ^C, which just throw an
--   exception in the target thread.  The current target thread is the
--   thread at the head of the list in the MVar passed to
--   installSignalHandlers.
installSignalHandlers :: IO ()
installSignalHandlers = do
  main_thread <- myThreadId
  wtid <- mkWeakThreadId main_thread

  let interrupt = do
        r <- deRefWeak wtid
        case r of
          Nothing -> return ()
          Just t  -> throwTo t UserInterrupt

#if !defined(mingw32_HOST_OS)
  _ <- installHandler sigQUIT  (Catch interrupt) Nothing
  _ <- installHandler sigINT   (Catch interrupt) Nothing
#else
  -- GHC 6.3+ has support for console events on Windows
  -- NOTE: running GHCi under a bash shell for some reason requires
  -- you to press Ctrl-Break rather than Ctrl-C to provoke
  -- an interrupt.  Ctrl-C is getting blocked somewhere, I don't know
  -- why --SDM 17/12/2004
  let sig_handler ControlC = interrupt
      sig_handler Break    = interrupt
      sig_handler _        = return ()

  _ <- installHandler (Catch sig_handler)
#endif
  return ()

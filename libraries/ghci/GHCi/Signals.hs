{-# LANGUAGE CPP #-}
module GHCi.Signals (installSignalHandlers) where

import Prelude -- See note [Why do we import Prelude here?]
import Control.Concurrent
import Control.Exception
import System.Mem.Weak  ( deRefWeak )

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim
#elif !defined(mingw32_HOST_OS)
import System.Posix.Signals
#endif

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler
#endif

-- | Install standard signal handlers for catching ^C, which just throw an
--   exception in the target thread.  The current target thread is the
--   thread at the head of the list in the MVar passed to
--   installSignalHandlers.
#if defined(wasm32_HOST_ARCH)
installSignalHandlers :: (JSVal -> IO ()) -> IO ()
installSignalHandlers cb_sig = do
#else
installSignalHandlers :: IO ()
installSignalHandlers = do
#endif
  main_thread <- myThreadId
  wtid <- mkWeakThreadId main_thread

  let interrupt = do
        r <- deRefWeak wtid
        case r of
          Nothing -> return ()
          Just t  -> throwTo t UserInterrupt

#if defined(wasm32_HOST_ARCH)
  cb_sig =<< js_export_signal_handler interrupt
#elif !defined(mingw32_HOST_OS)
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

#if defined(wasm32_HOST_ARCH)

-- Note [wasm ghci signal handlers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- For native ghc with -fexternal-interpreter, when you press ^C,
-- SIGINT is delivered to both ghc/iserv. iserv SIGINT handler raises
-- an async UserInterrupt exception in the main thread, which might
-- handle it right away, or later since it's doing I/O under
-- uninterruptibleMask.
--
-- wasm is no different here. The node process needs to catch the same
-- signals. Instead of calling process.on('SIGINT', handler) in
-- Haskell via JSFFI, we pass a JS callback that registers the handler
-- from iserv main, export the Haskell handler to JS, then invoke the
-- callback to register the handler. This allows iserv to be run in
-- non-nodejs environments as well, and the dyld script can pass a
-- handler register callback other than process.on().
--
-- IMPORTANT: THE SIGNAL HANDLER MUST BE EXPORTED AS ASYNC!!!!!!!
-- Otherwise, throwTo may block the handler thread, so the handler
-- thread is removed from the run queue. Since the main thread may
-- also be absent from the run queue (e.g. blocked on waiting for
-- input message), the run queue is empty and then the RTS scheduler
-- panics.

foreign import javascript "wrapper"
  js_export_signal_handler :: IO () -> IO JSVal

#endif

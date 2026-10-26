{-# LANGUAGE CPP #-}
module GHCi.Signals (ignoreInterrupts) where

import Prelude -- See note [Why do we import Prelude here?]

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim
#elif !defined(mingw32_HOST_OS)
import System.Posix.Signals
#endif

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler
#endif

-- | Install standard signal handlers for catching ^C, which just ignore it.
-- (We don't want the external interpreter to be interrupted when we ^C in
-- ghci; ghci should terminate the external server cleanly instead.)
#if defined(wasm32_HOST_ARCH)
ignoreInterrupts :: (JSVal -> IO ()) -> IO ()
ignoreInterrupts cb_sig = do
  -- See Note [wasm ghci signal handlers]
  cb_sig =<< js_export_signal_handler (return ())
#elif !defined(mingw32_HOST_OS)
ignoreInterrupts :: IO ()
ignoreInterrupts = do
  -- use `Catch (return ())` instead of `Ignore`, because `Ignore` gets
  -- inherited by subprocesses the server may spawn.
  _ <- installHandler sigQUIT (Catch (return ())) Nothing
  _ <- installHandler sigINT  (Catch (return ())) Nothing
  return ()
#else
ignoreInterrupts :: IO ()
ignoreInterrupts = do
  _ <- installHandler (Catch (\_ -> return ()))
  return ()
#endif

#if defined(wasm32_HOST_ARCH)

-- Note [wasm ghci signal handlers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- For native ghc with -fexternal-interpreter, when you press ^C,
-- SIGINT is delivered to both ghc/iserv. iserv SIGINT handler raises
-- an async UserInterrupt exception in the main thread
--
-- wasm is no different here. The node process needs to catch the same
-- signals. Instead of calling process.on('SIGINT', handler) in
-- Haskell via JSFFI, we pass a JS callback that registers the handler
-- from iserv main, export the Haskell handler to JS, then invoke the
-- callback to register the handler. This allows iserv to be run in
-- non-nodejs environments as well, and the dyld script can pass a
-- handler register callback other than process.on().

foreign import javascript "wrapper"
  js_export_signal_handler :: IO () -> IO JSVal

#endif

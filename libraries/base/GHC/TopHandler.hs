{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.TopHandler
-- Copyright   :  (c) The University of Glasgow, 2001-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Support for catching exceptions raised during top-level computations
-- (e.g. @Main.main@, 'Control.Concurrent.forkIO', and foreign exports)
--
-----------------------------------------------------------------------------

module GHC.TopHandler (
        runMainIO, runIO, runIOFastExit, runNonIO,
        topHandler, topHandlerFastExit,
        reportStackOverflow, reportError,
        flushStdHandles
    ) where

#include "HsBaseConfig.h"

import Control.Exception
import Data.Maybe

import Foreign
import Foreign.C
import GHC.Base
import GHC.Conc hiding (throwTo)
import GHC.Real
import GHC.IO
import GHC.IO.Handle.FD
import GHC.IO.Handle
import GHC.IO.Exception
import GHC.Weak

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler
#else
import Data.Dynamic (toDyn)
#endif

-- | 'runMainIO' is wrapped around 'Main.main' (or whatever main is
-- called in the program).  It catches otherwise uncaught exceptions,
-- and also flushes stdout\/stderr before exiting.
runMainIO :: IO a -> IO a
runMainIO main =
    do
      main_thread_id <- myThreadId
      weak_tid <- mkWeakThreadId main_thread_id
      install_interrupt_handler $ do
           m <- deRefWeak weak_tid
           case m of
               Nothing  -> return ()
               Just tid -> throwTo tid (toException UserInterrupt)
      main -- hs_exit() will flush
    `catch`
      topHandler

install_interrupt_handler :: IO () -> IO ()
#ifdef mingw32_HOST_OS
install_interrupt_handler handler = do
  _ <- GHC.ConsoleHandler.installHandler $
     Catch $ \event ->
        case event of
           ControlC -> handler
           Break    -> handler
           Close    -> handler
           _ -> return ()
  return ()
#else
#include "rts/Signals.h"
-- specialised version of System.Posix.Signals.installHandler, which
-- isn't available here.
install_interrupt_handler handler = do
   let sig = CONST_SIGINT :: CInt
   _ <- setHandler sig (Just (const handler, toDyn handler))
   _ <- stg_sig_install sig STG_SIG_RST nullPtr
     -- STG_SIG_RST: the second ^C kills us for real, just in case the
     -- RTS or program is unresponsive.
   return ()

foreign import ccall unsafe
  stg_sig_install
        :: CInt                         -- sig no.
        -> CInt                         -- action code (STG_SIG_HAN etc.)
        -> Ptr ()                       -- (in, out) blocked
        -> IO CInt                      -- (ret) old action code
#endif

-- | 'runIO' is wrapped around every @foreign export@ and @foreign
-- import \"wrapper\"@ to mop up any uncaught exceptions.  Thus, the
-- result of running 'System.Exit.exitWith' in a foreign-exported
-- function is the same as in the main thread: it terminates the
-- program.
--
runIO :: IO a -> IO a
runIO main = catch main topHandler

-- | Like 'runIO', but in the event of an exception that causes an exit,
-- we don't shut down the system cleanly, we just exit.  This is
-- useful in some cases, because the safe exit version will give other
-- threads a chance to clean up first, which might shut down the
-- system in a different way.  For example, try
--
--   main = forkIO (runIO (exitWith (ExitFailure 1))) >> threadDelay 10000
--
-- This will sometimes exit with "interrupted" and code 0, because the
-- main thread is given a chance to shut down when the child thread calls
-- safeExit.  There is a race to shut down between the main and child threads.
--
runIOFastExit :: IO a -> IO a
runIOFastExit main = catch main topHandlerFastExit
        -- NB. this is used by the testsuite driver

-- | The same as 'runIO', but for non-IO computations.  Used for
-- wrapping @foreign export@ and @foreign import \"wrapper\"@ when these
-- are used to export Haskell functions with non-IO types.
--
runNonIO :: a -> IO a
runNonIO a = catch (a `seq` return a) topHandler

topHandler :: SomeException -> IO a
topHandler err = catch (real_handler safeExit err) topHandler

topHandlerFastExit :: SomeException -> IO a
topHandlerFastExit err =
  catchException (real_handler fastExit err) topHandlerFastExit

-- Make sure we handle errors while reporting the error!
-- (e.g. evaluating the string passed to 'error' might generate
--  another error, etc.)
--
real_handler :: (Int -> IO a) -> SomeException -> IO a
real_handler exit se = do
  flushStdHandles -- before any error output
  case fromException se of
      Just StackOverflow -> do
           reportStackOverflow
           exit 2

      Just UserInterrupt  -> exitInterrupted

      _ -> case fromException se of
           -- only the main thread gets ExitException exceptions
           Just ExitSuccess     -> exit 0
           Just (ExitFailure n) -> exit n

           -- EPIPE errors received for stdout are ignored (#2699)
           _ -> case fromException se of
                Just IOError{ ioe_type = ResourceVanished,
                              ioe_errno = Just ioe,
                              ioe_handle = Just hdl }
                   | Errno ioe == ePIPE, hdl == stdout -> exit 0
                _ -> do reportError se
                        exit 1


-- try to flush stdout/stderr, but don't worry if we fail
-- (these handles might have errors, and we don't want to go into
-- an infinite loop).
flushStdHandles :: IO ()
flushStdHandles = do
  hFlush stdout `catchAny` \_ -> return ()
  hFlush stderr `catchAny` \_ -> return ()

safeExit, fastExit :: Int -> IO a
safeExit = exitHelper useSafeExit
fastExit = exitHelper useFastExit

unreachable :: IO a
unreachable = fail "If you can read this, shutdownHaskellAndExit did not exit."

exitHelper :: CInt -> Int -> IO a
#ifdef mingw32_HOST_OS
exitHelper exitKind r =
  shutdownHaskellAndExit (fromIntegral r) exitKind >> unreachable
#else
-- On Unix we use an encoding for the ExitCode:
--      0 -- 255  normal exit code
--   -127 -- -1   exit by signal
-- For any invalid encoding we just use a replacement (0xff).
exitHelper exitKind r
  | r >= 0 && r <= 255
  = shutdownHaskellAndExit   (fromIntegral   r)  exitKind >> unreachable
  | r >= -127 && r <= -1
  = shutdownHaskellAndSignal (fromIntegral (-r)) exitKind >> unreachable
  | otherwise
  = shutdownHaskellAndExit   0xff                exitKind >> unreachable

foreign import ccall "shutdownHaskellAndSignal"
  shutdownHaskellAndSignal :: CInt -> CInt -> IO ()
#endif

exitInterrupted :: IO a
exitInterrupted =
#ifdef mingw32_HOST_OS
  safeExit 252
#else
  -- we must exit via the default action for SIGINT, so that the
  -- parent of this process can take appropriate action (see #2301)
  safeExit (-CONST_SIGINT)
#endif

-- NOTE: shutdownHaskellAndExit must be called "safe", because it *can*
-- re-enter Haskell land through finalizers.
foreign import ccall "Rts.h shutdownHaskellAndExit"
  shutdownHaskellAndExit :: CInt -> CInt -> IO ()

useFastExit, useSafeExit :: CInt
useFastExit = 1
useSafeExit = 0

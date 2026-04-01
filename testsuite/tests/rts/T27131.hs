{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import Control.Concurrent
import Control.Monad
import Foreign.C.Types
import GHC.Conc.Sync (ThreadId(..), forkOn, myThreadId, setNumCapabilities)
import GHC.Exts (ThreadId#)

foreign import ccall unsafe "rts_enableStopNextBreakpoint"
  rts_enableStopNextBreakpoint :: ThreadId# -> IO ()

foreign import ccall unsafe "rts_disableStopNextBreakpoint"
  rts_disableStopNextBreakpoint :: ThreadId# -> IO ()

foreign import ccall unsafe "rts_enableStopAfterReturn"
  rts_enableStopAfterReturn :: ThreadId# -> IO ()

foreign import ccall unsafe "rts_disableStopAfterReturn"
  rts_disableStopAfterReturn :: ThreadId# -> IO ()

foreign import ccall unsafe "has_local_stop_next_breakpoint"
  c_hasLocalStopNextBreakpoint :: IO CInt

foreign import ccall unsafe "has_local_stop_after_return"
  c_hasLocalStopAfterReturn :: IO CInt

main :: IO ()
main = do
  setNumCapabilities 2
  checkFlag
    "TSO_STOP_NEXT_BREAKPOINT"
    rts_enableStopNextBreakpoint
    rts_disableStopNextBreakpoint
    c_hasLocalStopNextBreakpoint
  checkFlag
    "TSO_STOP_AFTER_RETURN"
    rts_enableStopAfterReturn
    rts_disableStopAfterReturn
    c_hasLocalStopAfterReturn

checkFlag
  :: String
  -> (ThreadId# -> IO ())
  -> (ThreadId# -> IO ())
  -> IO CInt
  -> IO ()
checkFlag label enable disable isMyThreadFlagSet = do
  -- Print the main thread's capability (should be 0)
  print =<< threadCapability =<< myThreadId

  -- Target thread will write its own flag value here
  targetCheckVar <- newEmptyMVar

  -- Run the new TSO runs on capability 1
  ThreadId tid# <- forkOn 1 $ do
    replicateM_ 2 $ do
      replyVar <- takeMVar targetCheckVar
      isSet <- (/= 0) <$> isMyThreadFlagSet
      putMVar replyVar isSet

  -- Enable the other TSO's flag
  enable tid#
  -- It will check whether it is set and reply here
  renderCheck label "set" =<< checkTarget targetCheckVar

  -- Ditto.
  disable tid#
  renderCheck label "unset" . not =<< checkTarget targetCheckVar

checkTarget :: MVar (MVar Bool) -> IO Bool
checkTarget targetCheckVar = do
  replyVar <- newEmptyMVar
  putMVar targetCheckVar replyVar
  takeMVar replyVar

renderCheck :: String -> String -> Bool -> IO ()
renderCheck label state ok = putStrLn $
  label ++ " " ++ state ++ ": " ++ if ok then "ok" else "failed"

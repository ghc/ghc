{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module GHCi.Debugger
  (
  -- * Single step mode
    rts_enableStopNextBreakpoint
  , rts_enableStopNextBreakpointAll
  , rts_disableStopNextBreakpoint
  , rts_disableStopNextBreakpointAll

  -- * Step out mode
  , rts_enableStopAfterReturn
  , rts_disableStopAfterReturn

  -- * Stop on exception
  , rts_enableStopOnException
  , rts_disableStopOnException
  , rts_enableStopOnExceptionAll
  , rts_disableStopOnExceptionAll

  -- * Breakpoint Callback
  , BreakpointCallback
  , breakPointIOAction
  ) where

import Prelude -- See note [Why do we import Prelude here?]

import GHC.Base (ThreadId#, Addr#, Int#)
import Foreign (StablePtr, Ptr)
import GHCi.RemoteTypes (HValue)

--------------------------------------------------------------------------------
-- Single step mode

-- | Enables the single step mode for a specific thread, thus stopping only on
-- breakpoints in that thread.
foreign import ccall unsafe "rts_enableStopNextBreakpoint"
  rts_enableStopNextBreakpoint :: ThreadId# -> IO ()

-- | Disables per-thread single-step mode. Note: if global single-step is
-- enabled we stop at all breakpoints regardless of the per-thread flag.
foreign import ccall unsafe "rts_disableStopNextBreakpoint"
  rts_disableStopNextBreakpoint :: ThreadId# -> IO ()

-- | Enables the single step mode for all threads, thus stopping at any
-- existing breakpoint.
foreign import ccall unsafe "rts_enableStopNextBreakpointAll"
  rts_enableStopNextBreakpointAll :: IO ()

-- | Disables the single step mode for all threads
foreign import ccall unsafe "rts_disableStopNextBreakpointAll"
  rts_disableStopNextBreakpointAll :: IO ()

--------------------------------------------------------------------------------
-- Step out mode

foreign import ccall unsafe "rts_enableStopAfterReturn"
  rts_enableStopAfterReturn :: ThreadId# -> IO ()

foreign import ccall unsafe "rts_disableStopAfterReturn"
  rts_disableStopAfterReturn :: ThreadId# -> IO ()

--------------------------------------------------------------------------------

-- | Make this thread break when an exception is hit
foreign import ccall unsafe "rts_enableStopOnException"  rts_enableStopOnException  :: ThreadId# -> IO ()

-- | No longer have this thread break when an exception is hit
foreign import ccall unsafe "rts_disableStopOnException" rts_disableStopOnException :: ThreadId# -> IO ()

-- | Enables global break-on-exception for all threads
foreign import ccall unsafe "rts_enableStopOnExceptionAll"  rts_enableStopOnExceptionAll :: IO ()

-- | Disables the global break-on-exception for all threads. Individual threads
-- with break-on-exception will still break on exception if the global flag is
-- disabled.
foreign import ccall unsafe "rts_disableStopOnExceptionAll" rts_disableStopOnExceptionAll :: IO ()

--------------------------------------------------------------------------------

type BreakpointCallback
     = Addr#   -- pointer to the breakpoint info module name
    -> Addr#   -- pointer to the breakpoint info module unit id
    -> Int#    -- breakpoint info index
    -> Bool    -- exception?
    -> HValue  -- the AP_STACK, or exception
    -> IO ()

foreign import ccall "&rts_breakpoint_io_action"
   breakPointIOAction :: Ptr (StablePtr BreakpointCallback)


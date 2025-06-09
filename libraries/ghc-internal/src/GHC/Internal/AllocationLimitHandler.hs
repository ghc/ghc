{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# OPTIONS_HADDOCK not-home #-}
module GHC.Internal.AllocationLimitHandler
  ( runAllocationLimitHandler
  , setGlobalAllocationLimitHandler
  , AllocationLimitKillBehaviour(..)
  , getAllocationCounterFor
  , setAllocationCounterFor
  , enableAllocationLimitFor
  , disableAllocationLimitFor
  )
  where
import GHC.Internal.Base
import GHC.Internal.Conc.Sync (ThreadId(..))
import GHC.Internal.Data.IORef (IORef, readIORef, writeIORef, newIORef)
import GHC.Internal.Foreign.C.Types
import GHC.Internal.IO (unsafePerformIO)
import GHC.Internal.Int (Int64(..))


{-# NOINLINE allocationLimitHandler #-}
allocationLimitHandler :: IORef (ThreadId -> IO ())
allocationLimitHandler = unsafePerformIO (newIORef defaultHandler)

defaultHandler :: ThreadId -> IO ()
defaultHandler _ = pure ()

foreign import ccall "setAllocLimitKill" setAllocLimitKill :: CBool -> CBool -> IO ()

runAllocationLimitHandler :: ThreadId# -> IO ()
runAllocationLimitHandler tid = do
  hook <- getAllocationLimitHandler
  hook $ ThreadId tid

getAllocationLimitHandler :: IO (ThreadId -> IO ())
getAllocationLimitHandler = readIORef allocationLimitHandler

data AllocationLimitKillBehaviour =
  KillOnAllocationLimit
  -- ^ Throw a @AllocationLimitExceeded@ async exception to the thread when the
  -- allocation limit is exceeded.
  | DontKillOnAllocationLimit
  -- ^ Do not throw an exception when the allocation limit is exceeded.

-- | Define the behaviour for handling allocation limits.
-- The default behaviour is to throw an @AllocationLimitExceeded@ async exception to the thread.
-- This can be overriden using @AllocationLimitKillBehaviour@.
--
-- We can set a user-specified handler, which can be run in addition to
-- or in place of the exception.
-- This allows for instance logging on the allocation limit being exceeded,
-- or dynamically determining whether to terminate the thread.
-- The handler is not guaranteed to run before the thread is terminated or restarted.
--
-- Note: that if you don't terminate the thread, then the allocation limit gets
-- removed.
-- If you wish to keep the allocation limit you will have to reset it using
-- @setAllocationCounter@ and @enableAllocationLimit@.
setGlobalAllocationLimitHandler :: AllocationLimitKillBehaviour -> Maybe (ThreadId -> IO ()) -> IO ()
setGlobalAllocationLimitHandler killBehaviour mHandler = do
  shouldRunHandler <- case mHandler of
    Just hook -> do
      writeIORef allocationLimitHandler hook
      pure 1
    Nothing -> do
      writeIORef allocationLimitHandler defaultHandler
      pure 0
  let shouldKill =
        case killBehaviour of
          KillOnAllocationLimit -> 1
          DontKillOnAllocationLimit -> 0
  setAllocLimitKill shouldKill shouldRunHandler

-- | Retrieves the allocation counter for the another thread.
foreign import prim "stg_getOtherThreadAllocationCounterzh" getOtherThreadAllocationCounter#
  :: ThreadId#
  -> State# RealWorld
  -> (# State# RealWorld, Int64# #)

-- | Get the allocation counter for a different thread.
--
-- Note: this doesn't take the current nursery chunk into account.
-- If the thread is running then it may underestimate allocations by the size of a nursery thread.
getAllocationCounterFor :: ThreadId -> IO Int64
getAllocationCounterFor (ThreadId t#) = IO $ \s ->
  case getOtherThreadAllocationCounter# t# s of (# s', i# #)  -> (# s', I64# i# #)

-- | Set the allocation counter for a different thread.
-- This can be combined with 'enableAllocationLimitFor' to enable allocation limits for another thread.
-- You may wish to do this during a user-specified allocation limit handler.
--
-- Note: this doesn't take the current nursery chunk into account.
-- If the thread is running then it may overestimate allocations by the size of a nursery thread,
-- and trigger the limit sooner than expected.
setAllocationCounterFor :: Int64 -> ThreadId -> IO ()
setAllocationCounterFor (I64# i#) (ThreadId t#) = IO $ \s ->
  case setOtherThreadAllocationCounter# i# t# s of s' -> (# s', () #)


-- | Enable allocation limit processing the thread @t@.
enableAllocationLimitFor :: ThreadId -> IO ()
enableAllocationLimitFor (ThreadId t) = do
  rts_enableThreadAllocationLimit t

-- | Disable allocation limit processing the thread @t@.
disableAllocationLimitFor :: ThreadId -> IO ()
disableAllocationLimitFor (ThreadId t) = do
  rts_disableThreadAllocationLimit t

foreign import ccall unsafe "rts_enableThreadAllocationLimit"
  rts_enableThreadAllocationLimit :: ThreadId# -> IO ()

foreign import ccall unsafe "rts_disableThreadAllocationLimit"
  rts_disableThreadAllocationLimit :: ThreadId# -> IO ()

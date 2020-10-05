{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Event.Internal
    (
    -- * Event back end
      Backend
    , backend
    , delete
    , poll
    , modifyFd
    , modifyFdOnce
    , module GHC.Event.Internal.Types
    -- * Helpers
    , throwErrnoIfMinus1NoRetry

    -- Atomic ptr exchange for WinIO
    , exchangePtr
    ) where

import Foreign.C.Error (eINTR, getErrno, throwErrno)
import System.Posix.Types (Fd)
import GHC.Base
import GHC.Num (Num(..))
import GHC.Event.Internal.Types

import GHC.Ptr (Ptr(..))

-- | Event notification backend.
data Backend = forall a. Backend {
      _beState :: !a

    -- | Poll backend for new events.  The provided callback is called
    -- once per file descriptor with new events.
    , _bePoll :: a                          -- backend state
              -> Maybe Timeout              -- timeout in milliseconds ('Nothing' for non-blocking poll)
              -> (Fd -> Event -> IO ())     -- I/O callback
              -> IO Int

    -- | Register, modify, or unregister interest in the given events
    -- on the given file descriptor.
    , _beModifyFd :: a
                  -> Fd       -- file descriptor
                  -> Event    -- old events to watch for ('mempty' for new)
                  -> Event    -- new events to watch for ('mempty' to delete)
                  -> IO Bool

    -- | Register interest in new events on a given file descriptor, set
    -- to be deactivated after the first event.
    , _beModifyFdOnce :: a
                         -> Fd    -- file descriptor
                         -> Event -- new events to watch
                         -> IO Bool

    , _beDelete :: a -> IO ()
    }

backend :: (a -> Maybe Timeout -> (Fd -> Event -> IO ()) -> IO Int)
        -> (a -> Fd -> Event -> Event -> IO Bool)
        -> (a -> Fd -> Event -> IO Bool)
        -> (a -> IO ())
        -> a
        -> Backend
backend bPoll bModifyFd bModifyFdOnce bDelete state =
  Backend state bPoll bModifyFd bModifyFdOnce bDelete
{-# INLINE backend #-}

poll :: Backend -> Maybe Timeout -> (Fd -> Event -> IO ()) -> IO Int
poll (Backend bState bPoll _ _ _) = bPoll bState
{-# INLINE poll #-}

-- | Returns 'True' if the modification succeeded.
-- Returns 'False' if this backend does not support
-- event notifications on this type of file.
modifyFd :: Backend -> Fd -> Event -> Event -> IO Bool
modifyFd (Backend bState _ bModifyFd _ _) = bModifyFd bState
{-# INLINE modifyFd #-}

-- | Returns 'True' if the modification succeeded.
-- Returns 'False' if this backend does not support
-- event notifications on this type of file.
modifyFdOnce :: Backend -> Fd -> Event -> IO Bool
modifyFdOnce (Backend bState _ _ bModifyFdOnce _) = bModifyFdOnce bState
{-# INLINE modifyFdOnce #-}

delete :: Backend -> IO ()
delete (Backend bState _ _ _ bDelete) = bDelete bState
{-# INLINE delete #-}

-- | Throw an 'Prelude.IOError' corresponding to the current value of
-- 'getErrno' if the result value of the 'IO' action is -1 and
-- 'getErrno' is not 'eINTR'.  If the result value is -1 and
-- 'getErrno' returns 'eINTR' 0 is returned.  Otherwise the result
-- value is returned.
throwErrnoIfMinus1NoRetry :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1NoRetry loc f = do
    res <- f
    if res == -1
        then do
            err <- getErrno
            if err == eINTR then return 0 else throwErrno loc
        else return res

{-# INLINE exchangePtr #-}
-- | @exchangePtr pptr x@ swaps the pointer pointed to by @pptr@ with the value
-- @x@, returning the old value.
exchangePtr :: Ptr (Ptr a) -> Ptr a -> IO (Ptr a)
exchangePtr (Ptr dst) (Ptr val) =
  IO $ \s ->
      case (atomicExchangeAddrAddr# dst val s) of
        (# s2, old_val #) -> (# s2, Ptr old_val #)

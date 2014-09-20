{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}

module GHC.Event.Internal
    (
    -- * Event back end
      Backend
    , backend
    , delete
    , poll
    , modifyFd
    , modifyFdOnce
    -- * Event type
    , Event
    , evtRead
    , evtWrite
    , evtClose
    , eventIs
    -- * Timeout type
    , Timeout(..)
    -- * Helpers
    , throwErrnoIfMinus1NoRetry
    ) where

import Data.Bits ((.|.), (.&.))
import Data.OldList (foldl', filter, intercalate, null)
import Foreign.C.Error (eINTR, getErrno, throwErrno)
import System.Posix.Types (Fd)
import GHC.Base
import GHC.Num (Num(..))
import GHC.Show (Show(..))

-- | An I\/O event.
newtype Event = Event Int
    deriving (Eq)

evtNothing :: Event
evtNothing = Event 0
{-# INLINE evtNothing #-}

-- | Data is available to be read.
evtRead :: Event
evtRead = Event 1
{-# INLINE evtRead #-}

-- | The file descriptor is ready to accept a write.
evtWrite :: Event
evtWrite = Event 2
{-# INLINE evtWrite #-}

-- | Another thread closed the file descriptor.
evtClose :: Event
evtClose = Event 4
{-# INLINE evtClose #-}

eventIs :: Event -> Event -> Bool
eventIs (Event a) (Event b) = a .&. b /= 0

instance Show Event where
    show e = '[' : (intercalate "," . filter (not . null) $
                    [evtRead `so` "evtRead",
                     evtWrite `so` "evtWrite",
                     evtClose `so` "evtClose"]) ++ "]"
        where ev `so` disp | e `eventIs` ev = disp
                           | otherwise      = ""

instance Monoid Event where
    mempty  = evtNothing
    mappend = evtCombine
    mconcat = evtConcat

evtCombine :: Event -> Event -> Event
evtCombine (Event a) (Event b) = Event (a .|. b)
{-# INLINE evtCombine #-}

evtConcat :: [Event] -> Event
evtConcat = foldl' evtCombine evtNothing
{-# INLINE evtConcat #-}

-- | A type alias for timeouts, specified in seconds.
data Timeout = Timeout {-# UNPACK #-} !Double
             | Forever
               deriving (Show)

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

-- | Throw an 'IOError' corresponding to the current value of
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

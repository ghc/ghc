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
    -- * Lifetimes
    , Lifetime(..)
    , EventLifetime
    , eventLifetime
    , elLifetime
    , elEvent
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
import GHC.Word (Word64)
import GHC.Num (Num(..))
import GHC.Show (Show(..))
import Data.Semigroup.Internal (stimesMonoid)

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

-- | @since 4.3.1.0
instance Show Event where
    show e = '[' : (intercalate "," . filter (not . null) $
                    [evtRead `so` "evtRead",
                     evtWrite `so` "evtWrite",
                     evtClose `so` "evtClose"]) ++ "]"
        where ev `so` disp | e `eventIs` ev = disp
                           | otherwise      = ""

-- | @since 4.10.0.0
instance Semigroup Event where
    (<>)    = evtCombine
    stimes  = stimesMonoid

-- | @since 4.3.1.0
instance Monoid Event where
    mempty  = evtNothing
    mconcat = evtConcat

evtCombine :: Event -> Event -> Event
evtCombine (Event a) (Event b) = Event (a .|. b)
{-# INLINE evtCombine #-}

evtConcat :: [Event] -> Event
evtConcat = foldl' evtCombine evtNothing
{-# INLINE evtConcat #-}

-- | The lifetime of an event registration.
--
-- @since 4.8.1.0
data Lifetime = OneShot   -- ^ the registration will be active for only one
                          -- event
              | MultiShot -- ^ the registration will trigger multiple times
              deriving (Show, Eq)

-- | The longer of two lifetimes.
elSupremum :: Lifetime -> Lifetime -> Lifetime
elSupremum OneShot OneShot = OneShot
elSupremum _       _       = MultiShot
{-# INLINE elSupremum #-}

-- | @since 4.10.0.0
instance Semigroup Lifetime where
    (<>) = elSupremum
    stimes = stimesMonoid

-- | @mappend@ takes the longer of two lifetimes.
--
-- @since 4.8.0.0
instance Monoid Lifetime where
    mempty = OneShot

-- | A pair of an event and lifetime
--
-- Here we encode the event in the bottom three bits and the lifetime
-- in the fourth bit.
newtype EventLifetime = EL Int
                      deriving (Show, Eq)

-- | @since 4.11.0.0
instance Semigroup EventLifetime where
    EL a <> EL b = EL (a .|. b)

-- | @since 4.8.0.0
instance Monoid EventLifetime where
    mempty = EL 0

eventLifetime :: Event -> Lifetime -> EventLifetime
eventLifetime (Event e) l = EL (e .|. lifetimeBit l)
  where
    lifetimeBit OneShot   = 0
    lifetimeBit MultiShot = 8
{-# INLINE eventLifetime #-}

elLifetime :: EventLifetime -> Lifetime
elLifetime (EL x) = if x .&. 8 == 0 then OneShot else MultiShot
{-# INLINE elLifetime #-}

elEvent :: EventLifetime -> Event
elEvent (EL x) = Event (x .&. 0x7)
{-# INLINE elEvent #-}

-- | A type alias for timeouts, specified in nanoseconds.
data Timeout = Timeout {-# UNPACK #-} !Word64
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

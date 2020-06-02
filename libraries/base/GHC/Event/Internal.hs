{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}

module GHC.Event.Internal
    (
    -- * Event back end
      Backend
    , backend
    , IOOperation(..)
    , IOResult(..)
    , delete
    , doIOOperation
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
import GHC.Event.Unique (Unique)
import GHC.Word (Word64)
import GHC.Num (Num(..))
import GHC.Show (Show(..))
import Data.Semigroup.Internal (stimesMonoid)

data IOOperation
    = IOOperation_Read
    -- IOOperation_Write IOOperationID ...
    -- ...

data IOResult
    -- | An event has occurred for a file handle.
    = IOResult_Event Fd Event
    -- | An IOOperation has completed.
    | IOResult_IOComplete Unique

-- | An I\/O event.
newtype Event = Event Int
    deriving Eq -- ^ @since 4.4.0.0

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

-- | @since 4.4.0.0
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

-- | @since 4.4.0.0
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
              deriving ( Show -- ^ @since 4.8.1.0
                       , Eq   -- ^ @since 4.8.1.0
                       )

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
                      deriving ( Show -- ^ @since 4.8.0.0
                               , Eq   -- ^ @since 4.8.0.0
                               )

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
               deriving Show -- ^ @since 4.4.0.0

-- | Event notification backend.
data Backend = forall a. Backend {
      _beState :: !a

    -- | Poll backend for new events.  The provided callback is called
    -- once per file descriptor with new events.
    , _bePoll
        :: a                    -- backend state
        -> Maybe Timeout        -- timeout in milliseconds ('Nothing' for non-blocking poll)
        -> (IOResult -> IO ())  -- I/O callback
        -> IO Int               -- ???? negative is error, 0 is success but no IOResults found, positive is success with IO Results. ???

    -- | Register, modify, or unregister interest in the given events
    -- on the given file descriptor.
    , _beModifyFd :: a
                  -> Fd       -- file descriptor
                  -> Event    -- old events to watch for ('mempty' for new)
                  -> Event    -- new events to watch for ('mempty' to delete)
                  -> IO Bool  -- The Bool indicates True for success,
                              -- False for a known failure, else this may throw
                              -- with `throwErrno`.

    -- | Register interest in new events on a given file descriptor, set
    -- to be deactivated after the first event.
    , _beModifyFdOnce :: a
                         -> Fd    -- file descriptor
                         -> Event -- new events to watch
                         -> IO Bool -- Bool indicates success (see _beModifyFd)

    -- | Perform some IO action (non-blocking).
    , _beDoIOOperation
        :: a
        -> Unique            -- Operation id.
        -> IOOperation       -- action to perform
        -> Maybe (IO Bool)   -- Nothing if the io action is not supported, and
                             -- the caller should use Fd Events instead. Else
                             -- Just the action to do the (non-blocking) IO
                             -- action. Bool indicates success (see _beModifyFd).

    , _beDelete :: a -> IO ()
    }

backend :: (a -> Maybe Timeout -> (IOResult -> IO ()) -> IO Int)
        -> (a -> Fd -> Event -> Event -> IO Bool)
        -> (a -> Fd -> Event -> IO Bool)
        -> (a -> Unique -> IOOperation -> Maybe (IO Bool))
        -> (a -> IO ())
        -> a
        -> Backend
backend bPoll bModifyFd bModifyFdOnce bDoIOOperation bDelete state =
  Backend state bPoll bModifyFd bModifyFdOnce bDoIOOperation bDelete
{-# INLINE backend #-}

poll :: Backend -> Maybe Timeout -> (IOResult -> IO ()) -> IO Int
poll (Backend bState bPoll _ _ _ _) = bPoll bState
{-# INLINE poll #-}

-- | Returns 'True' if the modification succeeded.
-- Returns 'False' if this backend does not support
-- event notifications on this type of file.
modifyFd :: Backend -> Fd -> Event -> Event -> IO Bool
modifyFd (Backend bState _ bModifyFd _ _ _) = bModifyFd bState
{-# INLINE modifyFd #-}

-- | Returns 'True' if the modification succeeded.
-- Returns 'False' if this backend does not support
-- event notifications on this type of file.
modifyFdOnce :: Backend -> Fd -> Event -> IO Bool
modifyFdOnce (Backend bState _ _ bModifyFdOnce _ _) = bModifyFdOnce bState
{-# INLINE modifyFdOnce #-}

doIOOperation :: Backend -> Unique -> IOOperation -> Maybe (IO Bool)
doIOOperation (Backend bState _ _ _ bDoIOOperation _) = bDoIOOperation bState

delete :: Backend -> IO ()
delete (Backend bState _ _ _ _ bDelete) = bDelete bState
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

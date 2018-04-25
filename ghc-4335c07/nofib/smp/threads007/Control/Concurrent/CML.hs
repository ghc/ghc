--------------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.CML
-- Copyright   :  Avik Chaudhuri 2009 (avik@cs.ucsc.edu)
-- License     :  BSD3
--
-- Maintainer  :  ben.franksen@online.de
-- Stability   :  provisional
-- Portability :  portable
--
-- Events and Channels as in Concurrent ML (extended with communication guards)
--
-- See /A Concurrent ML Library in Concurrent Haskell/ by Avik Chaudhuri
-- (avik\@cs.ucsc.edu). The original code as well as the papers can be
-- found at <http://www.cs.umd.edu/~avik/projects/cmllch/>.
--------------------------------------------------------------------------------
module Control.Concurrent.CML (
  -- * Channels
  -- $channels
  Channel,
  channel,
  receive,
  transmit,
  -- * Events
  -- $events
  Event,
  sync,
  choose,
  wrap,
  guard,
  wrapabort,
  spawn
) where

import Control.Concurrent.MVar(MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent(ThreadId, forkIO)
import Control.Monad.Fix(fix)
import Control.Monad(foldM, forever)
import Data.Maybe(isJust)

--------------------------------------------------------------------------------

type Commit = MVar Bool
type Decision = MVar (Maybe Commit)
type Candidate = MVar (Maybe Decision)
type In a = MVar (Candidate, a -> Bool, Synchronizer)
type Out a = MVar (Candidate, a, Synchronizer)

-- | Values of type @a@ can be transported over channels of type @Channel a@.
data Channel a = Channel (In a) (Out a) (MVar a)

instance Eq (Channel a) where
    Channel _ _ m1 == Channel _ _ m2 = m1 == m2

type Point = MVar ()
type Name = MVar [Point]
type Abort = MVar ([Point], IO ())
type Synchronizer = MVar (Point, Decision)

-- | Events return a value on synchronization.
--
-- Note that by construction, an event can synchronize at exactly one
-- /commit point/, where a message is either sent or accepted on a
-- channel. This commit point may be selected among several other,
-- potential commit points. Some code may be run before
-- synchronization, as specified by 'guard' functions throughout the
-- event. Some more code may be run after synchronization, as specified
-- by 'wrap' functions that surround the commit point, and by 'wrapabort'
-- functions that do not surround the commit point.
newtype Event a = Event (Synchronizer -> Abort -> Name -> IO a)

--------------------------------------------------------------------------------

atchan :: In a -> Out a -> IO ()
atchan i o = do
  (cand_i,patt,si) <- takeMVar i
  (cand_o,y,so) <- takeMVar o
  if (patt y && si /= so)
    then do
      dec_i <- newEmptyMVar
      putMVar cand_i (Just dec_i)
      ki <- takeMVar dec_i
      dec_o <- newEmptyMVar
      putMVar cand_o (Just dec_o)
      ko <- takeMVar dec_o
      maybe (return ()) (\ci -> putMVar ci (isJust ko)) ki
      maybe (return ()) (\co -> putMVar co (isJust ki)) ko
    else do
      putMVar cand_i Nothing
      putMVar cand_o Nothing
      atchan i o

atsync :: Synchronizer -> Abort -> IO () -> IO ()
atsync r a x = do
  (t,s) <- takeMVar r
  forkIO $ fix $ \z -> do
    (_,s') <- takeMVar r
    forkIO z
    putMVar s' Nothing
  c <- newEmptyMVar
  putMVar s (Just c)
  b <- takeMVar c
  if b
    then do
      putMVar t ()
      fix $ \z -> do
        (tL,f) <- takeMVar a
        forkIO z
        if elem t tL
          then return ()
          else f
    else x

atpointI :: Synchronizer -> Point -> In a -> (a -> Bool) -> IO a -> IO a
atpointI r t i patt x = do
  e <- newEmptyMVar
  putMVar i (e,patt,r)
  ms <- takeMVar e
  maybe
    (atpointI r t i patt x)
    (\s -> do
       putMVar r (t,s)
       takeMVar t
       x
    )
    ms

atpointO :: Synchronizer -> Point -> Out a -> a -> IO () -> IO ()
atpointO r t o y x = do
  e <- newEmptyMVar
  putMVar o (e,y,r)
  ms <- takeMVar e
  maybe
    (atpointO r t o y x)
    (\s -> do
       putMVar r (t,s)
       takeMVar t
       x
    )
    ms

--------------------------------------------------------------------------------

-- $channels
-- Channels transport a single value at a time. The operations on channels are:
-- creation, transmit, and receive. None of them block the calling thread, in
-- fact transmit and receive are pure functions, not IO actions. Blocking may
-- occur only when a thread explicitly synchronizes on the resulting event.

-- | Create a new channel.
channel :: IO (Channel a)
channel = do
  i <- newEmptyMVar
  o <- newEmptyMVar
  forkIO $ forever $ atchan i o
  m <- newEmptyMVar
  return (Channel i o m)

-- | Receive a message from a channel.
--
-- More precisely, @receive c cond@ returns an event that, on synchronization,
-- accepts a message @m@ on channel @c@ and returns @m@. The resulting
-- event is eligible for synchronization with a @transmit c m@ only if @cond m@
-- is true.
receive :: Channel a -> (a -> Bool) -> Event a
receive (Channel i _ m) patt = Event efun where
  efun r _ n = do
    t <- newEmptyMVar
    forkIO (putMVar n [t])
    atpointI r t i patt (takeMVar m)

-- | Transmit a message over a channel.
--
-- More precisely, @transmit c m@ returns an event that, on synchronization,
-- sends the message @m@ on channel @c@ and returns @()@. Such an event must
-- synchronize with @receive c@.
transmit :: Channel a -> a -> Event ()
transmit (Channel _ o m) y = Event efun where
  efun r _ n = do
    t <- newEmptyMVar
    forkIO (putMVar n [t])
    atpointO r t o y (putMVar m y)

-- $events
-- Events encapsulate a potentially blocking point of synchronization between
-- threads, together with possible pre- and post-synchronization code as well
-- as code that is executed (in a separate thread) when an event is /not/
-- selected (aborted).

-- | Non-deterministically select an event from a list of events, so that
-- the selected event can be synchronized. The other events in the list are
-- /aborted/.
choose :: [Event a] -> Event a
choose vL = Event efun where
  efun r a n = do
    j <- newEmptyMVar
    tL <- foldM (\tL -> \(Event v) -> do
        n' <- newEmptyMVar
        forkIO $ v r a n' >>= putMVar j
        tL' <- takeMVar n'
        putMVar n' tL'
        return (tL' ++ tL)
      ) [] vL
    forkIO (putMVar n tL)
    takeMVar j

-- | Specify a post-synchronization action.
--
-- More precisely, @wrap v f@ returns an event that, on synchronization,
-- synchronizes the event @v@ and then runs the action returned by @f@
-- applied to the result.
wrap :: Event a -> (a -> IO b) -> Event b
wrap (Event v) f = Event efun where
  efun r a n = v r a n >>= f

-- | Specify a pre-synchronization action.
--
-- More precisely, @guard a@ returns an event that, on synchronization,
-- synchronizes the event returned by the action @a@. Here, @a@ is run
-- every time a thread /tries/ to synchronize @guard a@.
guard :: IO (Event a) -> Event a
guard vs = Event efun where
  efun r a n = do
    Event v <- vs
    v r a n

-- | Specify a post-synchronization action that is spawned if an event is
-- /not/ selected by a 'choose'.
--
-- More precisely, @wrapabort a v@ returns an event that, on
-- synchronization, synchronizes the event @v@, and on abortion, spawns a
-- thread that runs the action @a@. Here, if @v@ itself is of the form
-- @choose vs@ and one of the events in @vs@ is selected, then @v@ is
-- considered selected, so @a@ is not spawned.
wrapabort :: IO () -> Event a -> Event a
wrapabort f (Event v) = Event efun where
  efun r a n = do
    forkIO $ do
      tL <- takeMVar n
      putMVar n tL
      putMVar a (tL, f)
    v r a n

-- | Synchronize an event.
--
-- This blocks the calling thread until a matching event is available.
sync :: Event a -> IO a
sync (Event v) = do
  j <- newEmptyMVar
  forkIO $ fix $ \z -> do
    r <- newEmptyMVar
    a <- newEmptyMVar
    n <- newEmptyMVar
    forkIO $ atsync r a z
    x <- v r a n
    putMVar j x
  takeMVar j

-- | A synonym for 'forkIO'.
spawn :: IO () -> IO ThreadId
spawn = forkIO

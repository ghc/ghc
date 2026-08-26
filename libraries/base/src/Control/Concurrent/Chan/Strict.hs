-- | Unbounded channels whose contents are always in weak head normal form
-- (WHNF).
--
-- This module is a thin wrapper around "Control.Concurrent.Chan": each
-- operation has the same semantics as its counterpart there, except that the
-- value written to the channel is forced to WHNF first.
-- "Control.Concurrent.Chan" is the reference documentation for these
-- operations; only the differences are repeated here.
--
-- The caveats of the lazy module apply unchanged. In particular a 'Chan' is
-- /unbounded/: a producer that outruns its consumer makes the channel grow
-- without limit. The @stm@ package offers channels with a different
-- concurrency model, and @TBQueue@ in particular is bounded.
--
-- Note that WHNF is not deep: writing @Just <thunk>@ to a
-- @'Chan' (Maybe a)@ still writes a thunk inside the @Just@.
module Control.Concurrent.Chan.Strict
  ( Chan

    -- * Operations
  , newChan
  , writeChan
  , readChan
  , dupChan
  , getChanContents
  , writeList2Chan
  ) where

import Data.Foldable (mapM_)
import Data.Functor
import GHC.Base
import qualified Control.Concurrent.Chan as Lazy

-- | An unbounded FIFO channel, wrapping @Chan@ from "Control.Concurrent.Chan".
--
-- The values it holds are always in WHNF: every operation of this module forces
-- a value before it is written.
newtype Chan a = Chan (Lazy.Chan a)
  deriving Eq

-- | Build and return a new instance of 'Chan'.
--
-- See @newChan@ in "Control.Concurrent.Chan" for the full semantics.
newChan :: IO (Chan a)
newChan = Chan <$> Lazy.newChan

-- | Write a value to a 'Chan'.
--
-- See @writeChan@ in "Control.Concurrent.Chan" for the full semantics.
--
-- The value is forced to WHNF before it is written.
writeChan :: Chan a -> a -> IO ()
writeChan (Chan chan) a = Lazy.writeChan chan =<< (pure $! a)

-- | Read the next value from the 'Chan'. Blocks when the channel is empty.
--
-- See @readChan@ in "Control.Concurrent.Chan" for the full semantics, in
-- particular for the fairness guarantees and for the
-- 'Control.Exception.BlockedIndefinitelyOnMVar' exception.
readChan :: Chan a -> IO a
readChan (Chan chan) = Lazy.readChan chan

-- | Duplicate a 'Chan'.
--
-- See @dupChan@ in "Control.Concurrent.Chan" for the full semantics.
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan chan) = Chan <$> Lazy.dupChan chan

-- | Return a lazy list representing the contents of the supplied 'Chan'.
--
-- See @getChanContents@ in "Control.Concurrent.Chan" for the full semantics.
getChanContents :: Chan a -> IO [a]
getChanContents (Chan chan) = Lazy.getChanContents chan

-- | Write an entire list of items to a 'Chan'.
--
-- See @writeList2Chan@ in "Control.Concurrent.Chan" for the full semantics.
--
-- Evaluates the list elements to WHNF.
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan = mapM_ . writeChan

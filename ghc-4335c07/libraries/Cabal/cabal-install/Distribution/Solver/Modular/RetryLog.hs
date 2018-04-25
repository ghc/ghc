{-# LANGUAGE Rank2Types #-}
module Distribution.Solver.Modular.RetryLog
    ( RetryLog
    , toProgress
    , fromProgress
    , mapFailure
    , retry
    , failWith
    , succeedWith
    , continueWith
    , tryWith
    ) where

import Distribution.Solver.Modular.Message
import Distribution.Solver.Types.Progress

-- | 'Progress' as a difference list that allows efficient appends at failures.
newtype RetryLog step fail done = RetryLog {
    unRetryLog :: forall fail2 . (fail -> Progress step fail2 done)
               -> Progress step fail2 done
  }

-- | /O(1)/. Convert a 'RetryLog' to a 'Progress'.
toProgress :: RetryLog step fail done -> Progress step fail done
toProgress (RetryLog f) = f Fail

-- | /O(N)/. Convert a 'Progress' to a 'RetryLog'.
fromProgress :: Progress step fail done -> RetryLog step fail done
fromProgress l = RetryLog $ \f -> go f l
  where
    go :: (fail1 -> Progress step fail2 done)
       -> Progress step fail1 done
       -> Progress step fail2 done
    go _ (Done d) = Done d
    go f (Fail failure) = f failure
    go f (Step m ms) = Step m (go f ms)

-- | /O(1)/. Apply a function to the failure value in a log.
mapFailure :: (fail1 -> fail2)
           -> RetryLog step fail1 done
           -> RetryLog step fail2 done
mapFailure f l = retry l $ \failure -> RetryLog $ \g -> g (f failure)

-- | /O(1)/. If the first log leads to failure, continue with the second.
retry :: RetryLog step fail1 done
      -> (fail1 -> RetryLog step fail2 done)
      -> RetryLog step fail2 done
retry (RetryLog f) g =
    RetryLog $ \extendLog -> f $ \failure -> unRetryLog (g failure) extendLog

-- | /O(1)/. Create a log with one message before a failure.
failWith :: step -> fail -> RetryLog step fail done
failWith m failure = RetryLog $ \f -> Step m (f failure)

-- | /O(1)/. Create a log with one message before a success.
succeedWith :: step -> done -> RetryLog step fail done
succeedWith m d = RetryLog $ const $ Step m (Done d)

-- | /O(1)/. Prepend a message to a log.
continueWith :: step
             -> RetryLog step fail done
             -> RetryLog step fail done
continueWith m (RetryLog f) = RetryLog $ Step m . f

-- | /O(1)/. Prepend the given message and 'Enter' to the log, and insert
-- 'Leave' before the failure if the log fails.
tryWith :: Message -> RetryLog Message fail done -> RetryLog Message fail done
tryWith m f =
  RetryLog $ Step m . Step Enter . unRetryLog (retry f (failWith Leave))

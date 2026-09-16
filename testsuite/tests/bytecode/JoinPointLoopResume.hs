-- An async exception delivered to a thread suspended at a loop's safepoint.
--
-- 'timeout' kills the thread while it is evaluating the thunk, which -- since
-- the loop of LoopSlow.hs leaves its BCO only at the safepoint -- is where
-- that thread is. 'raiseAsync' then captures the stack above the thunk's
-- update frame, resume frame included, into an AP_STACK, and forcing the
-- thunk again re-enters the resume frame through its entry code:
-- stg_resume_interp hands the stack to the interpreter, which dispatches on
-- the frame and continues the loop where it stopped. Both the timeout landing
-- and the value are the same with the flag off (JoinPointLoopResumeOff), so
-- nothing printed here depends on when the interrupt arrived.
{-# OPTIONS_GHC -fno-full-laziness #-}
module Main (main) where

import Control.Exception (evaluate)
import System.Timeout (timeout)

import LoopSlow (grow)

main :: IO ()
main = do
  x <- interrupted 20
  print x

-- | A thunk whose evaluation was interrupted, if we could interrupt one in
-- the given number of attempts. Each attempt is its own thunk, so a 'timeout'
-- that loses the race leaves nothing behind; the value is the same either way.
interrupted :: Int -> IO Int
interrupted n = do
  let x = grow 4000000
  r <- timeout 2000 (evaluate x)
  case r of
    Nothing -> return x
    Just _ | n > 0     -> interrupted (n - 1)
           | otherwise -> return x

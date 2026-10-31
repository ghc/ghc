{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
-- Test for deadlocks related to string literal CAFs
--
-- Thread A enters a shared thunk X, and from X a string literal whose stack
-- check fails while another thread B owns X. threadPaused then suspends A's
-- work on X. If A still owns the literal, B blocks on it forever (#27272).
module Main (main) where

import T27272_Lits (asciiLits, utf8Lits)

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad
import Data.Char (ord)
import Data.IORef
import GHC.Conc (BlockReason (..), ThreadStatus (..), threadStatus)
import GHC.Exts
import GHC.IO (IO (..))
import System.Exit
import System.Timeout
#if defined(SELF_CHECK)
import GHC.Exts.Heap
  (ClosureType (..), GenClosure (..), StgInfoTable (..), getClosureData)
#endif

-- | Per literal: the gate (0 until A is in X, 1 once it is, 2 once B is in X
-- too, 3 once B has claimed X), A's thread id, and for the self check whether
-- A was suspended at the literal's stack check.
data Gate = Gate (MutableByteArray# RealWorld) (MVar ThreadId) (IORef Bool)

newGate :: IO Gate
newGate = do
  aVar <- newEmptyMVar
  suspended <- newIORef False
  IO $ \s -> case newByteArray# 8# s of
    (# s1, g #) -> case writeIntArray# g 0# 0# s1 of
      s2 -> (# s2, Gate g aVar suspended #)

readGate :: Gate -> IO Int
readGate (Gate g _ _) = IO $ \s -> case atomicReadIntArray# g 0# s of
  (# s', n #) -> (# s', I# n #)

data Box = Box String

-- | Force the boxed string under @n@ one-word return frames. 'lazy' hides
-- the strictness in @s@, otherwise the string is evaluated before the call.
-- The frames are only one word with -O (see all.T).
deepFrames :: Int -> Box -> Int
deepFrames 0 (Box s) = case lazy s of
  c : _ -> ord c
  []    -> -1
deepFrames n b = 1 + deepFrames (n - 1) b
{-# NOINLINE deepFrames #-}

-- | Build the shared thunk X for a literal and return it unevaluated.
-- A's path through X only uses inline primops and does not allocate: any
-- return to the scheduler would claim X for A. The spin is bounded because it
-- holds up GC syncs.
mkShared :: Gate -> String -> IO String
mkShared gate@(Gate g _ _) lit = return x
  where
    x = runRW# (\s0 -> case fetchAddIntArray# g 0# 1# s0 of
      (# s1, 0# #) -> case spin 100000000# s1 of (# _, () #) -> lit
      (# s1, _ #) -> case claimX gate lit of
        IO m -> case m s1 of (# _, r #) -> r)
    spin :: Int# -> State# RealWorld -> (# State# RealWorld, () #)
    spin 0# s = (# s, () #)
    spin n s = case atomicReadIntArray# g 0# s of
      (# s', 3# #) -> (# s', () #)
      (# s', _ #) -> spin (n -# 1#) s'
{-# NOINLINE mkShared #-}

-- | B's part of the gate. The yield calls threadPaused, which claims X for B,
-- and B only enters the literal once A is blocked (suspended) or done.
-- It is OPAQUE and returns the literal, so X saves nothing across the call:
-- X's stack check stays small enough for the literal's check to fail.
claimX :: Gate -> String -> IO String
claimX (Gate g aVar suspended) lit = do
  yield
  IO $ \s -> (# atomicWriteIntArray# g 0# 3# s, () #)
  a <- readMVar aVar
  let waitA = do
        st <- threadStatus a
        case st of
          ThreadRunning -> yield >> waitA
          ThreadBlocked BlockedOnBlackHole ->
            writeIORef suspended =<< suspendedAtCheck lit
          _ -> return ()
  waitA
  return lit
{-# OPAQUE claimX #-}

-- Self check of the test, enabled with -DSELF_CHECK -package ghc-heap: fail
-- if no literal entry was suspended at the literal's own stack check, i.e. if
-- the test did not exercise the bug. The literal is then still unevaluated.
suspendedAtCheck :: String -> IO Bool
selfCheck :: [Bool] -> IO ()
#if defined(SELF_CHECK)
suspendedAtCheck lit = do
  c <- getClosureData lit
  return $ case c of
    ThunkClosure { info = i } -> tipe i == THUNK_STATIC
    _ -> False
selfCheck results =
  unless (or results) $ failWith "no literal entry was suspended"
#else
suspendedAtCheck _ = return False
selfCheck _ = return ()
#endif

failWith :: String -> IO a
failWith msg = putStrLn msg >> exitFailure

main :: IO ()
main = do
  let expect c fmt = [ (c, c : shows i fmt) | i <- [0 :: Int ..] ]
      lits = zip3 [0 ..] asciiLits (expect 'A' "-ABCDEFGHIJKLMNOPQRSTUVWXYZ") ++
             zip3 [0 ..] utf8Lits (expect 'U' "-λμν-ÄÖÜ-€")
  -- Literal d is entered under d frames. With the stack sizes from all.T this
  -- crosses stack chunk ends, so the literal's stack check fails for some d.
  results <- forM lits $ \(d, lit, (c, e)) -> do
    gate@(Gate _ aVar suspended) <- newGate
    x <- mkShared gate lit
    doneA <- newEmptyMVar
    doneB <- newEmptyMVar
    -- New threads for each literal, so that A's stack only depends on d. A and
    -- B have their own capabilities (-N3), so that B runs while A spins.
    a <- forkOn 1 $ do
      v <- evaluate (deepFrames d (Box x))
      putMVar doneA (v == d + ord c && x == e)
    putMVar aVar a
    _ <- forkOn 2 $ do
      -- B must only enter X once A is in it
      let waitA = do
            n <- readGate gate
            when (n == 0) (yield >> waitA)
      waitA
      _ <- evaluate x
      putMVar doneB ()
    -- With the bug, A blocks on X and B on the literal: the timeout catches it
    r <- timeout 10000000 (takeMVar doneA <* takeMVar doneB)
    case r of
      Just True -> readIORef suspended
      Just False -> failWith ("wrong result at depth " ++ show d)
      Nothing -> failWith ("deadlock at depth " ++ show d)
  selfCheck results

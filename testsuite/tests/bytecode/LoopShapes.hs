{-# LANGUAGE BangPatterns #-}
-- Join points compiled as loops: the shapes that exercise the parts of
-- 'schemeJoinPointLoop' and 'schemeEntryLoop' the counter controls cannot
-- reach, since those only read verdicts and never run anything.
--
--   count   the plain loop, enough iterations that safepoints fire
--   alloc   a loop that allocates; as written, though, its accumulator is a
--           boxed list and its counter is used boxed too, so Core keeps the
--           parameter boxed and the join point is rejected-recursive (see
--           JoinPointLoopsCount.stdout: 2 loops, 2 rejected-recursive). It
--           is kept as a control: the same program, flag on and off, must
--           agree with native code. The allocating loop that does reach
--           the slow path is LoopSlow.hs.
--   wide    many free variables and many parameters, so the fallback's
--           prologue copies a large frame: this is what would overflow a
--           stack check that did not count the copies
--   mixed   parameters and free variables of several representations
--           (boxed, Int#, Double#), so a wrong size anywhere in the frame
--           rebuild shows up as a wrong number rather than a crash; like
--           alloc it is rejected-recursive today (a boxed parameter used
--           boxed), and stays as a control.
--
-- The ghci way of the testsuite puts its own -O0 after the test's -O; this
-- pragma is applied per module, after the command line, so the loops are
-- compiled at -O in both ghci ways.
{-# OPTIONS_GHC -O #-}
module LoopShapes (count, alloc, wide, mixed) where

count :: Int -> Int
count n0 = go n0 0
  where
    go :: Int -> Int -> Int
    go 0 !a = a + n0
    go k !a = go (k - 1) (a + k)
{-# NOINLINE count #-}

alloc :: Int -> Int
alloc n0 = go n0 []
  where
    go :: Int -> [Int] -> Int
    go 0 xs = length xs + n0
    go k xs = go (k - 1) (if k `rem` 64 == 0 then k : xs else xs)
{-# NOINLINE alloc #-}

wide :: Int -> Int -> Int -> Int -> Int -> Int -> Int
wide a0 b0 c0 d0 e0 n0 = go n0 0 0 0 0 0 0
  where
    go :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
    go 0 !p !q !r !s !t !u = p + q + r + s + t + u + a0 + b0 + c0 + d0 + e0
    go k !p !q !r !s !t !u =
      go (k - 1) (p + a0) (q + b0) (r + c0) (s + d0) (t + e0) (u + k)
{-# NOINLINE wide #-}

mixed :: Int -> Double -> Int -> (Int, Double)
mixed n0 x0 m0 = go n0 x0 0 0
  where
    go :: Int -> Double -> Int -> Double -> (Int, Double)
    go 0 !x !i !acc = (i + m0, x + acc)
    go k !x !i !acc =
      go (k - 1) (x + 0.5) (i + m0) (acc + x * fromIntegral k)
{-# NOINLINE mixed #-}

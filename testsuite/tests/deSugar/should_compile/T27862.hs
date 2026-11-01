{-# LANGUAGE BangPatterns #-}
module T27862 (allBangs, firstBang, lastLazy) where

data Axis = Axis !Int !Int

-- The axes of extent other than 1, their strides made positive, and
-- the offset moved by each negative stride. The three functions are
-- the same, except for the bangs on 'off'.

{- With GHC 10.0
* allBangs has a bang on all three equations.
  The flag reports the bangs on equations 2 and 3.

* firstBang keeps only the bang that the flag does not report. Its
  worker matches both lists two times: the second match is in a join point fail, and
  the first match jumps to it. In the second match, GHC does not know that n is
  evaluated. Thus it builds the strict-field constructor Axis x n inside case n
  of I#, and in STG this is an updatable thunk, one for each axis with an extent
  other than 1 and a non-negative stride. allBangs allocates no thunk.

* lastLazy removes only the bang on equation 3. Its worker is better than
  the worker of allBangs. allBangs keeps a join point fail _ = (# axes, ww #),
  and its two [] alternatives jump to it. In lastLazy, the two [] alternatives
  return (# axes, ww #) directly.
-}

allBangs :: [Axis] -> Int -> [Int] -> [Int] -> ([Axis], Int)
allBangs axes !off (_ : sts) (1 : ns) = allBangs axes off sts ns
allBangs axes !off (st : sts) (n : ns)
  | st < 0 = allBangs (Axis (negate st) n : axes) (off + (n - 1) * st) sts ns
  | otherwise = allBangs (Axis st n : axes) off sts ns
allBangs axes !off _ _ = (axes, off)

-- Both bangs that the flag reports are removed.
firstBang :: [Axis] -> Int -> [Int] -> [Int] -> ([Axis], Int)
firstBang axes !off (_ : sts) (1 : ns) = firstBang axes off sts ns
firstBang axes off (st : sts) (n : ns)
  | st < 0 = firstBang (Axis (negate st) n : axes) (off + (n - 1) * st) sts ns
  | otherwise = firstBang (Axis st n : axes) off sts ns
firstBang axes off _ _ = (axes, off)

-- Only the last bang that the flag reports is removed.
lastLazy :: [Axis] -> Int -> [Int] -> [Int] -> ([Axis], Int)
lastLazy axes !off (_ : sts) (1 : ns) = lastLazy axes off sts ns
lastLazy axes !off (st : sts) (n : ns)
  | st < 0 = lastLazy (Axis (negate st) n : axes) (off + (n - 1) * st) sts ns
  | otherwise = lastLazy (Axis st n : axes) off sts ns
lastLazy axes off _ _ = (axes, off)

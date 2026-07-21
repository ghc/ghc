{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- Regression test for #27526 and #27123:
-- This is an alternative regression test to T27123 that has the benefit of reproducing
-- the issue even with the non threaded runtime.
-- It has the downside of being LLM generated so the description of its mechanism is
-- a bit rambling as a consequence.
--
-- LLM generated repro case starts below this line
-------------------------------------------------
-- Regression test for the profiling variant of stg_ap_0_fast (rts/Apply.cmm) entering a closure
-- that is no longer a thunk.
--
-- The thunk case of stg_ap_0_fast performs STK_CHK_GEN() -- a GC point --
-- before jumping to the closure's entry code.  A GC there can replace the
-- closure by its value: evacuate() short-circuits a BLACKHOLE whose
-- indirectee is a tagged value, and every updated thunk is such a
-- BLACKHOLE.  If the value is a FUN, the (unfixed) code jumped into the
-- function's entry code with R1 untagged and no arguments set up; profiled
-- FUN entry code loads its CCS at a tag-adjusted offset from R1, so it
-- reads a byte-shifted CCS pointer and enterFunCCS dereferences a
-- non-canonical address: segfault.
--
-- This program arranges that deterministically, single-threaded, no timer:
--
--  * per iteration, h is an updatable thunk whose value is a dynamic
--    arity-2 FUN; 'prime' evaluates it via an unknown call at shallow
--    stack depth, updating it to a BLACKHOLE whose indirectee is the
--    tag-2 FUN;
--  * the BLACKHOLE is carried down an allocation-free descent through
--    tiny stack chunks (+RTS -kc2k -kb256).  The descent's own stack
--    growth never garbage-collects (nothing arms CHECK_GC()), so the
--    BLACKHOLE survives to the bottom;
--  * at the bottom, 'bigAlloc' allocates one large (> the -AL8k large
--    allocation limit) ByteArray#.  That makes CHECK_GC() true, so the
--    next *failing stack check* returns HeapOverflow and garbage-collects
--    right away (stg_gc_noregs in rts/HeapStackCheck.cmm) instead of
--    merely growing the stack;
--  * 'forceIt' then seq's h, compiling to a call of stg_ap_0_fast with
--    the BLACKHOLE as argument.  When the stack check in the thunk case
--    fails right there, the resulting GC short-circuits the BLACKHOLE to
--    the FUN and the bug fires on resume;
--  * the sweep of descent depths and frame-size mixes (chosen per level
--    from the bits of a rotating pattern) makes the distance between the
--    bottom and the chunk's stack limit take every value at word
--    granularity, so the few-word failure window is hit many times;
--  * a performMinorGC per iteration disarms CHECK_GC() again, keeping
--    the next iteration's descent GC-free.
--
-- On an unfixed RTS this segfaults within the first few hundred
-- iterations (observed with GHC 9.10/9.12/9.14 on x86-64); with the fix,
-- stg_ap_0_fast re-dispatches on the closure type and returns the FUN
-- like any other value.
--
-- The test must be compiled with -fprof-late (and not -fprof-auto): an
-- SCC on the local bindings would change the shape of the thunks and the
-- exact stack frames the window depends on.  It is 64-bit only.

module Main (main) where

import GHC.Exts
import System.Environment (getArgs)
import System.Mem (performMinorGC)

data Pair = Pair (Int -> Int -> Int) [Int]

-- Evaluated constructor holding a dynamic arity-2 FUN (captures n).
{-# NOINLINE mkPair #-}
mkPair :: Int -> Pair
mkPair n = Pair (\x y -> x + y + n) [n]

-- Unknown call: evaluates (and thereby updates) the thunk, lazily.
{-# NOINLINE prime #-}
prime :: Bool -> (Int -> Int -> Int) -> Int
prime b f = if b then f 3 4 else 0

-- Statically-unknown eval via stg_ap_0_fast, lazily.
{-# NOINLINE forceIt #-}
forceIt :: Bool -> (Int -> Int -> Int) -> Int
forceIt b h = if b then h `seq` 1 else 0

-- One large allocation, arming CHECK_GC() until the next GC.  The
-- argument varies per call so this can never be floated out and shared.
{-# NOINLINE bigAlloc #-}
bigAlloc :: Int -> Int
bigAlloc (I# k) =
  case runRW# (\s -> case newByteArray# 9000# s of
                       (# s1, mba #) -> getSizeofMutableByteArray# mba s1) of
    (# _, n #) -> I# (n +# k)

-- The bottom of the descent: arm CHECK_GC(), then evaluate the BLACKHOLE
-- via stg_ap_0_fast.  Everything with deeper stack demands than the
-- forceIt/stg_ap_0_fast path itself must stay out of this function, or
-- its stack growth would swallow exactly the margins that reach the
-- window.  pat stays live across forceIt to deepen that frame a little,
-- widening the window.
{-# NOINLINE base #-}
base :: Bool -> Int -> (Int -> Int -> Int) -> Int
base b pat h =
  let !g = bigAlloc pat             -- CHECK_GC() true from here to next GC
      !c = forceIt (g >= 0 && b) h  -- stg_ap_0_fast(BLACKHOLE); the window.
                                    -- c depends on g so the two calls
                                    -- cannot be reordered.
  in g + c + pat

-- Descend to depth d, carrying the BLACKHOLE h.  Each level pushes one of
-- two continuation frames of different sizes, chosen by the bits of pat;
-- across the sweep the popcount jitter covers stack offsets at word
-- granularity where the depth alone only reaches multiples of one frame
-- size.  pat is rotated, not shifted, so the mix keeps varying below the
-- last stack chunk crossing, however deep it happens.  The descent must
-- not allocate, so that no heap check runs between prime and forceIt.
spinA :: Bool -> Int# -> Int# -> (Int -> Int -> Int) -> Int
spinA b pat d h
  | isTrue# (d <=# 0#)                = base b (I# pat) h
  | isTrue# ((pat `andI#` 1#) ==# 0#) = spinA b (rot pat) (d -# 1#) h + 1
  | otherwise                         = spinB b (rot pat) (d -# 1#) h (I# (d *# 17#))

spinB :: Bool -> Int# -> Int# -> (Int -> Int -> Int) -> Int -> Int
spinB b pat d h x = spinA b pat d h + (x `div` 1000003)

rot :: Int# -> Int#
rot w = uncheckedIShiftRL# w 1# `orI#` uncheckedIShiftL# w 63#
{-# INLINE rot #-}

-- One iteration: build and prime the thunk at shallow stack depth, then
-- descend and evaluate it at the bottom; finally disarm CHECK_GC() so
-- the next iteration's descent stays GC-free.
{-# NOINLINE iter #-}
iter :: Bool -> Int -> IO Int
iter b i@(I# i#) = do
  let p = mkPair i
      h = case p of Pair f _ -> f   -- updatable thunk, value = tag-2 FUN
      !a = prime b h                -- h := BLACKHOLE -> FUN (tag 2)
      !r = spinA (a >= 0 && b)      -- depends on a: descend only after
           i# (150# +# (i# `andI#` 511#)) h    -- priming
  performMinorGC
  pure (a + r)

main :: IO ()
main = do
  args <- getArgs
  let iters = case args of
        a:_ -> read a
        _   -> 20000
      go !acc i
        | i >= iters = pure acc
        | otherwise  = do
            v <- iter (iters > 0) i
            go (acc + v) (i + 1)
  r <- go 0 0
  print r

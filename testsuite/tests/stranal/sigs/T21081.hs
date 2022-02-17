{-# LANGUAGE BangPatterns #-}

module T21081 where

-- | Should put demand `MP(SL,SL)` or `MP(1L,1L)` on `pr`, telling us that `f`
-- will evaluate both components of `pr` whenever it evaluates `pr` lazily.
f :: (Bool, Bool) -> (Bool, Bool)
f pr = (case pr of (a,b) -> a /= b, True)
{-# NOINLINE f #-}
-- | If `f` gets the correct signature, we can case-bind `z` here (not tested)
g :: Int -> (Bool, Bool)
g x = let y = let z = odd x in (z,z) in f y

-- | Should put demand `LCS(C1(L))` on `f`, telling us that whenever `myfoldl`
-- evaluates `f`, it will also call it at least once (`S`) and then always call
-- it with a second argument (`1`).
-- This in turn allows us to eta-reduce `(\a b -> f a b)` to `f` (not tested,
-- but there's T20040 which tests an even more complicated case).
myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f z [] = z
myfoldl f !z (x:xs) = myfoldl (\a b -> f a b) (f z x) xs

-- | Should put demand `LCL(C1(L))` on `f`
blah :: (Int -> Int -> Int) -> Int -> Int
blah f 0 = 0
blah f 1 = f `seq` 1
blah f x = f (x+1) (x+2) + f (x+3) (x+4)
{-# NOINLINE blah #-}
-- | It's not safe to eta-reduce the lambda here, because `do_blah undefined`
-- would crash.
do_blah :: (Int -> Int -> Int) -> Int
do_blah f = blah (\a b -> f a b) 1

-- | Should put demand `MP(ML,ML)` on `p`, not `MP(L,L)`.
h :: (Int, Int) -> Int -> Int
h p 0 = 0
h p 1 = fst p
h p y = snd p + y
{-# NOINLINE h #-}
-- | We want to use call-by-name for `a` and `b`, justified by the used-once
-- info on `p` in `h`.
blurg :: Int -> Int
blurg x =
  let a = sum [0..x]
      b = sum [1..x]
  in h (a,b) x

-- | But we still need `p` to have demand `MP(L,L)` or simply `L` here.
-- NOT `MP(ML,ML)`. This demonstrates that product usage demands stay absolute.
h2 :: (Int, Int) -> Int -> Int
h2 p y = h p y + h p (y+1)
{-# NOINLINE h2 #-}
-- | Otherwise we'd use call-by-name for `a` and `b` here, although they are
-- evaluated multiple times in `h2`.
blurg2 :: Int -> Int
blurg2 x =
  let a = sum [0..x]
      b = sum [1..x]
  in h2 (a,b) x

-- | Must not put demand `MP(1L,1L)` on `x` (e.g., strict in the components).
-- This demonstrates that `plusDmd` must fall back to `lubSubDemand` when both
-- Demands are lazy.
i :: Bool -> Bool -> (Int, Int) -> Int
i b b' x = (if b then fst x else 3) + (if b' then snd x else 4)

fst' :: (a,b) -> a
fst' (x,_) = x
{-# NOINLINE fst' #-}

snd' :: (a,b) -> b
snd' (_,x) = x
{-# NOINLINE snd' #-}

-- | We want `SP(1L,1L)`, even if neither `fst'` nor `snd'` are strict in both
-- components. This dictates that `plusDmd` has to do `plusSubDemand` when both
-- Demands are strict. Which differs in a crucial way from the situation in `i`!
j :: (Integer, Integer) -> Integer
j p = fst' p + snd' p

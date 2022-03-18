{-# LANGUAGE MagicHash #-}

module T21257 (g, h, g2, notBot, notBot2, absent, absentUnlifted) where

import GHC.Exts

f :: (Bool, Bool) -> (Bool, Bool)
f pr = (pr `seq` True, case pr of (a,b) -> a && b)
{-# NOINLINE f #-}

-- The demand signature of `g` should say `LP(ML,ML)`, e.g., that the pair
-- components are used at most once.
g :: (Bool, Bool) -> ()
g pr = f pr `seq` ()

-- The same, but it let-binds the pair.
g2 :: Bool -> Bool -> ()
g2 x y = let pr = (x,y); {-# NOINLINE pr #-} in
         f pr `seq` ()

-- | Key point: Unlike for `seq`, repeated apps of `seq'` can't be simplified
-- away.
seq' :: a -> b -> b
seq' = seq
{-# NOINLINE seq' #-}

f2 :: (Bool, Bool) -> ()
f2 pr = pr `seq'` fst pr `seq'` ()
{-# NOINLINE f2 #-}

-- The demand signature of `h` should say `SP(1L,A)` (prior to worker/wrapper),
-- e.g., that the pair components are used at most once.
h :: (Bool, Bool) -> ()
h pr = f2 pr `seq` ()

---------------------------------------------------------------------------
-- The following tests are proof that in the trivial case of             --
-- `dmdAnalStar _ (n :* sd)`, we have to consider the lower bound in `n` --
-- and lazify accordingly, as well as consider whether the upper bound   --
-- is 0                                                                  --
---------------------------------------------------------------------------

data T = T T

-- This one calls an `h` with signature `<L><L>`.
-- Although `f` has signature `{x:->S}b`, that must not become the demand type
-- of `notBot`!
-- `notBot` must have `<L><...>`.
notBot :: T -> (T -> T -> T) -> T
notBot x h = h f f -- h :: <L><L>
  where
    f :: T
    f = x `seq` f

-- This one calls an `h` with signature `<A><A>`.
-- `notBot2` must have a signature that is lazy in `x`, such as `<A>`.
notBot2 :: T -> T
notBot2 x = h f f
  where
    f :: T
    f = x `seq` f
    h :: T -> T -> T
    h x y = T (h x y) -- h :: <A><A>

-- This one calls an `h` with signature `<A><A>`.
-- The `f` here is not bottoming on every code path, but still strict in `x`.
-- Yet the call to `h` will not actually force it, so the whole function
-- should be absent in `x`.
absent :: Bool -> T
absent x = h f f
  where
    f :: T
    f = case x of True -> T undefined; False -> f
    h :: T -> T -> T
    h x y = T (h x y) -- h :: <A><A>

------------------------------------------------------
-- Test for Note [Call-by-value in demand analysis] --
------------------------------------------------------

-- | We'd like to drop the second argument here
absentUnlifted :: Bool -> Int# -> Int#
absentUnlifted True _ = 0#
absentUnlifted _    n = absentUnlifted False (n -# 1#)

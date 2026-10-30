{-# LANGUAGE NoMonoLocalBinds, MagicHash, UnboxedTuples #-}

-- One of several regression tests arising from the work on #27737.
--
-- The Simplifier returns a single-alternative case as a case-float
-- (FloatCase).  Here the local binding
--     g = /\a. case addIntC# x y of (# r, _ #) -> ([], I# r)
-- is lazy, non-recursive and has a type lambda but no value lambda, so
-- simplLazyBind strips off the /\a and simplifies the body.  The case
-- becomes a FloatCase; since addIntC# is ok-for-speculation, the float is
-- FltOkSpec, which doFloatFromRhs allows out of a nested lazy binding.
-- But then the floats must be abstracted over `a` (abstractFloats), which
-- can only handle let-floats: at one stage the compiler panicked
-- with "letFloatBinds".

module T27737a where

import GHC.Exts

f :: Int -> Int -> ([Bool], [Char], Int)
f (I# x) (I# y) = let g = case addIntC# x y of (# r, _ #) -> ([], I# r)
                          -- g :: forall a. ([a], Int)
                  in (fst g, fst g, snd g)

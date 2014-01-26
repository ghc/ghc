{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Magic
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC magic.
--
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Magic ( inline, lazy, oneShot ) where

-- | The call @inline f@ arranges that 'f' is inlined, regardless of
-- its size. More precisely, the call @inline f@ rewrites to the
-- right-hand side of @f@'s definition. This allows the programmer to
-- control inlining from a particular call site rather than the
-- definition site of the function (c.f. 'INLINE' pragmas).
--
-- This inlining occurs regardless of the argument to the call or the
-- size of @f@'s definition; it is unconditional. The main caveat is
-- that @f@'s definition must be visible to the compiler; it is
-- therefore recommended to mark the function with an 'INLINABLE'
-- pragma at its definition so that GHC guarantees to record its
-- unfolding regardless of size.
--
-- If no inlining takes place, the 'inline' function expands to the
-- identity function in Phase zero, so its use imposes no overhead.
{-# NOINLINE[0] inline #-}
inline :: a -> a
inline x = x

-- | The 'lazy' function restrains strictness analysis a little. The
-- call @lazy e@ means the same as 'e', but 'lazy' has a magical
-- property so far as strictness analysis is concerned: it is lazy in
-- its first argument, even though its semantics is strict. After
-- strictness analysis has run, calls to 'lazy' are inlined to be the
-- identity function.
--
-- This behaviour is occasionally useful when controlling evaluation
-- order. Notably, 'lazy' is used in the library definition of
-- 'Control.Parallel.par':
--
-- > par :: a -> b -> b
-- > par x y = case (par# x) of _ -> lazy y
--
-- If 'lazy' were not lazy, 'par' would look strict in 'y' which
-- would defeat the whole purpose of 'par'.
--
-- Like 'seq', the argument of 'lazy' can have an unboxed type.
lazy :: a -> a
lazy x = x
-- Implementation note: its strictness and unfolding are over-ridden
-- by the definition in MkId.lhs; in both cases to nothing at all.
-- That way, 'lazy' does not get inlined, and the strictness analyser
-- sees it as lazy.  Then the worker/wrapper phase inlines it.
-- Result: happiness


-- | The 'oneShot' function can be used to give a hint to the compiler that its
-- argument will be called at most once, which may (or may not) enable certain
-- optimizations. It can be useful to improve the performance of code in continuation
-- passing style.
oneShot :: (a -> b) -> (a -> b)
oneShot f = f
-- Implementation note: This is wired in in MkId.lhs, so the code here is
-- mostly there to have a place for the documentation.

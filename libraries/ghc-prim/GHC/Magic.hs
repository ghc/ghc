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

module GHC.Magic ( inline ) where

-- | The call '(inline f)' arranges that 'f' is inlined, regardless of
-- its size. More precisely, the call '(inline f)' rewrites to the
-- right-hand side of 'f'\'s definition. This allows the programmer to
-- control inlining from a particular call site rather than the
-- definition site of the function (c.f. 'INLINE' pragmas).
--
-- This inlining occurs regardless of the argument to the call or the
-- size of 'f'\'s definition; it is unconditional. The main caveat is
-- that 'f'\'s definition must be visible to the compiler; it is
-- therefore recommended to mark the function with an 'INLINABLE'
-- pragma at its definition so that GHC guarantees to record its
-- unfolding regardless of size.
--
-- If no inlining takes place, the 'inline' function expands to the
-- identity function in Phase zero, so its use imposes no overhead.
{-# NOINLINE[0] inline #-}
inline :: a -> a
inline x = x


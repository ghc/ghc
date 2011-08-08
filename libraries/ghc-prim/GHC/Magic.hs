{-# LANGUAGE Trustworthy #-}
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
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module GHC.Magic (inline) where

-- | The call '(inline f)' reduces to 'f', but 'inline' has a BuiltInRule
-- that tries to inline 'f' (if it has an unfolding) unconditionally
-- The 'NOINLINE' pragma arranges that inline only gets inlined (and
-- hence eliminated) late in compilation, after the rule has had
-- a good chance to fire.
inline :: a -> a
{-# NOINLINE[0] inline #-}
inline x = x


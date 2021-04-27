{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Magic.Dict
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Defines the 'withDict' function. For more information, see
-- @Note [withDict]@ in "GHC.HsToCore.Expr" in GHC.
-- The definition of 'withDict' is located in a separate module from
-- "GHC.Magic" because 'withDict' is @Unsafe@ (it threatens type class
-- coherence) while "GHC.Magic" is @Trustworthy@.
--
-- Use "GHC.Exts" from the @base@ package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Magic.Dict (withDict) where

import GHC.Prim.Panic (panicError)
import GHC.Types (RuntimeRep, TYPE)

-- | 'withDict' is a special-purpose placeholder value.
-- It is used internally by modules such as "GHC.TypeNats" to cast a typeclass
-- dictionary with a single method. It is eliminated in the desugarer during
-- compilation. For the details, see @Note [withDict]@ in "GHC.HsToCore.Expr"
-- in GHC.
withDict :: forall {rr :: RuntimeRep} st dt (r :: TYPE rr). st -> (dt => r) -> r
{-# NOINLINE withDict #-}
withDict = panicError "Non-rewritten withDict"#

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- @Note [withDict]@ in "GHC.Tc.Instance.Class" in GHC.
-- The definition of 'withDict' is located in a separate module from
-- "GHC.Magic" because 'withDict' is @Unsafe@ (it threatens type class
-- coherence) while "GHC.Magic" is @Trustworthy@.
--
-- Use "GHC.Exts" from the @base@ package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Magic.Dict (WithDict(..)) where

import GHC.Types (RuntimeRep, TYPE)

-- | @'withDict' d f@ provides a way to call a type-class–overloaded function
-- @f@ by applying it to the supplied dictionary @d@.
--
-- 'withDict' can only be used if the type class has a single method with no
-- superclasses. For more (important) details on how this works, see
-- @Note [withDict]@ in "GHC.Tc.Instance.Class" in GHC.
class WithDict cls meth where
  withDict :: forall {rr :: RuntimeRep} (r :: TYPE rr). meth -> (cls => r) -> r

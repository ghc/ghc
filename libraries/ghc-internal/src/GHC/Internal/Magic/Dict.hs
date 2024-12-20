{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}  -- See Note [withDict has an ambiguous type]
{-# LANGUAGE Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Magic.Dict
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  see libraries/ghc-internal/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Defines the 'withDict' function. For more information, see
-- @Note [withDict]@ in "GHC.Tc.Instance.Class" in GHC.
-- The definition of 'withDict' is located in a separate module from
-- "GHC.Internal.Magic" because 'withDict' is @Unsafe@ (it threatens type class
-- coherence) while "GHC.Internal.Magic" is @Trustworthy@.
--
-- Use "GHC.Exts" from the @base@ package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Internal.Magic.Dict (
    WithDict( withDict )
  ) where

import GHC.Internal.Types (RuntimeRep, TYPE)

-- | The constraint @'WithDict' cls meth@ can be solved when evidence for
-- the constraint @cls@ can be provided in the form of a dictionary of
-- type @meth@. This requires @cls@ to be a class constraint whose single
-- method has type @meth@.
--
-- For more (important) details on how this works, see
-- @Note [withDict]@ in "GHC.Tc.Instance.Class" in GHC.
--
--   @since 0.9.0
class WithDict cls meth where
  -- @'withDict' d f@ provides a way to call a type-class–overloaded function
  -- @f@ by applying it to the supplied dictionary @d@.
  --
  -- 'withDict' can only be used if the type class has a single method with no
  -- superclasses.
  --
  --   @since 0.9.0
  withDict :: forall {rr :: RuntimeRep} (r :: TYPE rr). meth -> (cls => r) -> r

{- Note [withDict has an ambiguous type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type of `withDict` is ambiguous.  Consider
   foo :: forall cls meth. WithDict cls meth
       => forall rr (r::rr). meth -> (cls => r) -> r
   foo m k = withDict m k

If we instantiate `withDict` with fresh unification variables, including cls0 for cls,
there is nothing that forces the `cls` Wanted from the call to `k` to unify with the
`cls0` Given from the call to `withDict`.  You have to give it a class argument:

   foo m k = withDict @cls m k

That's fine.  But it means we need -XAllowAmbiguousTypes for the withDict definition,
at least with deep subsumption.
-}

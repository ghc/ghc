{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK print-explicit-runtime-reps #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.PrimOps
-- Copyright   :  Andreas Klebinger 2024
-- License     :  see libraries/ghc-experimental/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC Extensions: This is the Approved Way to get at GHC-specific extensions
-- without relying on the ghc-internal package.
-----------------------------------------------------------------------------

-- See Note [Where do we export PrimOps] for the purpose of this module.

module GHC.PrimOps
       (
        module GHC.Internal.Exts,
       ) where

import GHC.Internal.Exts hiding
  -- hide a few really unstable things
  ( dataToTagSmall#
  , dataToTagLarge#
    -- The dataToTag# primops are deprecated and expected to be
    -- un-exposed in the near future; see #24169 and DTW7 of
    -- Note [DataToTag overview] in GHC.Tc.Instance.Class
  , whereFrom#
  )

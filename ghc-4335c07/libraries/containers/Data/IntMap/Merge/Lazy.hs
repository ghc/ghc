{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
#define USE_MAGIC_PROXY 1
#endif

#if USE_MAGIC_PROXY
{-# LANGUAGE MagicHash #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Merge.Lazy
-- Copyright   :  (c) wren romano 2016
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This module defines an API for writing functions that merge two
-- maps. The key functions are 'merge' and 'mergeA'.
-- Each of these can be used with several different \"merge tactics\".
--
-- The 'merge' and 'mergeA' functions are shared by
-- the lazy and strict modules. Only the choice of merge tactics
-- determines strictness. If you use 'Data.Map.Merge.Strict.mapMissing'
-- from "Data.Map.Merge.Strict" then the results will be forced before
-- they are inserted. If you use 'Data.Map.Merge.Lazy.mapMissing' from
-- this module then they will not.
--
-- == Efficiency note
--
-- The 'Category', 'Applicative', and 'Monad' instances for 'WhenMissing'
-- tactics are included because they are valid. However, they are
-- inefficient in many cases and should usually be avoided. The instances
-- for 'WhenMatched' tactics should not pose any major efficiency problems.

module Data.IntMap.Merge.Lazy (
    -- ** Simple merge tactic types
      SimpleWhenMissing
    , SimpleWhenMatched

    -- ** General combining function
    , merge

    -- *** @WhenMatched@ tactics
    , zipWithMaybeMatched
    , zipWithMatched

    -- *** @WhenMissing@ tactics
    , mapMaybeMissing
    , dropMissing
    , preserveMissing
    , mapMissing
    , filterMissing

    -- ** Applicative merge tactic types
    , WhenMissing
    , WhenMatched

    -- ** Applicative general combining function
    , mergeA

    -- *** @WhenMatched@ tactics
    -- | The tactics described for 'merge' work for
    -- 'mergeA' as well. Furthermore, the following
    -- are available.
    , zipWithMaybeAMatched
    , zipWithAMatched

    -- *** @WhenMissing@ tactics
    -- | The tactics described for 'merge' work for
    -- 'mergeA' as well. Furthermore, the following
    -- are available.
    , traverseMaybeMissing
    , traverseMissing
    , filterAMissing

    -- *** Covariant maps for tactics
    , mapWhenMissing
    , mapWhenMatched

    -- *** Contravariant maps for tactics
    , lmapWhenMissing
    , contramapFirstWhenMatched
    , contramapSecondWhenMatched

    -- *** Miscellaneous tactic functions
    , runWhenMatched
    , runWhenMissing
    ) where

import Data.IntMap.Internal

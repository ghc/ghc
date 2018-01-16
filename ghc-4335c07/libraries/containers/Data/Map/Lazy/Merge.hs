{-# LANGUAGE CPP #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Lazy.Merge
-- Copyright   :  (c) David Feuer 2016
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This module defines an API for writing functions that merge two
-- maps. The key functions are 'merge' and 'mergeA'.
-- Each of these can be used with several different "merge tactics".
--
-- The 'merge' and 'mergeA' functions are shared by
-- the lazy and strict modules. Only the choice of merge tactics
-- determines strictness. If you use 'Data.Map.Strict.Merge.mapMissing'
-- from "Data.Map.Strict.Merge" then the results will be forced before
-- they are inserted. If you use 'Data.Map.Lazy.Merge.mapMissing' from
-- this module then they will not.
--
-- == Efficiency note
--
-- The 'Category', 'Applicative', and 'Monad' instances for 'WhenMissing'
-- tactics are included because they are valid. However, they are
-- inefficient in many cases and should usually be avoided. The instances
-- for 'WhenMatched' tactics should not pose any major efficiency problems.

module Data.Map.Lazy.Merge {-# DEPRECATED "Use \"Data.Map.Merge.Lazy\"." #-}
    ( module Data.Map.Merge.Lazy ) where

import Data.Map.Merge.Lazy

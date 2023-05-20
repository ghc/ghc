{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Maybe
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Maybe type, and associated operations.
--
-----------------------------------------------------------------------------

module Data.Maybe
   (
     Maybe(Nothing,Just)

   , maybe

   , isJust
   , isNothing
   , fromJust
   , fromMaybe
   , listToMaybe
   , maybeToList
   , catMaybes
   , mapMaybe
   ) where

import GHC.Err
import GHC.Maybe
import GHC.Stack.Types ( HasCallStack )

-- $setup
-- Allow the use of some Prelude functions in doctests.
-- >>> import Prelude

-- ---------------------------------------------------------------------------
-- Functions over Maybe

-- | The 'fromJust' function extracts the element out of a 'Just' and
-- throws an error if its argument is 'Nothing'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> fromJust (Just 1)
-- 1
--
-- >>> 2 * (fromJust (Just 10))
-- 20
--
-- >>> 2 * (fromJust Nothing)
-- *** Exception: Maybe.fromJust: Nothing
-- ...
--
-- WARNING: This function is partial. You can use case-matching instead.
fromJust          :: HasCallStack => Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing" -- yuck
fromJust (Just x) = x


{-# LANGUAGE Safe #-}

-- |
--
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

module Data.Maybe
    (Maybe(Nothing, Just),
     maybe,
     isJust,
     isNothing,
     fromJust,
     fromMaybe,
     listToMaybe,
     maybeToList,
     catMaybes,
     mapMaybe
     ) where

import GHC.Internal.Data.Maybe
{-# LANGUAGE Safe #-}

-- |
--
-- Copyright   :  (C) 2008-2014 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A logically uninhabited data type, used to indicate that a given
-- term should not exist.
--
-- @since 4.8.0.0

module Data.Void
    (Void,
     absurd,
     vacuous
     ) where

import GHC.Internal.Data.Void
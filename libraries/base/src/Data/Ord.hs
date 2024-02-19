{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Ord
-- Copyright   :  (c) The University of Glasgow 2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Orderings
--

module Data.Ord
    (Ord(..),
     Ordering(..),
     Down(..),
     comparing,
     clamp
     ) where

import GHC.Internal.Data.Ord
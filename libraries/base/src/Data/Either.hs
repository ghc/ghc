{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Either
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Either type, and associated operations.
--

module Data.Either
    (Either(..),
     either,
     lefts,
     rights,
     isLeft,
     isRight,
     fromLeft,
     fromRight,
     partitionEithers
     ) where

import GHC.Internal.Data.Either
{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Functions associated with the tuple data types.
--

module Data.Tuple
    (Solo(..),
     getSolo,
     fst,
     snd,
     curry,
     uncurry,
     swap
     ) where

import GHC.Internal.Data.Tuple
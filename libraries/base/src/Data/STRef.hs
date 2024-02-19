{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.STRef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (uses Control.Monad.ST)
--
-- Mutable references in the (strict) ST monad.
--

module Data.STRef
    (-- *  STRefs
     STRef,
     newSTRef,
     readSTRef,
     writeSTRef,
     modifySTRef,
     modifySTRef'
     ) where

import GHC.Internal.Data.STRef
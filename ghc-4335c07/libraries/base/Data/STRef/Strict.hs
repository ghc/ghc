{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.STRef.Strict
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (uses Control.Monad.ST.Strict)
--
-- Mutable references in the (strict) ST monad (re-export of "Data.STRef")
--
-----------------------------------------------------------------------------

module Data.STRef.Strict (
        module Data.STRef
  ) where

import Data.STRef


{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Eq
-- Copyright   :  (c) The University of Glasgow 2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Equality
--
-----------------------------------------------------------------------------

module Data.Eq (
   Eq(..),
 ) where

#if __GLASGOW_HASKELL__
import GHC.Base
#endif

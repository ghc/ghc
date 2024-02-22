{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.Int
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Signed integer types
--
-----------------------------------------------------------------------------

module GHC.Internal.Data.Int
    (
     -- * Signed integer types
     Int,
     Int8, Int16, Int32, Int64,
     ) where

import GHC.Internal.Base ( Int )
import GHC.Internal.Int  ( Int8, Int16, Int32, Int64 )

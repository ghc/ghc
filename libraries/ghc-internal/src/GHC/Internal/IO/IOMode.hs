{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.IO.IOMode
-- Copyright   :  (c) The University of Glasgow, 1994-2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- The IOMode type
--
-----------------------------------------------------------------------------

module GHC.Internal.IO.IOMode (IOMode(..)) where

import GHC.Internal.Base
import GHC.Internal.Show
import GHC.Internal.Read
import GHC.Internal.Arr
import GHC.Internal.Enum

-- | See 'GHC.Internal.System.IO.openFile'
data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                    deriving ( Eq   -- ^ @since 4.2.0.0
                             , Ord  -- ^ @since 4.2.0.0
                             , Ix   -- ^ @since 4.2.0.0
                             , Enum -- ^ @since 4.2.0.0
                             , Read -- ^ @since 4.2.0.0
                             , Show -- ^ @since 4.2.0.0
                             )


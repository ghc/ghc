{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Numeric.Natural
-- Copyright   :  (C) 2014 Herbert Valerio Riedel,
--                (C) 2011 Edward Kmett
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The arbitrary-precision 'Natural' number type.
--
-- @since base-4.8.0.0
-----------------------------------------------------------------------------

module GHC.Internal.Numeric.Natural
    ( Natural
    , minusNaturalMaybe
    ) where

import GHC.Internal.Bignum.Natural
import GHC.Internal.Natural (minusNaturalMaybe)

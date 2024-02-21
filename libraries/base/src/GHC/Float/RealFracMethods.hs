{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Float.RealFracMethods
-- Copyright   :  (c) Daniel Fischer 2010
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Methods for the RealFrac instances for 'Float' and 'Double',
-- with specialised versions for 'Int'.
--
-- Moved to their own module to not bloat "GHC.Float" further.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

module GHC.Float.RealFracMethods
    (-- *  Double methods
     -- **  Integer results
     properFractionDoubleInteger,
     truncateDoubleInteger,
     floorDoubleInteger,
     ceilingDoubleInteger,
     roundDoubleInteger,
     -- **  Int results
     properFractionDoubleInt,
     floorDoubleInt,
     ceilingDoubleInt,
     roundDoubleInt,
     -- *  Double/Int conversions, wrapped primops
     double2Int,
     int2Double,
     -- *  Float methods
     -- **  Integer results
     properFractionFloatInteger,
     truncateFloatInteger,
     floorFloatInteger,
     ceilingFloatInteger,
     roundFloatInteger,
     -- **  Int results
     properFractionFloatInt,
     floorFloatInt,
     ceilingFloatInt,
     roundFloatInt,
     -- *  Float/Int conversions, wrapped primops
     float2Int,
     int2Float
     ) where

import GHC.Internal.Float.RealFracMethods

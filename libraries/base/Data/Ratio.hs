-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ratio
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Standard functions on rational numbers
--
-----------------------------------------------------------------------------

module Data.Ratio
    ( Ratio
    , Rational
    , (%)		-- :: (Integral a) => a -> a -> Ratio a
    , numerator		-- :: (Integral a) => Ratio a -> a
    , denominator	-- :: (Integral a) => Ratio a -> a
    , approxRational	-- :: (RealFrac a) => a -> a -> Rational

    -- Ratio instances: 
    --   (Integral a) => Eq   (Ratio a)
    --   (Integral a) => Ord  (Ratio a)
    --   (Integral a) => Num  (Ratio a)
    --   (Integral a) => Real (Ratio a)
    --   (Integral a) => Fractional (Ratio a)
    --   (Integral a) => RealFrac (Ratio a)
    --   (Integral a) => Enum	  (Ratio a)
    --   (Read a, Integral a) => Read (Ratio a)
    --   (Integral a) => Show	  (Ratio a)

  ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.Real		-- The basic defns for Ratio
#endif

#ifdef __HUGS__
import Hugs.Prelude(Ratio(..), (%), numerator, denominator)
#endif

#ifdef __NHC__
import Ratio (Ratio(..), (%), numerator, denominator, approxRational)
#else

-- -----------------------------------------------------------------------------
-- approxRational

-- | 'approxRational', applied to two real fractional numbers @x@ and @epsilon@,
-- returns the simplest rational number within @epsilon@ of @x@.
-- A rational number @y@ is said to be /simpler/ than another @y'@ if
--
-- * @'abs' ('numerator' y) <= 'abs' ('numerator' y')@, and
--
-- * @'denominator' y <= 'denominator' y'@.
--
-- Any real interval contains a unique simplest rational;
-- in particular, note that @0\/1@ is the simplest rational of all.

-- Implementation details: Here, for simplicity, we assume a closed rational
-- interval.  If such an interval includes at least one whole number, then
-- the simplest rational is the absolutely least whole number.  Otherwise,
-- the bounds are of the form q%1 + r%d and q%1 + r'%d', where abs r < d
-- and abs r' < d', and the simplest rational is q%1 + the reciprocal of
-- the simplest rational between d'%r' and d%r.

approxRational		:: (RealFrac a) => a -> a -> Rational
approxRational rat eps	=  simplest (rat-eps) (rat+eps)
	where simplest x y | y < x	=  simplest y x
			   | x == y	=  xr
			   | x > 0	=  simplest' n d n' d'
			   | y < 0	=  - simplest' (-n') d' (-n) d
			   | otherwise	=  0 :% 1
					where xr  = toRational x
					      n   = numerator xr
					      d   = denominator xr
					      nd' = toRational y
					      n'  = numerator nd'
					      d'  = denominator nd'

	      simplest' n d n' d'	-- assumes 0 < n%d < n'%d'
			| r == 0     =	q :% 1
			| q /= q'    =	(q+1) :% 1
			| otherwise  =	(q*n''+d'') :% n''
				     where (q,r)      =	 quotRem n d
					   (q',r')    =	 quotRem n' d'
					   nd''       =  simplest' d' r' d r
					   n''        =  numerator nd''
					   d''        =	 denominator nd''
#endif

%
% (c) The AQUA Project, Glasgow University, 1994-1999
%

\section[Ratio]{Module @Ratio@}

Standard functions on rational numbers

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module	Ratio
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
    --
    -- Implementation checked wrt. Haskell 98 lib report, 1/99.

  ) where

#ifndef __HUGS__
import PrelNum
import PrelNumExtra
#endif
\end{code}

%
% (c) The AQUA Project, Glasgow University, 1994-1999
%

\section[Ratio]{Module @Ratio@}

Standard functions on rational numbers

\begin{code}
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
\end{code}


#ifndef __HUGS__

\begin{code}
import Prelude		-- To generate the dependencies
import PrelReal		-- The basic defns for Ratio
\end{code}

%*********************************************************
%*							*
\subsection{approxRational}
%*							*
%*********************************************************

@approxRational@, applied to two real fractional numbers x and epsilon,
returns the simplest rational number within epsilon of x.  A rational
number n%d in reduced form is said to be simpler than another n'%d' if
abs n <= abs n' && d <= d'.  Any real interval contains a unique
simplest rational; here, for simplicity, we assume a closed rational
interval.  If such an interval includes at least one whole number, then
the simplest rational is the absolutely least whole number.  Otherwise,
the bounds are of the form q%1 + r%d and q%1 + r'%d', where abs r < d
and abs r' < d', and the simplest rational is q%1 + the reciprocal of
the simplest rational between d'%r' and d%r.

\begin{code}
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
\end{code}


#endif


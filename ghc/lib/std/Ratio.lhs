%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Ratio]{Module @Ratio@}

Standard functions on rational numbers

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module	Ratio (
    Ratio, Rational, (%), numerator, denominator, approxRational
  ) where

import PrelNum
import PrelNumExtra
\end{code}



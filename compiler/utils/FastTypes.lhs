%
% (c) The University of Glasgow, 2000-2006
%
\section{Fast integers and booleans}

\begin{code}
module FastTypes (
    FastInt, _ILIT, iBox, iUnbox,
    (+#), (-#), (*#), quotFastInt, negateFastInt,
    (==#), (<#), (<=#), (>=#), (>#),

    FastBool, fastBool, isFastTrue, fastOr, fastAnd
  ) where

#include "HsVersions.h"

#if defined(__GLASGOW_HASKELL__)

-- Import the beggars
import GHC.Exts
	( Int(..), Int#, (+#), (-#), (*#), 
	  quotInt#, negateInt#, (==#), (<#), (<=#), (>=#), (>#)
	)

type FastInt = Int#
_ILIT (I# x) = x
iBox x = I# x
iUnbox (I# x) = x
quotFastInt   = quotInt#
negateFastInt = negateInt#

type FastBool = Int#
fastBool True  = 1#
fastBool False = 0#
isFastTrue x = x ==# 1#

fastOr 1# _ = 1#
fastOr 0# x = x

fastAnd 0# x = 0#
fastAnd 1# x = x

#else /* ! __GLASGOW_HASKELL__ */

type FastInt = Int
_ILIT x = x
iBox x = x
iUnbox x = x
(+#) = (+)
(-#) = (-)
(*#) = (*)
quotFastInt   = quot
negateFastInt = negate
(==#) = (==)
(<#)  = (<)
(<=#) = (<=)
(>=#) = (>=)
(>#)  = (>)

type FastBool = Bool
fastBool x = x
_IS_TRUE_ x = x

#endif /* ! __GLASGOW_HASKELL__ */
\end{code}

%
% (c) The University of Glasgow, 2000
%
\section{Fast integers and booleans}

\begin{code}
module FastTypes (
    FastInt, _ILIT, iBox, iUnbox,
    (+#), (-#), (*#), quotFastInt, negateFastInt,
    (==#), (<#), (<=#), (>=#), (>#),

    FastBool, fastBool, _IS_TRUE_
  ) where

#if defined(__GLASGOW_HASKELL__)

-- Import the beggars
import GlaExts
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
_IS_TRUE_ x = x ==# 1#

#else {- ! __GLASGOW_HASKELL__ -}

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

#endif  {- ! __GLASGOW_HASKELL__ -}
\end{code}

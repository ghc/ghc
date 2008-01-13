%
% (c) The University of Glasgow, 2000-2006
%
\section{Fast integers and booleans}

\begin{code}
module FastTypes (
    FastInt, _ILIT, iBox, iUnbox,
    (+#), (-#), (*#), quotFastInt, negateFastInt,
    (==#), (<#), (<=#), (>=#), (>#),
  ) where

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

--These are among the type-signatures necessary for !ghc to compile
-- but break ghc (can't give a signature for an import...)
--Note that the comparisons actually do return Bools not FastBools.
(+#) :: FastInt -> FastInt -> FastInt
(-#) :: FastInt -> FastInt -> FastInt
(*#) :: FastInt -> FastInt -> FastInt
(==#) :: FastInt -> FastInt -> Bool
(<#) :: FastInt -> FastInt -> Bool
(<=#) :: FastInt -> FastInt -> Bool
(>=#) :: FastInt -> FastInt -> Bool
(>#) :: FastInt -> FastInt -> Bool

#endif /* ! __GLASGOW_HASKELL__ */

-- type-signatures will improve the non-ghc-specific versions
-- and keep things accurate (and ABLE to compile!)
_ILIT :: Int -> FastInt
iBox :: FastInt -> Int
iUnbox :: Int -> FastInt

quotFastInt :: FastInt -> FastInt -> FastInt
negateFastInt :: FastInt -> FastInt

\end{code}

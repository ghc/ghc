%
% (c) The University of Glasgow, 2000-2006
%
\section{Fast booleans}

\begin{code}
module FastBool (
    FastBool, fastBool, isFastTrue, fastOr, fastAnd
  ) where

#if defined(__GLASGOW_HASKELL__)

-- Import the beggars
import GHC.Exts
	( Int(..), Int#, (+#), (-#), (*#), 
	  quotInt#, negateInt#, (==#), (<#), (<=#), (>=#), (>#)
	)
import Panic

type FastBool = Int#
fastBool True  = 1#
fastBool False = 0#
isFastTrue x = x ==# 1#

-- note that fastOr and fastAnd are strict in both arguments
-- since they are unboxed
fastOr 1# _ = 1#
fastOr 0# x = x
fastOr _  _ = panic# "FastTypes: fastOr"

fastAnd 0# _ = 0#
fastAnd 1# x = x
fastAnd _  _ = panic# "FastTypes: fastAnd"

#else /* ! __GLASGOW_HASKELL__ */

type FastBool = Bool
fastBool x = x
isFastTrue x = x
-- make sure these are as strict as the unboxed version,
-- so that the performance characteristics match
fastOr False False = False
fastOr _ _ = True
fastAnd True True = True
fastAnd _ _ = False

#endif /* ! __GLASGOW_HASKELL__ */

fastBool :: Bool -> FastBool
isFastTrue :: FastBool -> Bool
fastOr :: FastBool -> FastBool -> FastBool
fastAnd :: FastBool -> FastBool -> FastBool

\end{code}

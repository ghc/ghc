%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[IOExts]{Module @IOExts@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module IOExts
        ( fixIO
        , unsafePerformIO
        , unsafeInterleaveIO

        , IORef
          -- instance Eq (MutVar a)
        , newIORef
        , readIORef
        , writeIORef

        , trace
        , performGC
	
	, reallyUnsafePtrEq
        ) where
\end{code}

\begin{code}
import PrelBase
import IOBase
import IORef
import STBase
import Unsafe
import GHC

reallyUnsafePtrEq a b =
    case reallyUnsafePtrEquality# a b of
	 0# -> False
	 _  -> True
\end{code}

%
% (c) The AQUA Project, Glasgow University, 1994-1997
%
\section[ByteArray]{The @ByteArray@ interface}

Immutable, read-only chunks of bytes, the @ByteArray@ collects
together the definitions in @ArrBase@ and exports them as one.

\begin{code}
module ByteArray
       (
        ByteArray(..),  -- not abstract, for now.
        Ix,

        --Indexing of ordinary @Arrays@ is standard Haskell and isn't defined here.
        indexCharArray,       -- :: Ix ix => ByteArray ix -> ix -> Char 
        indexIntArray,        -- :: Ix ix => ByteArray ix -> ix -> Int
        indexWordArray,       -- :: Ix ix => ByteArray ix -> ix -> Word
        indexAddrArray,       -- :: Ix ix => ByteArray ix -> ix -> Addr
        indexFloatArray,      -- :: Ix ix => ByteArray ix -> ix -> Float
        indexDoubleArray,     -- :: Ix ix => ByteArray ix -> ix -> Double
        indexStablePtrArray   -- :: Ix ix => ByteArray ix -> ix -> (StablePtr a)
 
       ) where

import PrelArr
import PrelBase
import PrelForeign
import Ix
\end{code}

\begin{code}
indexStablePtrArray    :: Ix ix => ByteArray ix -> ix -> (StablePtr a)
indexStablePtrArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexStablePtrArray# barr# n# 	of { r# ->
    (StablePtr r#)}}
\end{code}

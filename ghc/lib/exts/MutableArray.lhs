%
% (c) The AQUA Project, Glasgow University, 1997
%
\section[MutableArray]{The @MutableArray@ interface}

Mutable (byte)arrays interface, re-exports type types and operations
over them from @ArrBase@. Have to be used in conjunction with
@ST@.

\begin{code}
module MutableArray 
   (
    MutableArray(..),        -- not abstract
    MutableByteArray(..),

    ST,
    Ix,

    -- Creators:
    newArray,           -- :: Ix ix => (ix,ix) -> elt -> ST s (MutableArray s ix elt)
    newCharArray,
    newAddrArray,
    newIntArray,
    newFloatArray,
    newDoubleArray,     -- :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 

    boundsOfArray,      -- :: Ix ix => MutableArray s ix elt -> (ix, ix)  
    boundsOfByteArray,  -- :: Ix ix => MutableByteArray s ix -> (ix, ix)

    readArray,   	-- :: Ix ix => MutableArray s ix elt -> ix -> ST s elt 

    readCharArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Char 
    readIntArray,       -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Int
    readAddrArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Addr
    readFloatArray,     -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Float
    readDoubleArray,    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Double

    writeArray,  	-- :: Ix ix => MutableArray s ix elt -> ix -> elt -> ST s () 
    writeCharArray,     -- :: Ix ix => MutableByteArray s ix -> ix -> Char -> ST s () 
    writeIntArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> Int  -> ST s () 
    writeAddrArray,     -- :: Ix ix => MutableByteArray s ix -> ix -> Addr -> ST s () 
    writeFloatArray,    -- :: Ix ix => MutableByteArray s ix -> ix -> Float -> ST s () 
    writeDoubleArray,   -- :: Ix ix => MutableByteArray s ix -> ix -> Double -> ST s () 

    freezeArray,	-- :: Ix ix => MutableArray s ix elt -> ST s (Array ix elt)
    freezeCharArray,    -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeIntArray,     -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeAddrArray,    -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeFloatArray,   -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeDoubleArray,  -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

    unsafeFreezeArray,     -- :: Ix ix => MutableArray s ix elt -> ST s (Array ix elt)  
    unsafeFreezeByteArray, -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    thawArray              -- :: Ix ix => Array ix elt -> ST s (MutableArray s ix elt)

    ) where

import PrelArr
import ST
import Ix

\end{code}

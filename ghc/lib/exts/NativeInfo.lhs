%
% (c) The AQUA Project, Glasgow University, 1998
%
\section[NativeInfo]{Module @NativeInfo@}

Misc information about the characteristics of the host 
architecture/machine lucky enough to run your program.

\begin{code}
#include "MachDeps.h"

module NativeInfo
       (
	 isBigEndian	    -- :: Bool

       , os		    -- :: String
       , arch		    -- :: String

       , sizeofAddr         -- :: Word32
       , sizeofDouble	    -- :: ""
       , sizeofFloat
       , sizeofChar
       
       , sizeofWord
       , sizeofWord8
       , sizeofWord16
       , sizeofWord32
       , sizeofWord64

       , sizeofInt
       , sizeofInt8
       , sizeofInt16
       , sizeofInt32
       , sizeofInt64
       
       ) where

import Word
import Addr
import Int

\end{code}

Byte-ordering:

\begin{code}
isBigEndian :: Bool
isBigEndian = 
#ifdef WORDS_BIGENDIAN
    True
#else
    False
#endif
\end{code}

Host architecture and OS info:

\begin{code}
arch :: String
arch = HOST_ARCH

os :: String
os = HOST_OS
\end{code}

@sizeofX@ returns the size of the (basic) type X (in 8-bit byte units.)

(Do not provide a type class for this, since writing out sizeofX is shorter
(and more consise) than using an overloaded function that returns the sizeof
at a particular type.)

\begin{code}
sizeofAddr :: Word32
sizeofAddr = ADDR_SIZE_IN_BYTES

sizeofDouble :: Word32
sizeofDouble = DOUBLE_SIZE_IN_BYTES

sizeofFloat :: Word32
sizeofFloat  = FLOAT_SIZE_IN_BYTES

sizeofInt   :: Word32
sizeofInt     = INT_SIZE_IN_BYTES

sizeofWord   :: Word32
sizeofWord     = WORD_SIZE_IN_BYTES

sizeofChar  :: Word32
sizeofChar    = CHAR_SIZE_IN_BYTES
\end{code}

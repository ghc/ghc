%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1997
%
\section[CPUTime]{Haskell 1.4 CPU Time Library}

\begin{code}
module CPUTime 
	(
         getCPUTime,       -- :: IO Integer
	 cpuTimePrecision  -- :: Integer
        ) where

import PrelBase (Int(..), indexIntArray#, Addr(..) )
import ArrBase  --(ByteArray(..))
import IOBase
import IO
import STBase
import Ratio

\end{code}

Computation @getCPUTime@ returns the number of picoseconds CPU time
used by the current program.  The precision of this result is
implementation-dependent.

The @cpuTimePrecision@ constant is the resolution (in picoseconds!) of
the number of 

\begin{code}
getCPUTime :: IO Integer
getCPUTime =
    newIntArray (0,3)			    `thenIO_Prim` \ marr ->
    unsafeFreezeByteArray marr		    `thenIO_Prim` \ barr@(ByteArray _ frozen#) ->
    _ccall_ getCPUTime barr		    `thenIO_Prim` \ ptr ->
    if (ptr::Addr) /= ``NULL'' then
        return ((fromIntegral (I# (indexIntArray# frozen# 0#)) * 1000000000 + 
                fromIntegral (I# (indexIntArray# frozen# 1#)) + 
		fromIntegral (I# (indexIntArray# frozen# 2#)) * 1000000000 + 
                fromIntegral (I# (indexIntArray# frozen# 3#))) * 1000)
    else
	fail (IOError Nothing UnsupportedOperation "getCPUTime: can't get CPU time")

cpuTimePrecision :: Integer
cpuTimePrecision = round ((1000000000000::Integer) % 
                          fromInt (unsafePerformPrimIO (_ccall_ clockTicks )))
\end{code}




 

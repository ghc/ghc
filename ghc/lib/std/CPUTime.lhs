%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1997
%
\section[CPUTime]{Haskell 1.4 CPU Time Library}

\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/stgio.h" #-}

module CPUTime 
	(
         getCPUTime,       -- :: IO Integer
	 cpuTimePrecision  -- :: Integer
        ) where
\end{code}


#ifndef __HUGS__

\begin{code}
import PrelBase
import PrelArr  	( ByteArray(..), newIntArray, unsafeFreezeByteArray )
import PrelMaybe
import PrelNum
import PrelNumExtra
import PrelIOBase
import PrelST
import IO		( ioError )
import PrelNum ( Num(..), Integral(..) )	-- To get fromInt/toInt
import Ratio
\end{code}

Computation @getCPUTime@ returns the number of picoseconds CPU time
used by the current program.  The precision of this result is
implementation-dependent.

The @cpuTimePrecision@ constant is the smallest measurable difference
in CPU time that the implementation can record, and is given as an
integral number of picoseconds.

\begin{code}
getCPUTime :: IO Integer
getCPUTime = do
    marr <- stToIO (newIntArray ((0::Int),3))
    barr <- stToIO (unsafeFreezeByteArray marr)
    rc   <- primGetCPUTime barr
    if rc /= 0 then
      case barr of
       ByteArray _ _ frozen# -> -- avoid bounds checking
        return ((fromIntegral (I# (indexIntArray# frozen# 0#)) * 1000000000 + 
                 fromIntegral (I# (indexIntArray# frozen# 1#)) + 
		 fromIntegral (I# (indexIntArray# frozen# 2#)) * 1000000000 + 
                 fromIntegral (I# (indexIntArray# frozen# 3#))) * 1000)
     else
	ioError (IOError Nothing UnsupportedOperation 
			 "getCPUTime"
		         "can't get CPU time")

cpuTimePrecision :: Integer
cpuTimePrecision = round ((1000000000000::Integer) % 
                          fromInt (unsafePerformIO clockTicks))

foreign import "libHS_cbits" "getCPUTime" unsafe primGetCPUTime :: ByteArray Int -> IO Int
foreign import "libHS_cbits" "clockTicks" clockTicks :: IO Int

\end{code}

#else

\begin{code}
getCPUTime :: IO Integer
getCPUTime 
   = do seconds <- nh_getCPUtime
        return (round (seconds * 1.0e+12))

cpuTimePrecision :: Integer
cpuTimePrecision
   = primRunST (
        do resolution <- nh_getCPUprec
           return (round (resolution * 1.0e+12))
     )
\end{code} 
#endif

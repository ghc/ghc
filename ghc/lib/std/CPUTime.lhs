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

#ifdef __HUGS__
import PreludeBuiltin
#else
import PrelBase
import PrelArr  	( ByteArray(..), newIntArray, unsafeFreezeByteArray )
import PrelMaybe
import PrelNum
import PrelNumExtra
import PrelIOBase
import PrelST
#endif
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
#ifdef __HUGS__

sizeof_int :: Int
sizeof_int = 4

getCPUTime :: IO Integer
getCPUTime = do
    marr <- primNewByteArray (sizeof_int * 4)
    rc   <- primGetCPUTime marr
    if rc /= 0 then do
        x0 <- primReadIntArray marr 0
        x1 <- primReadIntArray marr 1
        x2 <- primReadIntArray marr 2
        x3 <- primReadIntArray marr 3
        return ((fromIntegral x0 * 1000000000 + fromIntegral  x1 + 
		 fromIntegral x2 * 1000000000 + fromIntegral  x3)
	       * 1000)
      else
	ioError (IOError Nothing UnsupportedOperation 
			 "getCPUTime"
			 "can't get CPU time")

#else

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
#endif

cpuTimePrecision :: Integer
cpuTimePrecision = round ((1000000000000::Integer) % 
                          fromInt (unsafePerformIO clockTicks))
\end{code}

\begin{code}
foreign import "libHS_cbits" "getCPUTime" unsafe primGetCPUTime :: ByteArray Int -> IO Int
foreign import "libHS_cbits" "clockTicks" clockTicks :: IO Int

\end{code}


 

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
import PrelAddr
import PrelIOBase
import PrelST
#endif
import IO		( ioError )
import Ratio

#ifdef __HUGS__
#define cat2(x,y)  x/**/y
#define CCALL(fun) cat2(prim_,fun)
#define stToIO id
#define sizeof_int64 8
#else
#define CCALL(fun) _ccall_ fun
#define const_BUFSIZ ``BUFSIZ''
#define primPackString
#endif

\end{code}

Computation @getCPUTime@ returns the number of picoseconds CPU time
used by the current program.  The precision of this result is
implementation-dependent.

The @cpuTimePrecision@ constant is the smallest measurable difference
in CPU time that the implementation can record, and is given as an
integral number of picoseconds.

\begin{code}
#ifdef __HUGS__

getCPUTime :: IO Integer
getCPUTime = do
    marr <- primNewByteArray (sizeof_int * 4)
    ptr  <- CCALL(getCPUTime) marr
    if (ptr /= nullAddr) then do
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
getCPUTime = 
    stToIO (newIntArray (0,3))		>>= \ marr ->
    stToIO (unsafeFreezeByteArray marr)	>>= \ barr@(ByteArray _ frozen#) ->
    _ccall_ getCPUTime barr		>>= \ ptr ->
    if (ptr::Addr) /= ``NULL'' then
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
                          fromInt (unsafePerformIO (CCALL(clockTicks) )))
\end{code}

\begin{code}
#ifdef __HUGS__

sizeof_int = 4

foreign import stdcall "libHS_cbits.so" "getCPUTime" prim_getCPUTime :: Bytes -> IO Addr
foreign import stdcall "libHS_cbits.so" "clockTicks" prim_clockTicks :: IO Int
#endif
\end{code}


 

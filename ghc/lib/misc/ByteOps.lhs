{-
%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1994
%
\section[ByteOps]{Convert to/from ``bytes''; to support @Native@ class}

This mimics some code that comes with HBC.
-}

\begin{code}
{-# OPTIONS -#include "cbits/ByteOps.h" #-}

module ByteOps (
	longToBytes,
	intToBytes,
	shortToBytes,
	floatToBytes,
	doubleToBytes,

	bytesToLong,
	bytesToInt,
	bytesToShort,
	bytesToFloat,
	bytesToDouble
    ) where

import GlaExts
import PrelBase

-- \tr{xxxToBytes} prepends an \tr{xxx} to a byte stream.
-- \tr{bytesToXxx} snaffles an \tr{xxx} from a byte stream,
-- also returning the rest of the stream.

type Bytes = [Char]

longToBytes    :: Int    -> Bytes -> Bytes
intToBytes     :: Int    -> Bytes -> Bytes
shortToBytes   :: Int    -> Bytes -> Bytes
floatToBytes   :: Float  -> Bytes -> Bytes
doubleToBytes  :: Double -> Bytes -> Bytes

bytesToLong    :: Bytes -> (Int,    Bytes)
bytesToInt     :: Bytes -> (Int,    Bytes)
bytesToShort   :: Bytes -> (Int,    Bytes)
bytesToFloat   :: Bytes -> (Float,  Bytes)
bytesToDouble  :: Bytes -> (Double, Bytes)

--Here we go.

#define XXXXToBytes(type,xxxx,xxxx__) \
xxxx i stream \
  = let \
	long_bytes	{- DANGEROUS! -} \
	  = unsafePerformIO ( \
		{- Allocate a wad of memory to put the "long"'s bytes. \
		   Let's hope 32 bytes will be big enough. -} \
		stToIO (newCharArray (0::Int, 31)) >>= \ arr# -> \
 \
		{- Call out to C to do the dirty deed: -} \
		_casm_ ``%r = xxxx__ ((type)%0, (unsigned char *)%1);'' i arr# \
			>>= \ num_bytes -> \
 \
		unpack arr# 0 (num_bytes - 1) \
	    ) \
    in \
    long_bytes ++ stream

XXXXToBytes(long,longToBytes,long2bytes__)
XXXXToBytes(int,intToBytes,int2bytes__)
XXXXToBytes(short,shortToBytes,short2bytes__)
XXXXToBytes(float,floatToBytes,float2bytes__)
XXXXToBytes(double,doubleToBytes,double2bytes__)

--------------
unpack :: MutableByteArray RealWorld Int -> Int -> Int -> IO [Char]

unpack arr# curr last
  = if curr > last then
	return []
    else
	stToIO (readCharArray arr# curr) >>= \ ch ->
	unpack arr# (curr + 1) last	 >>= \ rest ->
	return (ch : rest)

-------------
--Now we go the other way.  The paranoia checking (absent) leaves
--something to be desired.  Really have to be careful on
--funny-sized things like \tr{shorts}...

#define bytesToXXXX(htype,xxxx,alloc,read,xxxx__) \
xxxx stream \
  = unsafePerformIO ( \
	{- slam (up to) 32 bytes [random] from the stream into an array -} \
	stToIO (newCharArray (0::Int, 31)) >>= \ arr# -> \
	pack arr# 0 31 stream		   >> \
 \
	{- make a one-element array to hold the result: -} \
	stToIO (alloc (0::Int, 0))	    >>= \ res# -> \
 \
	{- call the C to do the business: -} \
	_casm_ ``%r = xxxx__ ((P_)%0, (htype *) %1);'' arr# res# \
		>>= \ num_bytes -> \
 \
	{- read the result out of "res#": -} \
	stToIO (read res# (0::Int))  >>= \ i -> \
 \
	{- box the result and drop the number of bytes taken: -} \
	return (i, my_drop num_bytes stream) \
    )

bytesToXXXX(I_,bytesToLong,newIntArray,readIntArray,bytes2long__)
bytesToXXXX(I_,bytesToInt,newIntArray,readIntArray,bytes2int__)
bytesToXXXX(I_,bytesToShort,newIntArray,readIntArray,bytes2short__)
bytesToXXXX(StgFloat,bytesToFloat,newFloatArray,readFloatArray,bytes2float__)
bytesToXXXX(StgDouble,bytesToDouble,newDoubleArray,readDoubleArray,bytes2double__)

----------------------
pack :: MutableByteArray RealWorld Int -> Int -> Int -> [Char] -> IO ()

pack arr# curr last from_bytes
  = if curr > last then
       return ()
    else
       case from_bytes of
	 [] -> stToIO (writeCharArray arr# curr (chr 0))

	 (from_byte : xs) ->
	   stToIO (writeCharArray arr# curr from_byte) >>
	   pack arr# (curr + 1) last xs

-- more cavalier than usual; we know there will be enough bytes:

my_drop :: Int -> [a] -> [a]

my_drop 0 xs     = xs
--my_drop _  []	  = []
my_drop m (_:xs) = my_drop (m - 1) xs

\end{code}

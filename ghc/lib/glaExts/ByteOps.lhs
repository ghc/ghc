%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1994
%
\section[ByteOps]{Convert to/from ``bytes''; to support @Native@ class}

This mimics some code that comes with HBC.

\begin{code}
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

import Cls
import Core
import IInt
import IFloat
import IDouble
import List		( (++), foldr )
import Prel		( chr )
import PS		( _PackedString, _unpackPS )
import TyArray		( Array(..) )
import TyComplex
import PreludeGlaST
import Text
\end{code}

\tr{xxxToBytes} prepends an \tr{xxx} to a byte stream.
\tr{bytesToXxx} snaffles an \tr{xxx} from a byte stream,
also returning the rest of the stream.
\begin{code}
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
\end{code}

Here we go.
\begin{code}
#define XXXXToBytes(type,xxxx,xxxx__) \
xxxx i stream \
  = let \
	long_bytes	{- DANGEROUS! -} \
	  = unsafePerformPrimIO ( \
		{- Allocate a wad of memory to put the "long"'s bytes. \
		   Let's hope 32 bytes will be big enough. -} \
		newCharArray (0::Int, 31)   `thenPrimIO` \ arr# -> \
 \
		{- Call out to C to do the dirty deed: -} \
		_casm_ ``%r = xxxx__ ((type)%0, (unsigned char *)%1);'' i arr# \
			`thenPrimIO` \ num_bytes -> \
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
\end{code}

\begin{code}
unpack :: _MutableByteArray _RealWorld Int -> Int -> Int -> PrimIO [Char]

unpack arr# curr last
  = if curr > last then
	returnPrimIO []
    else
	readCharArray arr# curr	    `thenPrimIO` \ ch ->
	unpack arr# (curr + 1) last `thenPrimIO` \ rest ->
	returnPrimIO (ch : rest)
\end{code}

Now we go the other way.  The paranoia checking (absent) leaves
something to be desired.  Really have to be careful on
funny-sized things like \tr{shorts}...
\begin{code}
#define bytesToXXXX(htype,xxxx,alloc,read,xxxx__) \
xxxx stream \
  = unsafePerformPrimIO ( \
	{- slam (up to) 32 bytes [random] from the stream into an array -} \
	newCharArray (0::Int, 31)   `thenPrimIO` \ arr# -> \
	pack arr# 0 31 stream	    `seqPrimIO` \
 \
	{- make a one-element array to hold the result: -} \
	alloc (0::Int, 0)	    `thenPrimIO` \ res# -> \
 \
	{- call the C to do the business: -} \
	_casm_ ``%r = xxxx__ ((P_)%0, (htype *) %1);'' arr# res# \
		`thenPrimIO` \ num_bytes -> \
 \
	{- read the result out of "res#": -} \
	read res# (0::Int)  `thenPrimIO` \ i -> \
 \
	{- box the result and drop the number of bytes taken: -} \
	returnPrimIO (i, my_drop num_bytes stream) \
    )

bytesToXXXX(I_,bytesToLong,newIntArray,readIntArray,bytes2long__)
bytesToXXXX(I_,bytesToInt,newIntArray,readIntArray,bytes2int__)
bytesToXXXX(I_,bytesToShort,newIntArray,readIntArray,bytes2short__)
bytesToXXXX(StgFloat,bytesToFloat,newFloatArray,readFloatArray,bytes2float__)
bytesToXXXX(StgDouble,bytesToDouble,newDoubleArray,readDoubleArray,bytes2double__)
\end{code}

\begin{code}
pack :: _MutableByteArray _RealWorld Int -> Int -> Int -> [Char] -> PrimIO ()

pack arr# curr last from_bytes
  = if curr > last then
       returnPrimIO ()
    else
       case from_bytes of
	 [] -> writeCharArray arr# curr (chr 0)

	 (from_byte : xs) ->
	   writeCharArray arr# curr from_byte	`seqPrimIO`
	   pack arr# (curr + 1) last xs

-- more cavalier than usual; we know there will be enough bytes:

my_drop :: Int -> [a] -> [a]

my_drop 0 xs     = xs
--my_drop _  []	  = []
my_drop m (_:xs) = my_drop (m - 1) xs
\end{code}

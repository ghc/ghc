%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Addr]{Module @Addr@}

\begin{code}
module Addr 
	( module PrelAddr
	, module Word
	, module Int
	, module Addr 
	
        -- (non-standard) coercions
	, addrToInt		-- :: Addr -> Int  
	, intToAddr		-- :: Int  -> Addr
	    
	) where

import PrelAddr
import PrelCCall  ( Word(..) )
import PrelBase
import Word	( indexWord8OffAddr,  indexWord16OffAddr
		, indexWord32OffAddr, indexWord64OffAddr
		, readWord8OffAddr,   readWord16OffAddr
		, readWord32OffAddr,  readWord64OffAddr
		, writeWord8OffAddr,  writeWord16OffAddr
		, writeWord32OffAddr, writeWord64OffAddr
		)

import Int	( indexInt8OffAddr,  indexInt16OffAddr
		, indexInt32OffAddr, indexInt64OffAddr
		, readInt8OffAddr,   readInt16OffAddr
		, readInt32OffAddr,  readInt64OffAddr
		, writeInt8OffAddr,  writeInt16OffAddr
		, writeInt32OffAddr, writeInt64OffAddr
		)
import PrelIOBase ( IO(..), IOResult(..) )

#ifndef __PARALLEL_HASKELL__
import PrelForeign ( ForeignObj(..), StablePtr(..) )
#endif

\end{code}

Coercing between machine ints and words

\begin{code}
addrToInt :: Addr -> Int
addrToInt (A# a#) = I# (addr2Int# a#)

intToAddr :: Int -> Addr
intToAddr (I# i#) = A# (int2Addr# i#)
\end{code}

Indexing immutable memory:

\begin{code}
indexCharOffAddr   :: Addr -> Int -> Char
indexIntOffAddr    :: Addr -> Int -> Int
indexWordOffAddr   :: Addr -> Int -> Word
--in PrelAddr: indexAddrOffAddr   :: Addr -> Int -> Addr
indexFloatOffAddr  :: Addr -> Int -> Float
indexDoubleOffAddr :: Addr -> Int -> Double

indexCharOffAddr (A# addr#) n
  = case n  	    		    	of { I# n# ->
    case indexCharOffAddr# addr# n# 	of { r# ->
    (C# r#)}}

indexIntOffAddr (A# addr#) n
  = case n  	    		    	of { I# n# ->
    case indexIntOffAddr# addr# n# 	of { r# ->
    (I# r#)}}

indexWordOffAddr (A# addr#) n
  = case n  	    		    	of { I# n# ->
    case indexWordOffAddr# addr# n# 	of { r# ->
    (W# r#)}}

indexFloatOffAddr (A# addr#) n
  = case n  	    		    	of { I# n# ->
    case indexFloatOffAddr# addr# n# 	of { r# ->
    (F# r#)}}

indexDoubleOffAddr (A# addr#) n
  = case n  	    	 	    	of { I# n# ->
    case indexDoubleOffAddr# addr# n# 	of { r# ->
    (D# r#)}}
\end{code}

Indexing mutable memory:

\begin{code}
readCharOffAddr    :: Addr -> Int -> IO Char
readCharOffAddr a i = _casm_ `` %r=(StgChar)(((StgChar*)%0)[(StgInt)%1]); '' a i

readIntOffAddr    :: Addr -> Int -> IO Int
readIntOffAddr a i = _casm_ `` %r=(StgInt)(((StgInt*)%0)[(StgInt)%1]); '' a i

readStablePtrOffAddr    :: Addr -> Int -> IO (StablePtr a)
readStablePtrOffAddr a i = _casm_ `` %r=(StgStablePtr)(((StgStablePtr*)%0)[(StgInt)%1]); '' a i

readWordOffAddr    :: Addr -> Int -> IO Word
readWordOffAddr a i = _casm_ `` %r=(StgWord)(((StgWord*)%0)[(StgInt)%1]); '' a i

readAddrOffAddr    :: Addr -> Int -> IO Addr
readAddrOffAddr a i = _casm_ `` %r=(StgAddr)(((StgAddr*)%0)[(StgInt)%1]); '' a i

readFloatOffAddr    :: Addr -> Int -> IO Float
readFloatOffAddr a i = _casm_ `` %r=(StgFloat)(((StgFloat*)%0)[(StgInt)%1]); '' a i

readDoubleOffAddr  :: Addr -> Int -> IO Double
readDoubleOffAddr a i = _casm_ `` %r=(StgDouble)(((StgDouble*)%0)[(StgInt)%1]); '' a i
\end{code}


\begin{code}
writeCharOffAddr   :: Addr -> Int -> Char   -> IO ()
writeCharOffAddr (A# a#) (I# i#) (C# c#) = IO $ \ s# ->
      case (writeCharOffAddr#  a# i# c# s#) of s2# -> IOok s2# () 

writeIntOffAddr    :: Addr -> Int -> Int    -> IO ()
writeIntOffAddr (A# a#) (I# i#) (I# e#) = IO $ \ s# ->
      case (writeIntOffAddr#  a# i# e# s#) of s2# -> IOok s2# () 

writeStablePtrOffAddr    :: Addr -> Int -> StablePtr a -> IO ()
writeStablePtrOffAddr (A# a#) (I# i#) (StablePtr e#) = IO $ \ s# ->
      case (writeStablePtrOffAddr#  a# i# e# s#) of s2# -> IOok s2# () 

writeWordOffAddr    :: Addr -> Int -> Word  -> IO ()
writeWordOffAddr (A# a#) (I# i#) (W# e#) = IO $ \ s# ->
      case (writeWordOffAddr#  a# i# e# s#) of s2# -> IOok s2# () 

writeAddrOffAddr   :: Addr -> Int -> Addr   -> IO ()
writeAddrOffAddr (A# a#) (I# i#) (A# e#) = IO $ \ s# ->
      case (writeAddrOffAddr#  a# i# e# s#) of s2# -> IOok s2# () 

#ifndef __PARALLEL_HASKELL__
writeForeignObjOffAddr   :: Addr -> Int -> ForeignObj -> IO ()
writeForeignObjOffAddr (A# a#) (I# i#) (ForeignObj e#) = IO $ \ s# ->
      case (writeForeignObjOffAddr#  a# i# e# s#) of s2# -> IOok s2# () 
#endif

writeFloatOffAddr  :: Addr -> Int -> Float  -> IO ()
writeFloatOffAddr (A# a#) (I# i#) (F# e#) = IO $ \ s# ->
      case (writeFloatOffAddr#  a# i# e# s#) of s2# -> IOok s2# () 

writeDoubleOffAddr :: Addr -> Int -> Double -> IO ()
writeDoubleOffAddr (A# a#) (I# i#) (D# e#) = IO $ \ s# ->
      case (writeDoubleOffAddr#  a# i# e# s#) of s2# -> IOok s2# () 

\end{code}

%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Addr]{Module @Addr@}

\begin{code}
#include "MachDeps.h"

module Addr 
	( Addr

	, module Addr
#ifndef __HUGS__
	, module Word
	, module Int
	, module PrelAddr 
#endif

        -- (non-standard) coercions
	, addrToInt		-- :: Addr -> Int  
	, intToAddr		-- :: Int  -> Addr
	    
	) where

import NumExts
#ifndef __HUGS__
import PrelAddr
import PrelForeign
import PrelStable
import PrelBase
import PrelIOBase ( IO(..) )
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
#endif

\end{code}

\begin{code}
#ifdef __HUGS__
instance Show Addr where
   showsPrec p addr rs = pad_out (showHex int "") rs
     where
        -- want 0s prefixed to pad it out to a fixed length.
       pad_out ('0':'x':ls) rs = 
	  '0':'x':(replicate (2*ADDR_SIZE_IN_BYTES - length ls) '0') 
			++ ls ++ rs
       int = primAddrToInt addr
#else
instance Show Addr where
   showsPrec p (A# a) rs = pad_out (showHex int "") rs
     where
        -- want 0s prefixed to pad it out to a fixed length.
       pad_out ('0':'x':ls) rs = 
	  '0':'x':(replicate (2*ADDR_SIZE_IN_BYTES - length ls) '0') ++ ls ++ rs

       int = 
	case word2Integer# (int2Word# (addr2Int# a)) of
	  (# s, d #) -> J# s d
#endif
\end{code}


Coercing between machine ints and words

\begin{code}
addrToInt :: Addr -> Int
intToAddr :: Int -> Addr

#ifdef __HUGS__
addrToInt = primAddrToInt
intToAddr = primIntToAddr
#else
addrToInt (A# a#) = I# (addr2Int# a#)
intToAddr (I# i#) = A# (int2Addr# i#)
#endif
\end{code}

Indexing immutable memory:

\begin{code}
indexCharOffAddr   :: Addr -> Int -> Char
indexIntOffAddr    :: Addr -> Int -> Int
indexWordOffAddr   :: Addr -> Int -> Word
--in PrelAddr: indexAddrOffAddr   :: Addr -> Int -> Addr
indexFloatOffAddr  :: Addr -> Int -> Float
indexDoubleOffAddr :: Addr -> Int -> Double
indexStablePtrOffAddr :: Addr -> Int -> StablePtr a

#ifdef __HUGS__
indexCharOffAddr   = error "TODO: indexCharOffAddr  "
indexIntOffAddr    = error "TODO: indexIntOffAddr   "
indexWordOffAddr   = error "TODO: indexWordOffAddr  "
indexAddrOffAddr   = error "TODO: indexAddrOffAddr  "
indexFloatOffAddr  = error "TODO: indexFloatOffAddr "
indexDoubleOffAddr = error "TODO: indexDoubleOffAddr"
indexStablePtrOffAddr = error "TODO: indexStablePtrOffAddr"
#else
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

indexStablePtrOffAddr (A# addr#) n
  = case n  	    	 	    	 of { I# n# ->
    case indexStablePtrOffAddr# addr# n# of { r# ->
    (StablePtr r#)}}
#endif
\end{code}

Indexing mutable memory:

\begin{code}
readCharOffAddr    :: Addr -> Int -> IO Char
readIntOffAddr     :: Addr -> Int -> IO Int
readWordOffAddr    :: Addr -> Int -> IO Word
readAddrOffAddr    :: Addr -> Int -> IO Addr
readFloatOffAddr   :: Addr -> Int -> IO Float
readDoubleOffAddr  :: Addr -> Int -> IO Double
readStablePtrOffAddr  :: Addr -> Int -> IO (StablePtr a)

#ifdef __HUGS__
readCharOffAddr      = error "TODO: readCharOffAddr     "
readIntOffAddr       = error "TODO: readIntOffAddr      "
readWordOffAddr      = error "TODO: readWordOffAddr     "
readAddrOffAddr      = error "TODO: readAddrOffAddr     "
readFloatOffAddr     = error "TODO: readFloatOffAddr    "
readDoubleOffAddr    = error "TODO: readDoubleOffAddr   "
readStablePtrOffAddr = error "TODO: readStablePtrOffAddr"
#else
readCharOffAddr a i = case indexCharOffAddr a i of { C# o# -> return (C# o#) }
readIntOffAddr a i  = case indexIntOffAddr a i of { I# o# -> return (I# o#) }
readWordOffAddr a i = case indexWordOffAddr a i of { W# o# -> return (W# o#) }
readAddrOffAddr a i = case indexAddrOffAddr a i of { A# o# -> return (A# o#) }
readFloatOffAddr a i = case indexFloatOffAddr a i of { F# o# -> return (F# o#) }
readDoubleOffAddr a i = case indexDoubleOffAddr a i of { D# o# -> return (D# o#) }
readStablePtrOffAddr a i = case indexStablePtrOffAddr a i of { StablePtr x -> return (StablePtr x) }
#endif
\end{code}


\begin{code}
writeCharOffAddr   :: Addr -> Int -> Char   -> IO ()
writeIntOffAddr    :: Addr -> Int -> Int    -> IO ()
writeWordOffAddr   :: Addr -> Int -> Word  -> IO ()
writeAddrOffAddr   :: Addr -> Int -> Addr   -> IO ()
writeFloatOffAddr  :: Addr -> Int -> Float  -> IO ()
writeDoubleOffAddr :: Addr -> Int -> Double -> IO ()

#ifdef __HUGS__
writeCharOffAddr    = error "TODO: writeCharOffAddr   "
writeIntOffAddr     = error "TODO: writeIntOffAddr    "
writeWordOffAddr    = error "TODO: writeWordOffAddr   "
writeAddrOffAddr    = error "TODO: writeAddrOffAddr   "
writeFloatOffAddr   = error "TODO: writeFloatOffAddr  "
writeDoubleOffAddr  = error "TODO: writeDoubleOffAddr "
#else
writeCharOffAddr (A# a#) (I# i#) (C# c#) = IO $ \ s# ->
      case (writeCharOffAddr#  a# i# c# s#) of s2# -> (# s2#, () #)

writeIntOffAddr (A# a#) (I# i#) (I# e#) = IO $ \ s# ->
      case (writeIntOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeWordOffAddr (A# a#) (I# i#) (W# e#) = IO $ \ s# ->
      case (writeWordOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeAddrOffAddr (A# a#) (I# i#) (A# e#) = IO $ \ s# ->
      case (writeAddrOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeFloatOffAddr (A# a#) (I# i#) (F# e#) = IO $ \ s# ->
      case (writeFloatOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeDoubleOffAddr (A# a#) (I# i#) (D# e#) = IO $ \ s# ->
      case (writeDoubleOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

#ifndef __PARALLEL_HASKELL__
writeForeignObjOffAddr   :: Addr -> Int -> ForeignObj -> IO ()
writeForeignObjOffAddr (A# a#) (I# i#) (ForeignObj e#) = IO $ \ s# ->
      case (writeForeignObjOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)
#endif

writeStablePtrOffAddr    :: Addr -> Int -> StablePtr a -> IO ()
writeStablePtrOffAddr (A# a#) (I# i#) (StablePtr e#) = IO $ \ s# ->
      case (writeStablePtrOffAddr#  a# i# e# s#) of s2# -> (# s2# , () #)

#endif
\end{code}

%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[PrelArrExtra]{Module @PrelArrExtra@}

The following functions should be in PrelArr, but need -monly-2-regs
to compile.  So as not to compile the whole of PrelArr with
-monly-2-regs, the culprits have been moved out into a separate
module.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelArrExtra where

import Ix
import PrelArr
import PrelST
import PrelBase
import PrelGHC

freezeFloatArray  :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeDoubleArray :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

freezeFloatArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray ixs frozen# #) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> (# State# s, ByteArray# #)

    freeze arr1# end# s#
      = case (newFloatArray# end# s#)    of { (# s2#, newarr1# #) ->
	case copy 0# arr1# newarr1# s2#  of { (# s3#, newarr2# #) ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> (# State# s, MutableByteArray# s #)

	copy cur# from# to# s1#
	  | cur# ==# end#
	    = (# s1#, to# #)
	  | otherwise
	    = case (readFloatArray#  from# cur#     s1#)  of { (# s2#, ele #) ->
	      case (writeFloatArray# to#   cur# ele s2#)  of { s3# ->
	      copy (cur# +# 1#) from# to# s3#
	      }}

freezeDoubleArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray ixs frozen# #) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> (# State# s, ByteArray# #)

    freeze arr1# n# s1#
      = case (newDoubleArray# n# s1#)  	   of { (# s2#, newarr1# #) ->
	case copy 0# n# arr1# newarr1# s2# of { (# s3#, newarr2# #) ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> (# State# s, MutableByteArray# s #)

	copy cur# end# from# to# st#
	  | cur# ==# end#
	    = (# st#, to# #)
	  | otherwise
	    = case (readDoubleArray#  from# cur#     st#) of { (# s2#, ele #) ->
	      case (writeDoubleArray# to#   cur# ele s2#) of { s3# ->
	      copy (cur# +# 1#) end# from# to# s3#
	      }}
\end{code}

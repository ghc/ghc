%
% (c) The AQUA Project, Glasgow University, 1997
%
\section[MutableArray]{The @MutableArray@ interface}

Mutable (byte)arrays interface, re-exports type types and operations
over them from @ArrBase@. Have to be used in conjunction with
@ST@.

\begin{code}
module MutableArray 
   (
    MutableArray(..),        -- not abstract
    MutableByteArray(..),

    ST,
    Ix,

    -- Creators:
    newArray,           -- :: Ix ix => (ix,ix) -> elt -> ST s (MutableArray s ix elt)
    newCharArray,
    newAddrArray,
    newIntArray,
    newFloatArray,
    newDoubleArray,
    newStablePtrArray,  -- :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 

    boundsOfArray,      -- :: Ix ix => MutableArray s ix elt -> (ix, ix)  
    boundsOfByteArray,  -- :: Ix ix => MutableByteArray s ix -> (ix, ix)

    readArray,   	-- :: Ix ix => MutableArray s ix elt -> ix -> ST s elt 

    readCharArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Char 
    readIntArray,       -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Int
    readAddrArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Addr
    readFloatArray,     -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Float
    readDoubleArray,    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Double
    readStablePtrArray, -- :: Ix ix => MutableByteArray s ix -> ix -> ST s (StablePtr a)

    writeArray,  	  -- :: Ix ix => MutableArray s ix elt -> ix -> elt -> ST s () 
    writeCharArray,       -- :: Ix ix => MutableByteArray s ix -> ix -> Char -> ST s () 
    writeIntArray,        -- :: Ix ix => MutableByteArray s ix -> ix -> Int  -> ST s () 
    writeAddrArray,       -- :: Ix ix => MutableByteArray s ix -> ix -> Addr -> ST s () 
    writeFloatArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> Float -> ST s () 
    writeDoubleArray,     -- :: Ix ix => MutableByteArray s ix -> ix -> Double -> ST s () 
    writeStablePtrArray,  -- :: Ix ix => MutableByteArray s ix -> ix -> StablePtr a -> ST s () 

    freezeArray,	  -- :: Ix ix => MutableArray s ix elt -> ST s (Array ix elt)
    freezeCharArray,      -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeIntArray,       -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeAddrArray,      -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeFloatArray,     -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeDoubleArray,    -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeStablePtrArray, -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

    unsafeFreezeArray,     -- :: Ix ix => MutableArray s ix elt -> ST s (Array ix elt)  
    unsafeFreezeByteArray, -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    thawArray,             -- :: Ix ix => Array ix elt -> ST s (MutableArray s ix elt)

     -- the sizes are reported back are *in bytes*.
    sizeofByteArray,	    -- :: Ix ix => ByteArray ix -> Int
    sizeofMutableByteArray, -- :: Ix ix => MutableByteArray s ix -> Int

{-
    readWord8Array,	    -- :: Ix ix => MutableByteArray s ix -> Word8
    readWord16Array,	    -- :: Ix ix => MutableByteArray s ix -> Word16
    readWord32Array,	    -- :: Ix ix => MutableByteArray s ix -> Word32
-}
    ) where

import PrelArr
import PrelBase ( sizeofMutableByteArray#, sizeofByteArray#
		, Int(..), Int#, (+#), (==#)
		, StablePtr#, MutableByteArray#, State#
		, unsafeFreezeByteArray#
		, newStablePtrArray#, readStablePtrArray#
		, indexStablePtrArray#, writeStablePtrArray#
		)

import PrelForeign
import PrelST
import ST
import Ix

\end{code}

Note: the absence of operations to read/write ForeignObjs to a mutable
array is not accidental; storing foreign objs in a mutable array is
not supported.

\begin{code}
sizeofByteArray :: Ix ix => ByteArray ix -> Int
sizeofByteArray (ByteArray _ arr#) = 
  case (sizeofByteArray# arr#) of
    i# -> (I# i#)

sizeofMutableByteArray :: Ix ix => MutableByteArray s ix -> Int
sizeofMutableByteArray (MutableByteArray _ arr#) = 
  case (sizeofMutableByteArray# arr#) of
    i# -> (I# i#)

\end{code}

\begin{code}
newStablePtrArray :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 
newStablePtrArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newStablePtrArray# n# s#) of { StateAndMutableByteArray# s2# barr# ->
    STret s2# (MutableByteArray ixs barr#) }}

readStablePtrArray    :: Ix ix => MutableByteArray s ix -> ix -> ST s (StablePtr a)
readStablePtrArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n)	    	    	  of { I# n# ->
    case readStablePtrArray# barr# n# s#  of { StateAndStablePtr# s2# r# ->
    STret s2# (StablePtr r#) }}

indexStablePtrArray    :: Ix ix => ByteArray ix -> ix -> (StablePtr a)
indexStablePtrArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexStablePtrArray# barr# n# 	of { r# ->
    (StablePtr r#)}}

writeStablePtrArray    :: Ix ix => MutableByteArray s ix -> ix -> StablePtr a  -> ST s () 
writeStablePtrArray (MutableByteArray ixs barr#) n (StablePtr sp#) = ST $ \ s# ->
    case (index ixs n)	    	    	       of { I# n# ->
    case writeStablePtrArray# barr# n# sp# s#  of { s2#   ->
    STret s2# () }}

freezeStablePtrArray    :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeStablePtrArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    STret s2# (ByteArray ixs frozen#) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newStablePtrArray# n# s#)    of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | otherwise
	    = case (readStablePtrArray#  from# cur#       s#) of { StateAndStablePtr# s1# ele ->
	      case (writeStablePtrArray# to#   cur# ele  s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

\end{code}


begin{code}
readWord8Array  :: Ix ix => MutableByteArray RealWorld ix -> ix -> IO Word8
readWord16Array :: Ix ix => MutableByteArray RealWorld ix -> ix -> IO Word16
readWord32Array :: Ix ix => MutableByteArray RealWorld ix -> ix -> IO Word32

{- NB!!: The index for an array is in units of the element type being read -}

readWord8Array (MutableByteArray ixs arr#) n@(I# n#) =
    case sizeofMutableByteArray# arr#   of 
      I# bytes# 
       | n# ># (bytes# -# 1#) -> fail (userError "readWord8Array: index out of bounds "++show n)
       | otherwise            -> IO $ \ s# ->
         case readCharArray# barr# n# s#  of 
           StateAndChar# s2# r# -> IOok s2# (W8# (int2Word# (ord# r#)))

readWord16Array (MutableByteArray ixs arr#) n@(I# n#) =
    case sizeofMutableByteArray# arr#   of 
      I# bytes# 
       | (2# *# n#) ># (bytes# -# 1#) -> fail (userError "readWord16Array: index out of bounds "++show n)
       | otherwise                    -> IO $ \ s# ->
         case readWordArray# barr# n# s#  of 
           StateAndInt# s2# w# -> IOok s2# (wordToWord16 (W# w#))

readWord32Array (MutableByteArray ixs arr#) n@(I# n#) =
    case sizeofMutableByteArray# arr#   of 
      I# bytes# 
       | (4# *# n#) ># (bytes# -# 1#) -> fail (userError "readWord32Array: index out of bounds "++show n)
       | otherwise                    -> IO $ \ s# ->
         case readWordArray# barr# n# s#  of 
           StateAndInt# s2# w# -> IOok s2# (wordToWord32 (W# w#))

end{code}

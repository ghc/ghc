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
    newDoubleArray,     -- :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 

    boundsOfArray,      -- :: Ix ix => MutableArray s ix elt -> (ix, ix)  
    boundsOfByteArray,  -- :: Ix ix => MutableByteArray s ix -> (ix, ix)

    readArray,   	-- :: Ix ix => MutableArray s ix elt -> ix -> ST s elt 

    readCharArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Char 
    readIntArray,       -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Int
    readAddrArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Addr
    readFloatArray,     -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Float
    readDoubleArray,    -- :: Ix ix => MutableByteArray s ix -> ix -> ST s Double

    writeArray,  	-- :: Ix ix => MutableArray s ix elt -> ix -> elt -> ST s () 
    writeCharArray,     -- :: Ix ix => MutableByteArray s ix -> ix -> Char -> ST s () 
    writeIntArray,      -- :: Ix ix => MutableByteArray s ix -> ix -> Int  -> ST s () 
    writeAddrArray,     -- :: Ix ix => MutableByteArray s ix -> ix -> Addr -> ST s () 
    writeFloatArray,    -- :: Ix ix => MutableByteArray s ix -> ix -> Float -> ST s () 
    writeDoubleArray,   -- :: Ix ix => MutableByteArray s ix -> ix -> Double -> ST s () 

    freezeArray,	-- :: Ix ix => MutableArray s ix elt -> ST s (Array ix elt)
    freezeCharArray,    -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeIntArray,     -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeAddrArray,    -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeFloatArray,   -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
    freezeDoubleArray,  -- :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

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
import PrelBase (sizeofMutableByteArray#, sizeofByteArray#, Int(..) )
import ST
import Ix

\end{code}

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

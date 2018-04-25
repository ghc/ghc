{-# LANGUAGE ParallelListComp #-}

module OddEvenFlat
	( bitReverse
	, bitReverse8
	, bitReverse16
	, bitReverse32 )
where
import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Vector	as V
import Data.Vector		(Vector)

-- | Get the list of bits in this word of a given length.
bits :: Int -> Int -> [Bool]
bits len x 
	= map (testBit x) [0 .. (len - 1)]


-- | Bit reverse an arbitrary int (slowly)
bitReverse :: Int -> Int -> Int
bitReverse len xx
	= foldl (\acc ix -> acc + bit ix) 0
	$ catMaybes
	$ [ if b then Just ix else Nothing
		| b  <- reverse $ bits len xx
		| ix <- [0..] ]


-- | Table of bit-reversed 8 bit words.
table8	:: Vector Word32
table8	= V.fromList
	$ map (fromIntegral . bitReverse 8) [0..255]



-- | Bit-reverse an 8 bit word 
--   (packed in the low part of a 32 bit word)	
bitReverse8 :: Word32 -> Word32
{-# INLINE bitReverse #-}
bitReverse8 n	
	= table8 `V.unsafeIndex` (fromIntegral n)


-- | Bit-reverse a 16 bit word
--   (packed in the low part of a 32 bit word)
bitReverse16 :: Word32 -> Word32
{-# INLINE bitReverse16 #-}
bitReverse16 n
 = let	high	= (n .&. 0x0ff00) `shiftR` 8
	low	=  n .&. 0x000ff	
   in	(bitReverse8 low `shiftL` 8) .|. bitReverse8 high


-- | Bit-reverse a 32 bit word
bitReverse32 :: Word32 -> Word32
{-# INLINE bitReverse32 #-}
bitReverse32 n
 = let	high	= (n .&. 0xffff0000) `shiftR` 16
	low	=  n .&. 0x0000ffff
   in	(bitReverse16 low `shiftL` 16) .|. bitReverse16 high


-- | Bit-reverse a word of a given length, 
--   which is packed in the low part of 
bitReverseN :: Int -> Word32 -> Word32
bitReverseN len n
	= bitReverse32 n `shiftR` (32 - (fromIntegral len))

bitReverseIntN :: Int -> Int -> Int
bitReverseIntN len ix
	= fromIntegral $ bitReverseN len $ fromIntegral ix


-- | Permute an array by bit-wise reversing the index of each element.
--   The length of the array must be a power of two.
permReverse :: Vector Word32 -> Vector Word32
permReverse arr
 = let	-- Length of the array index in bits.
	ixLen	= truncate 
		$ log (fromIntegral $ V.length arr) / log 2

   in	V.generate 
		(V.length arr) 
		(\ix -> arr `V.unsafeIndex` bitReverseIntN ixLen ix)





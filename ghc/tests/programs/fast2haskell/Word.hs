-- mimic "hbc_library" module, Word.
-- [seriously non-std Haskell here]

module Word2 (
	Bits(..),		-- class
	Byte, Short, Word,	-- data types: abstract
	byteToInt, shortToInt, wordToInt
    ) where

import GHC
import PrelBase

infixl 8 `bitLsh`, `bitRsh`
infixl 7 `bitAnd`
infixl 6 `bitXor`
infixl 5 `bitOr`

class Bits a where
	bitAnd, bitOr, bitXor :: a -> a -> a
	bitCompl :: a -> a
	bitRsh, bitLsh :: a -> Int -> a
	bitSwap :: a -> a
	bit0 :: a
	bitSize :: a -> Int

------------------------------------------------------------------
data Word = Word Word# deriving (Eq, Ord)

instance Bits Word where
	bitAnd (Word x) (Word y) = case and# x y of z -> Word z
	bitOr  (Word x) (Word y) = case or#  x y of z -> Word z
	bitXor (Word x) (Word y) = error "later..." -- Word (XOR x y)
	bitCompl (Word x)        = case not# x of x' -> Word x'
	bitLsh (Word x) (I# y)	 = case shiftL# x y of z -> Word z
	bitRsh (Word x) (I# y)	 = case shiftRA# x y of z -> Word z
        bitSwap (Word x)         = --Word (OR (LSH x 16) (AND (RSH x 16) 65535))
				   case shiftL# x 16# of { a# ->
				   case shiftRA# x 16# of { b# ->
				   case and# b# (i2w 65535#) of { c# ->
				   case or#  a# c# of  { r# ->
				   Word r# }}}}
	bit0                     = Word (i2w 1#)
	bitSize (Word _)	 = 32

w2i x = word2Int# x
i2w x = int2Word# x

instance Num Word where
	Word x + Word y = case plusInt#  (w2i x) (w2i y) of z -> Word (i2w z)
	Word x - Word y = case minusInt# (w2i x) (w2i y) of z -> Word (i2w z)
	Word x * Word y = case timesInt# (w2i x) (w2i y) of z -> Word (i2w z)
	negate (Word x) = case negateInt# (w2i x)  of z -> Word (i2w z)
	fromInteger (J# a# s# d#)
	  = case integer2Int# a# s# d# of { z# ->
	    Word (i2w z#) }

instance Show Word where
	showsPrec _ (Word w) =
		let i = toInteger (I# (w2i w)) + (if geWord# w (i2w 0#) then 0 else  2*(toInteger maxBound + 1))
		in  showString (conv 8 i)

conv :: Int -> Integer -> String
conv 0 _ = ""

-- Was: 
-- 	conv n i = conv (n-1) q ++ ["0123456789ABCDEF"!!r] where (q, r) = quotRem (fromInteger i) 16
-- But !!'s type has changed (Haskell 1.3) to take an Int index

conv n i = conv (n-1) q ++ ["0123456789ABCDEF"!!fromInteger r] 
         where 
	   (q, r) = quotRem i 16

------------------------------------------------------------------
data Short = Short Int# deriving (Eq, Ord)

------------------------------------------------------------------
data Byte = Byte Int# deriving (Eq, Ord)

------------------------------------------------------------------
wordToInt :: Word -> Int
wordToInt (Word w) = I# (w2i w)

shortToInt :: Short -> Int
shortToInt (Short w) = I# w

byteToInt :: Byte -> Int
byteToInt (Byte w) = I# w

-- mimic "hbc_library" module, Word.
-- [seriously non-std Haskell here]
--
module Word (
	Bits(..),		-- class
	Byte, Short, Word,	-- data types: abstract
	byteToInt, shortToInt, wordToInt,
	wordToShorts, wordToBytes, bytesToString
    ) where

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
	bitLsh (Word x) (I# y)	 = case shiftL#  x y of z -> Word z
	bitRsh (Word x) (I# y)	 = case shiftRL# x y of z -> Word z
        bitSwap (Word x)         = --Word (OR (LSH x 16) (AND (RSH x 16) 65535))
				   case shiftL#  x 16# of { a# ->
				   case shiftRL# x 16# of { b# ->
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
	fromInt (I# x) = Word (i2w x)

instance Text Word where
	showsPrec _ (Word w) =
		let i = toInteger (I# (w2i w)) + (if geWord# w (i2w 0#) then 0 else  2*(toInteger maxInt + 1))
		in  showString (conv 8 i)

conv :: Int -> Integer -> String
conv 0 _ = ""
conv n i = conv (n-1) q ++ ["0123456789ABCDEF"!!r] where (q, r) = quotRem i 16

------------------------------------------------------------------
data Short = Short Word# deriving (Eq, Ord)

sHORTMASK x = and# x (i2w 65535#)

instance Bits Short where
    bitAnd (Short x) (Short y) = case and# x y of z -> Short z
    bitOr  (Short x) (Short y) = case or#  x y of z -> Short z
    bitXor (Short x) (Short y) = error "later..." -- Short (XOR x y)
    bitCompl (Short x)         = case not# x of x' -> Short (sHORTMASK x')
    bitLsh (Short x) (I# y)    = case shiftL#  x y of z -> Short (sHORTMASK z)
    bitRsh (Short x) (I# y)    = case shiftRL# x y of z -> Short z
    bitSwap (Short x)          = --Short (SHORTMASK(OR (LSH x 8) (AND (RSH x 8) 255)))
				 case shiftL#  x 8# of { a# ->
				 case shiftRL# x 8# of { b# ->
				 case and# b# (i2w 255#) of { c# ->
				 case or#  a# c# of  { r# ->
				 Short (sHORTMASK r#) }}}}
    bit0                       = Short (i2w 1#)
    bitSize (Short _)	       = 16

instance Num Short where
    Short x + Short y = case plusInt#  (w2i x) (w2i y) of z -> Short (sHORTMASK (i2w z))
    Short x - Short y = case minusInt# (w2i x) (w2i y) of z -> Short (sHORTMASK (i2w z))
    Short x * Short y = case timesInt# (w2i x) (w2i y) of z -> Short (sHORTMASK (i2w z))
    negate (Short x) = case negateInt# (w2i x)  of z -> Short (sHORTMASK (i2w z))
    fromInteger (J# a# s# d#)
      = case integer2Int# a# s# d# of { z# ->
	Short (sHORTMASK (i2w z#)) }
    fromInt (I# x) = Short (sHORTMASK (i2w x))

instance Text Short where
	showsPrec _ (Short w) =
		let i = toInteger (I# (w2i w))
		in  showString (conv 4 i)
--	showsType _ = showString "Short"

------------------------------------------------------------------
data Byte = Byte Word# deriving (Eq, Ord)

bYTEMASK x = and# x (i2w 255#)

instance Bits Byte where
    bitAnd (Byte x) (Byte y) = case and# x y of z -> Byte z
    bitOr  (Byte x) (Byte y) = case or#  x y of z -> Byte z
    bitXor (Byte x) (Byte y) = error "later..." -- Byte (XOR x y)
    bitCompl (Byte x)         = case not# x of x' -> Byte (bYTEMASK x')
    bitLsh (Byte x) (I# y)    = case shiftL#  x y of z -> Byte (bYTEMASK z)
    bitRsh (Byte x) (I# y)    = case shiftRL# x y of z -> Byte z
    bitSwap (Byte x)          = --Byte (BYTEMASK(OR (LSH x 4) (AND (RSH x 8) 15)))
				 case shiftL#  x 4# of { a# ->
				 case shiftRL# x 8# of { b# ->
				 case and# b# (i2w 15#) of { c# ->
				 case or#  a# c# of  { r# ->
				 Byte (bYTEMASK r#) }}}}
    bit0                       = Byte (i2w 1#)
    bitSize (Byte _)	       = 8

instance Num Byte where
    Byte x + Byte y = case plusInt#  (w2i x) (w2i y) of z -> Byte (bYTEMASK (i2w z))
    Byte x - Byte y = case minusInt# (w2i x) (w2i y) of z -> Byte (bYTEMASK (i2w z))
    Byte x * Byte y = case timesInt# (w2i x) (w2i y) of z -> Byte (bYTEMASK (i2w z))
    negate (Byte x) = case negateInt# (w2i x)  of z -> Byte (bYTEMASK (i2w z))
    fromInteger (J# a# s# d#)
      = case integer2Int# a# s# d# of { z# ->
	Byte (bYTEMASK (i2w z#)) }
    fromInt (I# x) = Byte (bYTEMASK (i2w x))

instance Text Byte where
	showsPrec _ (Byte w) =
		let i = toInteger (I# (w2i w))
		in  showString (conv 2 i)
--	showsType _ = showString "Byte"

------------------------------------------------------------------
wordToShorts (Word w) = [Short (sHORTMASK(shiftRL# w 16#)), Short (sHORTMASK(w))]
wordToBytes  (Word w) = [Byte  (bYTEMASK(shiftRL#  w 24#)), Byte  (bYTEMASK(shiftRL#  w 16#)), Byte (bYTEMASK(shiftRL#  w 8#)), Byte (bYTEMASK(w))]

bytesToString :: [Byte] -> String
bytesToString bs = map (\ (Byte b) -> chr (I# (w2i b))) bs

stringToBytes :: String -> [Byte]
stringToBytes cs = map (\c -> Byte (case ord c of {I# i -> bYTEMASK (i2w i)})) cs

wordToInt :: Word -> Int
wordToInt (Word w) = I# (w2i w)

shortToInt :: Short -> Int
shortToInt (Short w) = I# (w2i w)

byteToInt :: Byte -> Int
byteToInt (Byte w) = I# (w2i w)

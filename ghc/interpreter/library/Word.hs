-----------------------------------------------------------------------------
-- Unsigned Integers
-- Suitable for use with Hugs 1.4 on 32 bit systems.
-----------------------------------------------------------------------------
module Word
	( Word8
	, Word16
	, Word32
	, Word64
	, word8ToWord32  -- :: Word8  -> Word32
	, word32ToWord8  -- :: Word32 -> Word8
	, word16ToWord32 -- :: Word16 -> Word32
	, word32ToWord16 -- :: Word32 -> Word16
	, word8ToInt     -- :: Word8  -> Int
	, intToWord8     -- :: Int    -> Word8
	, word16ToInt    -- :: Word16 -> Int
	, intToWord16    -- :: Int    -> Word16
	, word32ToInt    -- :: Word32 -> Int
	, intToWord32    -- :: Int    -> Word32
	) where

import PreludeBuiltin
import Bits

-----------------------------------------------------------------------------
-- The "official" coercion functions
-----------------------------------------------------------------------------

word8ToWord32  :: Word8  -> Word32
word32ToWord8  :: Word32 -> Word8
word16ToWord32 :: Word16 -> Word32
word32ToWord16 :: Word32 -> Word16

word8ToInt   :: Word8  -> Int
intToWord8   :: Int    -> Word8
word16ToInt  :: Word16 -> Int
intToWord16  :: Int    -> Word16
word32ToInt :: Word32 -> Int
intToWord32 :: Int    -> Word32

word8ToInt  = word32ToInt    . word8ToWord32
intToWord8  = word32ToWord8  . intToWord32
word16ToInt = word32ToInt    . word16ToWord32
intToWord16 = word32ToWord16 . intToWord32

word32ToInt (W32 x) = primWordToInt x
intToWord32 x       = W32 (primIntToWord x)


-----------------------------------------------------------------------------
-- Word8
-----------------------------------------------------------------------------

newtype Word8  = W8 Word32

word8ToWord32 (W8 x) = x .&. 0xff
word32ToWord8 = W8

instance Eq  Word8     where (==)    = binop (==)
instance Ord Word8     where compare = binop compare

instance Num Word8 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . fromInteger
    fromInt       = intToWord8

instance Bounded Word8 where
    minBound = 0
    maxBound = 0xff

instance Real Word8 where
    toRational x = toInteger x % 1

instance Integral Word8 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    divMod        = quotRem
    toInteger     = toInteger . from
    toInt         = word8ToInt

instance Ix Word8 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = word32ToInt (from (i - m))
	   | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word8 where
    toEnum         = to . intToWord32
    fromEnum       = word32ToInt . from
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word8)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word8)]
		       where last = if d < c then minBound else maxBound

instance Read Word8 where
    readsPrec p = readDec

instance Show Word8 where
    showsPrec p = showInt . toInteger -- a particularily counterintuitive name!

instance Bits Word8 where
  x .&. y       = to (binop (.&.) x y)
  x .|. y       = to (binop (.|.) x y)
  x `xor` y     = to (binop xor x y)
  complement    = to . complement . from
  x `shift` i   = to (from x `shift` i)
--  rotate      
  bit           = to . bit
  setBit x i    = to (setBit (from x) i)
  clearBit x i  = to (clearBit (from x) i)
  complementBit x i = to (complementBit (from x) i)
  testBit x i   = testBit (from x) i
  bitSize  _    = 8
  isSigned _    = False

-----------------------------------------------------------------------------
-- Word16
-----------------------------------------------------------------------------

newtype Word16 = W16 Word32

word16ToWord32 (W16 x) = x .&. 0xffff
word32ToWord16 = W16

instance Eq  Word16     where (==)    = binop (==)
instance Ord Word16     where compare = binop compare

instance Num Word16 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . fromInteger
    fromInt       = intToWord16

instance Bounded Word16 where
    minBound = 0
    maxBound = 0xffff

instance Real Word16 where
  toRational x = toInteger x % 1

instance Integral Word16 where
  x `div` y     = to  (binop div x y)
  x `quot` y    = to  (binop quot x y)
  x `rem` y     = to  (binop rem x y)
  x `mod` y     = to  (binop mod x y)
  x `quotRem` y = to2 (binop quotRem x y)
  divMod        = quotRem
  toInteger     = toInteger . from
  toInt         = word16ToInt

instance Ix Word16 where
  range (m,n)          = [m..n]
  index b@(m,n) i
         | inRange b i = word32ToInt (from (i - m))
         | otherwise   = error "index: Index out of range"
  inRange (m,n) i      = m <= i && i <= n

instance Enum Word16 where
  toEnum         = to . intToWord32
  fromEnum       = word32ToInt . from
  enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word16)]
  enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word16)]
		       where last = if d < c then minBound else maxBound

instance Read Word16 where
  readsPrec p = readDec

instance Show Word16 where
  showsPrec p = showInt . toInteger -- a particularily counterintuitive name!

instance Bits Word16 where
  x .&. y       = to (binop (.&.) x y)
  x .|. y       = to (binop (.|.) x y)
  x `xor` y     = to (binop xor x y)
  complement    = to . complement . from
  x `shift` i   = to (from x `shift` i)
--  rotate      
  bit           = to . bit
  setBit x i    = to (setBit (from x) i)
  clearBit x i  = to (clearBit (from x) i)
  complementBit x i = to (complementBit (from x) i)
  testBit x i   = testBit (from x) i
  bitSize  _    = 16
  isSigned _    = False

-----------------------------------------------------------------------------
-- Word32
-----------------------------------------------------------------------------

newtype Word32 = W32 Word

w32 :: Word32 -> Word
w32 (W32 x) = x

lift0 :: Word -> Word32
lift1 :: (Word -> Word) -> (Word32 -> Word32)
lift2 :: (Word -> Word -> Word) -> (Word32 -> Word32 -> Word32)
lift2' :: (Word -> Word -> (Word,Word)) -> (Word32 -> Word32 -> (Word32,Word32))

lift0 x                 = W32 x
lift1 f (W32 x)         = W32 (f x)
lift2 f (W32 x) (W32 y) = W32 (f x y)

lift2' f (W32 x) (W32 y) = case f x y of (a,b) -> (W32 a, W32 b)

instance Eq  Word32 where 
  x == y  = primEqWord (w32 x) (w32 y)
  x /= y  = primNeWord (w32 x) (w32 y)

instance Ord Word32 where
  x <  y  = primLtWord (w32 x) (w32 y)
  x <= y  = primLeWord (w32 x) (w32 y)
  x >= y  = primGeWord (w32 x) (w32 y)
  x >  y  = primGtWord (w32 x) (w32 y)

instance Num Word32 where
    (+)         = lift2 primPlusWord
    (-)         = lift2 primMinusWord
    negate      = lift1 primNegateWord
    (*)         = lift2 primTimesWord
    abs         = id
    signum x    = if x == 0 then 0 else 1
    fromInteger = W32 . primIntegerToWord
    fromInt     = W32 . primIntToWord

instance Bounded Word32 where
    minBound = 0
    maxBound = W32 primMaxWord

instance Real Word32 where
    toRational x = toInteger x % 1

instance Integral Word32 where
    quotRem   = lift2' primQuotRemWord
    quot      = lift2  primQuotWord
    rem       = lift2  primRemWord
    divMod    = lift2' primQuotRemWord  -- no difference for unsigned values!
    div       = lift2  primQuotWord
    mod       = lift2  primRemWord
    toInteger = primWordToInteger . w32
    toInt     = primWordToInt     . w32

instance Ix Word32 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = word32ToInt (i - m)
	   | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word32 where
    toEnum        = fromInt
    fromEnum      = toInt

    enumFrom w              = [w .. maxBound]
    enumFromTo   w1 w2
      | w1 <= w2  = eft32 w1 w2
      | otherwise = []
    enumFromThen w1 w2      = [w1, w2 .. last]
        where 
	 last
	  | w1 < w2   = maxBound::Word32
	  | otherwise = minBound
    enumFromThenTo w1 w2 last = eftt32 w1 (w2 - w1) (>last)

--------------------------------
-- Begin stolen from GHC (but then modified!)
--------------------------------

-- Termination is easy because the step is 1
eft32 :: Word32 -> Word32 -> [Word32]
eft32 now last = go now
  where 
   go x
    | x == last = [x]
    | otherwise = x : (go `strict` (x+1))

-- Termination is hard because the step is not 1
-- Warning: this code is known not to work near maxBound
eftt32 :: Word32 -> Word32 -> (Word32->Bool) -> [Word32]
eftt32 now step done = go now
  where
   go now
     | done now  = []
     | otherwise = now : (go `strict` (now+step))

--------------------------------
-- End stolen from GHC.
--------------------------------

instance Read Word32 where
    readsPrec p = readDec

instance Show Word32 where
    showsPrec p = showInt . toInteger -- a particularily counterintuitive name!

instance Bits Word32 where
  (.&.)         = lift2 primAndWord
  (.|.)         = lift2 primOrWord
  xor           = lift2 primXorWord
  complement    = lift1 primNotWord
  shift x n     
    | n >= 0    = W32 (primShiftLWord  (w32 x) (primIntToWord n))
    | otherwise = W32 (primShiftRLWord (w32 x) (primIntToWord (-n)))
--  rotate      
  bit           = shift 1
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = x .&. bit i /= 0
  bitSize  _    = 32
  isSigned _    = False

-----------------------------------------------------------------------------
-- Word64
-----------------------------------------------------------------------------

data Word64 = W64 {lo,hi::Word32} deriving (Eq, Ord, Bounded)

w64ToInteger W64{lo,hi} = toInteger lo + 0x100000000 * toInteger hi 
integerToW64 x = case x `quotRem` 0x100000000 of 
                 (h,l) -> W64{lo=fromInteger l, hi=fromInteger h}

instance Show Word64 where
  showsPrec p = showInt . w64ToInteger

instance Read Word64 where
  readsPrec p s = [ (integerToW64 x,r) | (x,r) <- readDec s ]

-----------------------------------------------------------------------------
-- End of exported definitions
--
-- The remainder of this file consists of definitions which are only
-- used in the implementation.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Enumeration code: copied from Prelude
-----------------------------------------------------------------------------

numericEnumFrom        :: Real a => a -> [a]
numericEnumFromThen    :: Real a => a -> a -> [a]
numericEnumFromTo      :: Real a => a -> a -> [a]
numericEnumFromThenTo  :: Real a => a -> a -> a -> [a]
numericEnumFrom n            = n : strict numericEnumFrom (n+1)
numericEnumFromThen n m      = iterate ((m-n)+) n
numericEnumFromTo n m        = takeWhile (<= m) (numericEnumFrom n)
numericEnumFromThenTo n n' m = takeWhile (if n' >= n then (<= m) else (>= m))
                                         (numericEnumFromThen n n')

-----------------------------------------------------------------------------
-- Coercions - used to make the instance declarations more uniform
-----------------------------------------------------------------------------

class Coerce a where
  to   :: Word32 -> a
  from :: a -> Word32

instance Coerce Word8 where
  from = word8ToWord32
  to   = word32ToWord8

instance Coerce Word16 where
  from = word16ToWord32
  to   = word32ToWord16

binop :: Coerce word => (Word32 -> Word32 -> a) -> (word -> word -> a)
binop op x y = from x `op` from y

to2 :: Coerce word => (Word32, Word32) -> (word, word)
to2 (x,y) = (to x, to y)

-----------------------------------------------------------------------------
-- Code copied from the Prelude
-----------------------------------------------------------------------------

absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1

-----------------------------------------------------------------------------
-- End
-----------------------------------------------------------------------------

import Data.Bits        (FiniteBits (..), unsafeShiftL, unsafeShiftR, (.&.),
                         (.|.))
import Data.Char        (intToDigit)
import Data.Word        (Word8, Word16, Word32, Word64, bitReverse8,
                         bitReverse16, bitReverse32, bitReverse64)
import Numeric          (showIntAtBase)

-- | Given a word, check:
--
-- * if the reverse of its @String@ representation in base 2 matches the
--   @String@ representation of that word with its bit order reversed.
--   order reversed, and
-- * if reversing its bits and then reverse the resulting word's bits again
--   yields the same word.
-- Takes the bit reversion function as an argument so different word types
-- can be used with their own functions.
test :: (FiniteBits a, Integral a, Show a) => (a -> a) -> a -> Bool
test bitReverter x =
    let -- These zeroes are to left-pad the base-2 representation of
        -- @x@ so that the string has one ASCII character per bit in the
        -- word type e.g. @Word8@s produce strings with 8 characters.
        leftPad = countLeadingZeros x
        -- These zeroes are to left-pad the base-2 representation of
        -- bit-reversed @x@ so that the string has one ASCII character per bit
        -- in the word type e.g. @Word8@s produce strings with 8 characters.
        reverseLeftPad = countTrailingZeros x
        toBinaryString a = showIntAtBase 2 intToDigit a ""
        binaryX = replicate leftPad '0' ++ toBinaryString x
        revX = bitReverter x
        binaryRevX = replicate reverseLeftPad '0' ++ toBinaryString revX
        revRevX = bitReverter revX
    in (x == revRevX) && (reverse binaryX == binaryRevX)

word8s :: [Word8]
word8s = [29, 31, 61, 102, 129, 129, 153, 213, 241, 246]

word16s :: [Word16]
word16s = [555, 3298, 4548, 12557, 16464, 16481, 40722, 51736, 55009, 62554]

word32s :: [Word32]
word32s = [6585, 10944, 21639, 25202, 27228,  836732395, 848624442, 3798715760, 3909052537, 4224371164]

word64s :: [Word64]
word64s = [2451351, 5096456, 8248539, 13039372, 15656413,  367814400638368418, 15152819454280096771, 15184978641026131315, 16329695467052396714, 17634654963076276082]

main :: IO ()
main = do
    let printer f = mapM_ (print . test f)
    printer bitReverse8 word8s
    printer bitReverse16 word16s
    printer bitReverse32 word32s
    printer bitReverse64 word64s
import Data.Bits
import Data.Int
import Data.Word

unsafeShift32R :: (Bits a, Num a) => a -> a
unsafeShift32R x = unsafeShiftR x 32

main :: IO ()
main = do
    print $ map unsafeShift32R [ 123456, 0x7fffffff :: Int ]
    print $ map unsafeShift32R [ -123456, -0x80000000 :: Int ]
    print $ map unsafeShift32R [ 123456, 0xffffffff :: Word ]

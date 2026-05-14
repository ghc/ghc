import Data.Bits (shiftR)
import Data.Word (Word8, Word16)

main :: IO ()
main = do
    print (shiftR (0x80 :: Word8) 1)
    print (shiftR (0xFF :: Word8) 4)
    print (shiftR (0x8000 :: Word16) 1)
    print (shiftR (0xFFFF :: Word16) 8)

import Data.Bits
import Data.Word
import Debug.Trace

fl :: Word64 -> Word64 -> Word64
fl fin sk =
	let (x1, x2) = w64tow32 fin in
	let (k1, k2) = w64tow32 sk in
	let y2 = x2 `xor` ((x1 .&. k1) `rotateL` 1) in
	let y1 = x1 `xor` (y2 .|. k2) in
	trace (show fin ++ " " ++ show sk ++ " -> " ++ show (w32tow64 (y1, y2))) $ w32tow64 (y1, y2)

w64tow32 :: Word64 -> (Word32, Word32)
w64tow32 w = (fromIntegral (w `shiftR` 32), fromIntegral (w .&. 0xffffffff))

w32tow64 :: (Word32, Word32) -> Word64
w32tow64 (x1, x2) = ((fromIntegral x1) `shiftL` 32) .|. (fromIntegral x2)

a, b :: Word64
a = 1238988323332265734
b = 11185553392205053542

main :: IO ()
main =
    putStrLn $ "fl " ++ show a ++ " " ++ show b ++ " -> " ++ show (fl a b)

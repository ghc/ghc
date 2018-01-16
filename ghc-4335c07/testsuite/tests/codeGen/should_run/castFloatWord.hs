import Data.Bits
import GHC.Float
import GHC.Word
import Numeric

main :: IO ()
main = do
    putStrLn "Float"
    mapM_ print floats
    putStrLn "\nDouble"
    mapM_ print doubles
    putStrLn "\nWord32"
    mapM_ (printHex32 . castFloatToWord32) floats
    putStrLn "\nWord64"
    mapM_ (printHex64 . castDoubleToWord64) doubles
    putStrLn "Done!"

floats :: [Float]
floats = map castWord32ToFloat $ 0 : map (2^) [ 0 .. 31 ]

doubles :: [Double]
doubles = map castWord64ToDouble $ 0 : map (2^) [ 0 .. 63 ]

printHex32 :: Word32 -> IO ()
printHex32 w = putStrLn $ "0x" ++ showHex (0xffffffff .&. w) ""

printHex64 :: Word64 -> IO ()
printHex64 w = putStrLn $ "0x" ++ showHex w ""

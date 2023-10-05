import Numeric
import GHC.Float

main = do
  putStrLn (showHex (castFloatToWord32 (abs (castWord32ToFloat 0xFFC00000))) "")
  putStrLn (showHex (castDoubleToWord64 (abs (castWord64ToDouble 0xFFF8000000000000))) "")

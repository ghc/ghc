{-# LANGUAGE MagicHash #-}
import GHC.Exts
import GHC.Word
import Numeric

main :: IO ()
main = do
  putStrLn $ showHex (W# (bitReverse8# 0x55##)) ""
  putStrLn $ showHex (W# (bitReverse8# 0xAA##)) ""
  putStrLn $ showHex (W# (bitReverse16# 0x5555##)) ""
  putStrLn $ showHex (W# (bitReverse16# 0xAAAA##)) ""
  putStrLn $ showHex (W# (bitReverse32# 0x5555_5555##)) ""
  putStrLn $ showHex (W# (bitReverse32# 0xAAAA_AAAA##)) ""
  putStrLn $ showHex (W# (byteSwap16# 0xABCD##)) ""
  putStrLn $ showHex (W# (byteSwap32# 0xABCD_EF12##)) ""
  putStrLn $ showHex (W# (noinline bitReverse8# 0x55##)) ""
  putStrLn $ showHex (W# (noinline bitReverse8# 0xAA##)) ""
  putStrLn $ showHex (W# (noinline bitReverse16# 0x5555##)) ""
  putStrLn $ showHex (W# (noinline bitReverse16# 0xAAAA##)) ""
  putStrLn $ showHex (W# (noinline bitReverse32# 0x5555_5555##)) ""
  putStrLn $ showHex (W# (noinline bitReverse32# 0xAAAA_AAAA##)) ""
  putStrLn $ showHex (W# (noinline byteSwap16# 0xABCD##)) ""
  putStrLn $ showHex (W# (noinline byteSwap32# 0xABCD_EF12##)) ""

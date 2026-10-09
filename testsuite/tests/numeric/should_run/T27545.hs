{-# LANGUAGE MagicHash, NegativeLiterals #-}

import GHC.Exts
import GHC.Word
import Numeric

main :: IO ()
main = do
  putStrLn $ " 0.0#  bits = 0x" ++ showHex (W32# (castFloatToWord32#   0.0# )) ""
  putStrLn $ "-0.0#  bits = 0x" ++ showHex (W32# (castFloatToWord32#  -0.0# )) ""
  putStrLn $ " 0.0## bits = 0x" ++ showHex (W64# (castDoubleToWord64#  0.0##)) ""
  putStrLn $ "-0.0## bits = 0x" ++ showHex (W64# (castDoubleToWord64# -0.0##)) ""

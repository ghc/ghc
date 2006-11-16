{-# OPTIONS_GHC -fglasgow-exts #-}

-- Actually, this exercises prelude/PrelRules, but this is the closest
-- place I could find to put it...

import GHC.Exts
import Numeric
import Data.Bits

main = do phex (I# (uncheckedIShiftL#  (negateInt# 5#) 2#))
          phex (I# (uncheckedIShiftRA# (negateInt# 5#) 1#))
          phex (I# (uncheckedIShiftRL# (negateInt# 5#) 1#))
          phex (W# (uncheckedShiftL#   (int2Word# (negateInt# 5#)) 2#))
          phex (W# (uncheckedShiftRL#  (int2Word# (negateInt# 5#)) 1#))

phex x = putStrLn (showSigned (\x -> ("0x"++) . showHex x) 0 x "")

{- Too wordsize-dependant
phex x = putStrLn (hex x)
hex x = "0x" ++ [onedigit (fromIntegral ((x `shiftR` (i*4)) .&. 0xF))
                 | i <- [digits-1,digits-2..0]]
    where
    digits = bitSize x `div` 4
    onedigit n = "0123456789abcdef" !! n
-}



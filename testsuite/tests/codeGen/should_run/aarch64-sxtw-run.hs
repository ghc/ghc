{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Main where

import GHC.Exts
import Data.Bits (shiftR, (.&.))

foreign import prim "runMul2W32zh" runMul2W32# :: Word# -> Word#
foreign import prim "runMul2W16zh" runMul2W16# :: Word# -> Word#
foreign import prim "runMul2W8zh"  runMul2W8#  :: Word# -> Word#
foreign import prim "runMul2W16Overflowzh"
    runMul2W16Overflow# :: Word# -> Word# -> Word#

mul2W32 :: Word -> Word
mul2W32 (W# x) = W# (runMul2W32# x)

mul2W16 :: Word -> Word
mul2W16 (W# x) = W# (runMul2W16# x)

mul2W8 :: Word -> Word
mul2W8 (W# x) = W# (runMul2W8# x)

mul2W16Overflow :: Word -> Word -> (Word, Word)
mul2W16Overflow (W# x) (W# y) =
    let r = W# (runMul2W16Overflow# x y)
        needed = r `shiftR` 32
        lo = r .&. 0xFFFF
    in (needed, lo)

main :: IO ()
main = do
    -- W32: 50000 has bit 15 set, bit 31 clear. The old SXTH bug would
    -- sign-extend from bit 15, corrupting the value before SMULL.
    putStrLn $ "W32 50000*2 = " ++ show (mul2W32 50000)
    putStrLn $ "W32 1*2 = " ++ show (mul2W32 1)
    putStrLn $ "W32 70000*2 = " ++ show (mul2W32 70000)

    -- W16: exercises signExtendReg W16 W32 (SXTH before SMULL).
    putStrLn $ "W16 200*2 = " ++ show (mul2W16 200)
    putStrLn $ "W16 100*2 = " ++ show (mul2W16 100)

    -- W8: exercises signExtendReg W8 W32 (SXTB before SMULL).
    putStrLn $ "W8 50*2 = " ++ show (mul2W8 50)
    putStrLn $ "W8 3*2 = " ++ show (mul2W8 3)

    -- Overflow detection (CSET NE fix):
    -- 200 * 200 = 40000, overflows I16 [-32768, 32767]. needed=1.
    let (n1, lo1) = mul2W16Overflow 200 200
    putStrLn $ "W16 200*200: needed=" ++ show n1 ++ " lo=" ++ show lo1
    -- 100 * 2 = 200, fits in I16. needed=0.
    let (n2, lo2) = mul2W16Overflow 100 2
    putStrLn $ "W16 100*2: needed=" ++ show n2 ++ " lo=" ++ show lo2
    -- 181 * 181 = 32761, just under 32767. needed=0.
    let (n3, lo3) = mul2W16Overflow 181 181
    putStrLn $ "W16 181*181: needed=" ++ show n3 ++ " lo=" ++ show lo3
    -- 182 * 182 = 33124, just over 32767. needed=1.
    let (n4, lo4) = mul2W16Overflow 182 182
    putStrLn $ "W16 182*182: needed=" ++ show n4 ++ " lo=" ++ show lo4

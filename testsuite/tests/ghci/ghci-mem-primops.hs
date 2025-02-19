{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExtendedLiterals #-}

module Main where

-- Test memory primops interpreted in interpreter, extend if you add more.
import GHC.Word
import GHC.PrimOps
import GHC.IO
import Numeric (showHex)

data Bytes = Bytes { byte_addr :: Addr# }

bytes :: Bytes
bytes = Bytes "\0\1\2\3\4\5\6\7\8\0"#

main = do
    let val = 0x1122334455667788#Word64
    IO (\s -> case writeWord64OffAddr# (byte_addr bytes) 0# val s of s2 -> (# s2,() #))
    putStrLn . flip showHex "" $ W64# (indexWord64OffAddr# (byte_addr bytes) 0#)

    IO (\s -> case writeWord32OffAddr# (byte_addr bytes) 0# 0x11223344#Word32 s of s2 -> (# s2,() #))
    putStrLn . flip showHex "" $ W32# (indexWord32OffAddr# (byte_addr bytes) 0#)

    IO (\s -> case writeWord16OffAddr# (byte_addr bytes) 0# 0x1122#Word16 s of s2 -> (# s2,() #))
    putStrLn . flip showHex "" $ W16# (indexWord16OffAddr# (byte_addr bytes) 0#)

    IO (\s -> case writeWord8OffAddr# (byte_addr bytes) 0# 0x11#Word8 s of s2 -> (# s2,() #))
    putStrLn . flip showHex "" $ W8# (indexWord8OffAddr# (byte_addr bytes) 0#)
{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

-- Verifies correctness of built-in rewrite rules that constant fold
-- applications of the primops (index|read)Word(8|16|32)OffAddr#
-- to Addr# literals. These rewrite rules are unusual because their
-- behavior depends on the endiannes of the target platform.

import Data.Char
import GHC.ByteOrder
import GHC.Exts
import GHC.Word
import GHC.IO
import Numeric

main :: IO ()
main = do
  let !person = "person"#
      !stringOfManyLetters = "stringOfManyLetters"#
  printHex (indexWord8Array "hello"# 0)
  printHex =<< readWord8Array "hello"# 0
  printHex (indexWord8Array "hello"# 1)
  printHex =<< readWord8Array "hello"# 1
  printHex (indexWord8Array "hello"# 4)
  printHex =<< readWord8Array "hello"# 4
  printHex (indexWord8Array person 5)
  printHex =<< readWord8Array person 5
  printHex (indexWord16ArrayBE "hello"# 1)
  printHex =<< readWord16ArrayBE "hello"# 1
  printHex (indexWord16ArrayBE person 1)
  printHex =<< readWord16ArrayBE person 1
  printHex (indexWord32ArrayBE person 0)
  printHex =<< readWord32ArrayBE person 0
  printHex (indexWord32ArrayBE stringOfManyLetters 0)
  printHex =<< readWord32ArrayBE stringOfManyLetters 0
  printHex (indexWord32ArrayBE stringOfManyLetters 1)
  printHex =<< readWord32ArrayBE stringOfManyLetters 1

printHex w = putStrLn (showIntAtBase 16 intToDigit w "")

indexWord8Array :: Addr# -> Int -> Word8
indexWord8Array a (I# i) = W8# (indexWord8OffAddr# a i)

indexWord16ArrayBE :: Addr# -> Int -> Word16
indexWord16ArrayBE a (I# i) = case targetByteOrder of
  LittleEndian -> W16# (byteSwap16# (indexWord16OffAddr# a i))
  BigEndian -> W16# (indexWord16OffAddr# a i)

indexWord32ArrayBE :: Addr# -> Int -> Word32
indexWord32ArrayBE a (I# i) = case targetByteOrder of
  LittleEndian -> W32# (byteSwap32# (indexWord32OffAddr# a i))
  BigEndian -> W32# (indexWord32OffAddr# a i)

readWord8Array :: Addr# -> Int -> IO Word8
readWord8Array a (I# i) = IO $ \s0 -> case readWord8OffAddr# a i s0 of
  (# s1, r #) -> (# s1, W8# r #)

readWord16ArrayBE :: Addr# -> Int -> IO Word16
readWord16ArrayBE a (I# i) = IO $ \s0 -> case readWord16OffAddr# a i s0 of
  (# s1, r #) -> case targetByteOrder of
    LittleEndian -> (# s1, W16# (byteSwap16# r) #)
    BigEndian -> (# s1, W16# r #)

readWord32ArrayBE :: Addr# -> Int -> IO Word32
readWord32ArrayBE a (I# i) = IO $ \s0 -> case readWord32OffAddr# a i s0 of
  (# s1, r #) -> case targetByteOrder of
    LittleEndian -> (# s1, W32# (byteSwap32# r) #)
    BigEndian -> (# s1, W32# r #)

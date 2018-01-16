module Main where

import Data.Int
import Data.Word

-- Test case for bug #5785. The cause of this was that the LLVM backend
-- converted all Int constants using (fromInteger :: Int) and so on 32bit a
-- Int64 or Word32 would be truncated to 32bit! value before printing out.
main :: IO ()
main = do
    -- first two should print as big numbers (as unsigned)
    print (-1 :: Word8)
    print (-1 :: Word16)
    print (-1 :: Word32)
    print (-1 :: Word64)
    print (-1 :: Int8)
    print (-1 :: Int16)
    print (-1 :: Int32)
    print (-1 :: Int64)

    -- only requires 32 bits (unsigned)
    print (2316287658 :: Word8)
    print (2316287658 :: Word16)
    print (2316287658 :: Word32)
    print (2316287658 :: Word64)
    print (2316287658 :: Int8)
    print (2316287658 :: Int16)
    print (2316287658 :: Int32)
    print (2316287658 :: Int64)

    -- this requries a 64 (unsigned) bit word to store correctly
    print (32342316287658 :: Word8)
    print (32342316287658 :: Word16)
    print (32342316287658 :: Word32)
    print (32342316287658 :: Word64)
    print (32342316287658 :: Int8)
    print (32342316287658 :: Int16)
    print (32342316287658 :: Int32)
    print (32342316287658 :: Int64)


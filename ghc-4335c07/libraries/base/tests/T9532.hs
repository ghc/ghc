-- Tests Data.Bits.FiniteBits(count{Leading,Trailing}Zeros)` -- c.f. T9340.hs

import Control.Monad
import Data.Bits
import Data.Int
import Data.Typeable
import Data.Word
import Numeric (showHex)

-- Reference Implementations

-- count trailing zeros
ctzRI :: FiniteBits a => a -> Word
ctzRI x = fromIntegral $ go 0
  where
    go i | i >= w      = i
         | testBit x i = i
         | otherwise   = go (i+1)

    w = finiteBitSize x

-- count leading zeros
clzRI :: FiniteBits a => a -> Word
clzRI x = fromIntegral $ (w-1) - go (w-1)
  where
    go i | i < 0       = i -- no bit set
         | testBit x i = i
         | otherwise   = go (i-1)

    w = finiteBitSize x

-- Test Driver
main :: IO ()
main = do
    forM_ testpats $ \w64 -> do
        checkCLZ (fromIntegral w64 :: Word)
        checkCLZ (fromIntegral w64 :: Word8)
        checkCLZ (fromIntegral w64 :: Word16)
        checkCLZ (fromIntegral w64 :: Word32)
        checkCLZ (fromIntegral w64 :: Word64)

        checkCLZ (fromIntegral w64 :: Int)
        checkCLZ (fromIntegral w64 :: Int8)
        checkCLZ (fromIntegral w64 :: Int16)
        checkCLZ (fromIntegral w64 :: Int32)
        checkCLZ (fromIntegral w64 :: Int64)

        checkCTZ (fromIntegral w64 :: Word)
        checkCTZ (fromIntegral w64 :: Word8)
        checkCTZ (fromIntegral w64 :: Word16)
        checkCTZ (fromIntegral w64 :: Word32)
        checkCTZ (fromIntegral w64 :: Word64)

        checkCTZ (fromIntegral w64 :: Int)
        checkCTZ (fromIntegral w64 :: Int8)
        checkCTZ (fromIntegral w64 :: Int16)
        checkCTZ (fromIntegral w64 :: Int32)
        checkCTZ (fromIntegral w64 :: Int64)

    putStrLn $ concat ["tested ", show (length testpats), " patterns"]

  where
    -- try to construct some interesting patterns
    testpats :: [Word64]
    testpats = [ bit i - 1 | i <- [0..63] ] ++
               [ complement (bit i - 1) | i <- [0..63] ] ++
               [ bit i .|. bit j | i <- [0..63], j <- [0..i] ]

    -- Compare impl-under-test with reference-impl
    checkCLZ :: (Typeable a, Show a, Integral a, FiniteBits a) => a -> IO ()
    checkCLZ v = unless (vri == viut) $ do
        putStrLn $ concat [ "FAILED: clz (0x", showHex v " :: ", tyName
                          , ") ==> (RI=", show vri, " vs. IUT=", show viut, ")"
                          ]
      where
        tyName = show (typeOf v)
        vri    = clzRI v
        viut   = fromIntegral (countLeadingZeros v)

    -- Compare impl-under-test with reference-impl
    checkCTZ :: (Typeable a, Show a, Integral a, FiniteBits a) => a -> IO ()
    checkCTZ v = unless (vri == viut) $ do
        putStrLn $ concat [ "FAILED: ctz (0x", showHex v " :: ", tyName
                          , ") ==> (RI=", show vri, " vs. IUT=", show viut, ")"
                          ]
      where
        tyName = show (typeOf v)
        vri    = ctzRI v
        viut   = fromIntegral (countTrailingZeros v)

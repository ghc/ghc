{-# LANGUAGE MagicHash #-}

import Control.Monad
import Data.Bits
import GHC.Exts
import GHC.Word
import Numeric (showHex)

-- Reference Implementation

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

clzRI32, ctzRI32 :: Word -> Word
clzRI32 x = clzRI (fromIntegral x :: Word32)
ctzRI32 x = ctzRI (fromIntegral x :: Word32)

clzRI16, ctzRI16 :: Word -> Word
clzRI16 x = clzRI (fromIntegral x :: Word16)
ctzRI16 x = ctzRI (fromIntegral x :: Word16)

clzRI8, ctzRI8 :: Word -> Word
clzRI8 x = clzRI (fromIntegral x :: Word8)
ctzRI8 x = ctzRI (fromIntegral x :: Word8)

-- Implementation Under Test
ctzIUT, clzIUT :: Word -> Word
ctzIUT (W# x#) = W# (ctz# x#)
clzIUT (W# x#) = W# (clz# x#)

ctzIUT8, clzIUT8 :: Word -> Word
ctzIUT8 (W# x#) = W# (ctz8# x#)
clzIUT8 (W# x#) = W# (clz8# x#)

ctzIUT16, clzIUT16 :: Word -> Word
ctzIUT16 (W# x#) = W# (ctz16# x#)
clzIUT16 (W# x#) = W# (clz16# x#)

ctzIUT32, clzIUT32 :: Word -> Word
ctzIUT32 (W# x#) = W# (ctz32# x#)
clzIUT32 (W# x#) = W# (clz32# x#)

ctzIUT64, clzIUT64 :: Word64 -> Word
ctzIUT64 (W64# x#) = W# (ctz64# x#)
clzIUT64 (W64# x#) = W# (clz64# x#)

main :: IO ()
main = do
    forM_ testpats $ \w64 -> do
        let w = fromIntegral w64 :: Word

        check "clz"   clzRI   clzIUT   w
        check "clz8"  clzRI8  clzIUT8  w
        check "clz16" clzRI16 clzIUT16 w
        check "clz32" clzRI32 clzIUT32 w
        check "clz64" clzRI   clzIUT64 w64

        check "ctz"   ctzRI   ctzIUT   w
        check "ctz8"  ctzRI8  ctzIUT8  w
        check "ctz16" ctzRI16 ctzIUT16 w
        check "ctz32" ctzRI32 ctzIUT32 w
        check "ctz64" ctzRI   ctzIUT64 w64

    putStrLn $ concat ["tested ", show (length testpats), " patterns"]

  where
    -- try to construct some interesting patterns
    testpats :: [Word64]
    testpats = [ bit i - 1 | i <- [0..63] ] ++
               [ complement (bit i - 1) | i <- [0..63] ] ++
               [ bit i .|. bit j | i <- [0..63], j <- [0..i] ]

    check s fri fiut v = unless (vri == viut) $ do
        putStrLn $ concat [ "FAILED ", s, " for x=0x", showHex v ""
                          , " (RI=", show vri, " IUT=", show viut, ")"
                          ]
      where
        vri = fri v
        viut = fiut v

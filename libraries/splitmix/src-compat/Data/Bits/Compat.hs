{-# LANGUAGE CPP #-}
module Data.Bits.Compat (
    popCount,
    zeroBits,
    finiteBitSize,
    countLeadingZeros,
    ) where

import Data.Bits

#if !MIN_VERSION_base(4,7,0)
#define FiniteBits Bits
#endif

#if !MIN_VERSION_base(4,5,0)
popCount :: Bits a => a -> Int
popCount = go 0
 where
   go c 0 = c `seq` c
   go c w = go (c+1) (w .&. (w - 1)) -- clear the least significant
{-# INLINE popCount #-}
#endif

#if !MIN_VERSION_base(4,7,0)
zeroBits :: Bits a => a
zeroBits = clearBit (bit 0) 0
{-# INLINE zeroBits #-}

finiteBitSize :: Bits a => a -> Int
finiteBitSize = bitSize
{-# INLINE finiteBitSize #-}
#endif

#if !MIN_VERSION_base(4,8,0)
countLeadingZeros :: FiniteBits b => b -> Int
countLeadingZeros x = (w-1) - go (w-1)
  where
    go i | i < 0       = i -- no bit set
         | testBit x i = i
         | otherwise   = go (i-1)

    w = finiteBitSize x
{-# INLINE countLeadingZeros #-}
#endif

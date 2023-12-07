{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
module GHC.TypeNats.Experimental (
    plusSNat,
    timesSNat,
    powerSNat,
    minusSNat,
    divSNat,
    modSNat,
    log2SNat,
) where

import GHC.Internal.TypeNats
import GHC.Num.Natural (naturalLog2)

plusSNat :: SNat n -> SNat m -> SNat (n + m)
plusSNat (UnsafeSNat n) (UnsafeSNat m) = UnsafeSNat (n + m)

timesSNat :: SNat n -> SNat m -> SNat (n * m)
timesSNat (UnsafeSNat n) (UnsafeSNat m) = UnsafeSNat (n * m)

powerSNat :: SNat n -> SNat m -> SNat (n ^ m)
powerSNat (UnsafeSNat n) (UnsafeSNat m) = UnsafeSNat (n ^ m)

minusSNat :: (m <= n) => SNat n -> SNat m -> SNat (n - m)
minusSNat (UnsafeSNat n) (UnsafeSNat m) = UnsafeSNat (n - m)

divSNat :: (1 <= m) => SNat n -> SNat m -> SNat (Div n m)
divSNat (UnsafeSNat n) (UnsafeSNat m) = UnsafeSNat (div n m)

modSNat :: (1 <= m) => SNat n -> SNat m -> SNat (Mod n m)
modSNat (UnsafeSNat n) (UnsafeSNat m) = UnsafeSNat (mod n m)

log2SNat :: (1 <= n) => SNat n -> SNat (Log2 n)
log2SNat (UnsafeSNat n) = UnsafeSNat (fromIntegral (naturalLog2 n))

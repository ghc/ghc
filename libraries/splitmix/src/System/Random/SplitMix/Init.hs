{-# LANGUAGE CPP #-}
-- | Initialization of global generator.
module System.Random.SplitMix.Init (
    initialSeed,
) where

import Data.Word (Word64)

#if defined(SPLITMIX_INIT_GHCJS) && __GHCJS__

import Data.Word (Word32)

#else
#if defined(SPLITMIX_INIT_C)

#else

import Data.Bits             (xor)
import Data.Time.Clock.POSIX (getPOSIXTime)
#if !__GHCJS__
import System.CPUTime (cpuTimePrecision, getCPUTime)
#endif

#endif
#endif

initialSeed :: IO Word64

#if defined(SPLITMIX_INIT_GHCJS) && __GHCJS__

initialSeed = fmap fromIntegral initialSeedJS

foreign import javascript
    "$r = Math.floor(Math.random()*0x100000000);"
    initialSeedJS :: IO Word32

#else
#if defined(SPLITMIX_INIT_C)

initialSeed = initialSeedC

foreign import ccall "splitmix_init" initialSeedC :: IO Word64

#else

initialSeed =  do
    now <- getPOSIXTime
    let timebits = truncate now :: Word64
#if __GHCJS__
    let cpubits = 0
#else
    cpu <- getCPUTime
    let cpubits = fromIntegral (cpu `div` cpuTimePrecision) :: Word64
#endif
    return $ timebits `xor` cpubits

#endif
#endif

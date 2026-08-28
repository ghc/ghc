{-# LANGUAGE CPP #-}

module GHC.Driver.CpuFeatures
  ( X86CpuFeature(..)
  , cachedX86CpuFeatures
  ) where

import GHC.Prelude

import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)

-- | x86 CPU features understood by GHC's native CPU feature probe.
data X86CpuFeature
  = SSE2
  | SSE3
  | SSSE3
  | SSE4_1
  | SSE4_2
  | AVX
  | AVX2
  | AVX512F
  | AVX512BW
  | AVX512CD
  | AVX512DQ
  | AVX512VL
  | BMI1
  | BMI2
  | FMA
  | GFNI
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Decode the bitmask returned by 'ghc_detect_x86_cpu_features'.
--
-- Bit positions are constructor order, which must match the enum in
-- @compiler/cbits/cpu_features_x86.c@.
decodeX86CpuFeatureMask :: Word64 -> [X86CpuFeature]
decodeX86CpuFeatureMask mask =
  [ feat
  | feat <- [minBound .. maxBound]
  , testBit mask (fromEnum feat)
  ]

-- | Low-level FFI access to the C probe.
detectX86CpuFeatureMask :: IO Word64
#if defined(javascript_HOST_ARCH)
detectX86CpuFeatureMask = pure 0
#else
detectX86CpuFeatureMask = c_ghc_detect_x86_cpu_features
#endif

-- | Probe host x86 CPU features and decode them into an ordered feature list.
detectX86CpuFeatures :: IO [X86CpuFeature]
detectX86CpuFeatures = decodeX86CpuFeatureMask <$> detectX86CpuFeatureMask

-- | The host's x86 CPU features, probed once and memoized.
cachedX86CpuFeatures :: [X86CpuFeature]
cachedX86CpuFeatures = unsafePerformIO detectX86CpuFeatures
{-# NOINLINE cachedX86CpuFeatures #-}

#if !defined(javascript_HOST_ARCH)
foreign import ccall unsafe "ghc_detect_x86_cpu_features"
  c_ghc_detect_x86_cpu_features :: IO Word64
#endif

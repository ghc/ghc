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
  deriving (Eq, Ord, Show)

-- | Decode the bitmask returned by 'ghc_detect_x86_cpu_features'.
--
-- NOTE: Bit positions are LLVM\/GCC's @__builtin_cpu_supports@ feature
-- numbering, i.e. @enum ProcessorFeatures@ vendored in
-- @compiler/cbits/cpu_features_x86.c@. The C side returns the first 64
-- feature bits of that numbering.
decodeX86CpuFeatureMask :: Word64 -> [X86CpuFeature]
decodeX86CpuFeatureMask mask =
  [ feat
  | (bit_ix, feat) <- cpuFeatureBitLayout
  , testBit mask bit_ix
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
--
-- CPUID results are constant for the lifetime of the process, so probing more
-- than once (e.g. once per @-march=native@ in a command line or file pragma)
-- is wasteful. This is referentially transparent despite the FFI call.
cachedX86CpuFeatures :: [X86CpuFeature]
cachedX86CpuFeatures = unsafePerformIO detectX86CpuFeatures
{-# NOINLINE cachedX86CpuFeatures #-}

-- | See the NOTE on 'decodeX86CpuFeatureMask' for where these bit positions
-- come from. The constant names on the C side are upstream's: @FEATURE_SSE2@,
-- @FEATURE_BMI@ (= 'BMI1'), etc.
cpuFeatureBitLayout :: [(Int, X86CpuFeature)]
cpuFeatureBitLayout =
  [ (4,  SSE2)     -- FEATURE_SSE2
  , (5,  SSE3)     -- FEATURE_SSE3
  , (6,  SSSE3)    -- FEATURE_SSSE3
  , (7,  SSE4_1)   -- FEATURE_SSE4_1
  , (8,  SSE4_2)   -- FEATURE_SSE4_2
  , (9,  AVX)      -- FEATURE_AVX
  , (10, AVX2)     -- FEATURE_AVX2
  , (14, FMA)      -- FEATURE_FMA
  , (15, AVX512F)  -- FEATURE_AVX512F
  , (16, BMI1)     -- FEATURE_BMI
  , (17, BMI2)     -- FEATURE_BMI2
  , (20, AVX512VL) -- FEATURE_AVX512VL
  , (21, AVX512BW) -- FEATURE_AVX512BW
  , (22, AVX512DQ) -- FEATURE_AVX512DQ
  , (23, AVX512CD) -- FEATURE_AVX512CD
  , (32, GFNI)     -- FEATURE_GFNI
  ]

#if !defined(javascript_HOST_ARCH)
foreign import ccall unsafe "ghc_detect_x86_cpu_features"
  c_ghc_detect_x86_cpu_features :: IO Word64
#endif

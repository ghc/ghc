{-# LANGUAGE CPP #-}

module GHC.Platform.HostCpuFeatures
  ( hostX86CpuFeatures
  ) where

import GHC.Prelude

import GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Platform.CpuFeatures (CpuFeature(..))

import Data.Word (Word64)

#include "HostCpuFeaturesX86.h"

#define GHC_X86_FEATURE_CONS(feat, cond) feat :
-- | The features of the table in @HostCpuFeaturesX86.h@, in row order.
probedX86CpuFeatures :: [CpuFeature]
probedX86CpuFeatures = GHC_X86_PROBED_FEATURES(GHC_X86_FEATURE_CONS) []

-- | The host's x86 CPU features, probed once and memoized.
hostX86CpuFeatures :: EnumSet CpuFeature
hostX86CpuFeatures = EnumSet.fromList
  [ feat
  | (bit, feat) <- zip [0 ..] probedX86CpuFeatures
  , testBit mask bit
  ]
  where
    -- Strict, so that GHC doesn't inline the probe into the comprehension.
    !mask = hostX86CpuFeatureMask

#if defined(javascript_HOST_ARCH)
hostX86CpuFeatureMask :: Word64
hostX86CpuFeatureMask = 0
#else
foreign import ccall unsafe "ghc_host_x86_cpu_feature_mask"
  hostX86CpuFeatureMask :: Word64
#endif

{-# LANGUAGE CPP #-}

module GHC.Platform.HostCpuFeatures
  ( hostX86CpuFeatures
  ) where

import GHC.Prelude

import GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Platform.CpuFeatures (CpuFeature(..))

import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)

#include "HostCpuFeaturesX86.h"

#define GHC_X86_FEATURE_CONS(feat, cond) feat :

-- | The features of the table in @HostCpuFeaturesX86.h@, in row order.
probedX86CpuFeatures :: [CpuFeature]
probedX86CpuFeatures = GHC_X86_PROBED_FEATURES(GHC_X86_FEATURE_CONS) []

-- | The host's x86 CPU features.
hostX86CpuFeatures :: EnumSet CpuFeature
hostX86CpuFeatures = unsafePerformIO $ do
  mask <- hostX86CpuFeatureMask
  pure $ EnumSet.fromList
    [ feat
    | (bit, feat) <- zip [0 ..] probedX86CpuFeatures
    , testBit mask bit
    ]
{-# NOINLINE hostX86CpuFeatures #-}

#if defined(javascript_HOST_ARCH)
hostX86CpuFeatureMask :: IO Word64
hostX86CpuFeatureMask = pure 0
#else
foreign import ccall unsafe "ghc_host_x86_cpu_feature_mask"
  hostX86CpuFeatureMask :: IO Word64
#endif

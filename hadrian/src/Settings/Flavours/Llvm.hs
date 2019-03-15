module Settings.Flavours.Llvm (
  benchmarkLlvmFlavour,
  performanceLlvmFlavour,
  profiledLlvmFlavour,
  quickLlvmFlavour,
) where

import Expression
import Flavour

import Settings.Flavours.Benchmark
import Settings.Flavours.Performance
import Settings.Flavours.Profiled
import Settings.Flavours.Quick

-- Please update doc/flavours.md when changing this file.
benchmarkLlvmFlavour, performanceLlvmFlavour, profiledLlvmFlavour, quickLlvmFlavour :: Flavour
benchmarkLlvmFlavour   = mkLlvmFlavour benchmarkFlavour
performanceLlvmFlavour = mkLlvmFlavour performanceFlavour
profiledLlvmFlavour    = mkLlvmFlavour profiledFlavour
quickLlvmFlavour       = mkLlvmFlavour quickFlavour

-- | Turn a flavour into an LLVM flavour
mkLlvmFlavour :: Flavour -> Flavour
mkLlvmFlavour flav = flav
    { name = name flav ++ "-llvm"
    , args = mconcat [ args flav
                     , builder Ghc ? arg "-fllvm" ]
    }

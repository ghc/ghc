module Settings.Flavours.Llvm (
  performanceLlvmFlavour,
  profiledLlvmFlavour,
  quickLlvmFlavour,
) where

import Expression
import Flavour

import Settings.Flavours.Performance
import Settings.Flavours.Profiled
import Settings.Flavours.Quick

-- Please update doc/flavours.md when changing this file.
performanceLlvmFlavour, profiledLlvmFlavour, quickLlvmFlavour :: Flavour
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

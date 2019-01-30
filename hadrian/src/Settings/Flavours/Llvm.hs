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
performanceLlvmFlavour = llvm performanceFlavour{ name = name performanceFlavour ++ "-llvm" }
profiledLlvmFlavour    = llvm profiledFlavour   { name = name profiledFlavour    ++ "-llvm" }
quickLlvmFlavour       = llvm quickFlavour      { name = name quickFlavour       ++ "-llvm" }


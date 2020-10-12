{-# LANGUAGE CPP #-}

module GHC.Platform.RiscV64 where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_riscv64 1
#include "../../../includes/CodeGen.Platform.hs"

{-# LANGUAGE CPP #-}

module CodeGen.Platform.RiscV64 where

#define MACHREGS_NO_REGS 0
#define MACHREGS_riscv64 1
#include "../../../../includes/CodeGen.Platform.hs"

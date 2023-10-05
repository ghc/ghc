{-# LANGUAGE CPP #-}

module GHC.Platform.RISCV64 where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_riscv64 1
#include "CodeGen.Platform.h"


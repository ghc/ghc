{-# LANGUAGE CPP #-}

module CodeGen.Platform.NoRegs where

#define MACHREGS_NO_REGS 1
#include "../../../../includes/CodeGen.Platform.hs"


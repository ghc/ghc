{-# LANGUAGE CPP #-}

module CodeGen.Platform.PPC_Darwin where

#define MACHREGS_NO_REGS 0
#define MACHREGS_powerpc 1
#define MACHREGS_darwin 1
#include "../../../../includes/CodeGen.Platform.hs"


{-# LANGUAGE CPP #-}

module CodeGen.Platform.X86 where

import GhcPrelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_i386 1
#include "../../../../includes/CodeGen.Platform.hs"


{-# LANGUAGE CPP #-}

module CodeGen.Platform.X86_64 where

import GhcPrelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_x86_64 1
#include "../../../../includes/CodeGen.Platform.hs"


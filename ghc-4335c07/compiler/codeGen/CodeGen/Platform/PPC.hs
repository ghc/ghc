{-# LANGUAGE CPP #-}

module CodeGen.Platform.PPC where

import GhcPrelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_powerpc 1
#include "../../../../includes/CodeGen.Platform.hs"


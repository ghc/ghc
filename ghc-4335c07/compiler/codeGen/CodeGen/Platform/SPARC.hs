{-# LANGUAGE CPP #-}

module CodeGen.Platform.SPARC where

import GhcPrelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_sparc 1
#include "../../../../includes/CodeGen.Platform.hs"


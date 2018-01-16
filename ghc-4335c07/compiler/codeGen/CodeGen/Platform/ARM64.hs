{-# LANGUAGE CPP #-}

module CodeGen.Platform.ARM64 where

import GhcPrelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_aarch64 1
#include "../../../../includes/CodeGen.Platform.hs"


{-# LANGUAGE CPP #-}

module GHC.Platform.NoRegs where

import GHC.Prelude

#define MACHREGS_NO_REGS 1
#include "../../../includes/CodeGen.Platform.hs"


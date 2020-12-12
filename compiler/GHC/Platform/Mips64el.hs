{-# LANGUAGE CPP #-}

module GHC.Platform.Mips64el where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_mips64el 1
#include "../../../includes/CodeGen.Platform.hs"

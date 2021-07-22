{-# LANGUAGE CPP #-}

module GHC.Platform.AArch64 where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_aarch64 1
#include "CodeGen.Platform.h"

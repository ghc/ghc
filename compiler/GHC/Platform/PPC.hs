{-# LANGUAGE CPP #-}

module GHC.Platform.PPC where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_powerpc 1
#include "CodeGen.Platform.h"


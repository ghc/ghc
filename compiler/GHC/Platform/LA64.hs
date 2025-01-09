{-# LANGUAGE CPP #-}

module GHC.Platform.LA64 where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_loongarch64 1
#include "CodeGen.Platform.h"

{-# LANGUAGE CPP #-}

module GHC.Platform.ARM where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_arm 1
#include "CodeGen.Platform.h"


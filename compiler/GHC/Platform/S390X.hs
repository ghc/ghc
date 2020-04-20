{-# LANGUAGE CPP #-}

module GHC.Platform.S390X where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_s390x 1
#include "../../../includes/CodeGen.Platform.hs"


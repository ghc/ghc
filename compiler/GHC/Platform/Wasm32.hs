{-# LANGUAGE CPP #-}

module GHC.Platform.Wasm32 where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_wasm32 1
#include "CodeGen.Platform.h"

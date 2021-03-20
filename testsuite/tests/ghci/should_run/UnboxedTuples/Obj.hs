{-# LANGUAGE CPP, UnboxedTuples, MagicHash, ScopedTypeVariables, PolyKinds #-}
{-# OPTIONS_GHC -fobject-code #-}

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS < 64
#define WW Word64
#else
#define WW Word
#endif

module Obj where

import GHC.Exts
import GHC.Word

#include "Common.hs-incl"

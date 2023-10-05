{-# LANGUAGE CPP, UnboxedTuples, MagicHash, ScopedTypeVariables, PolyKinds #-}
{-# OPTIONS_GHC -fbyte-code #-}

module ByteCode where

import GHC.Exts
import GHC.Word

#include "Common.hs-incl"

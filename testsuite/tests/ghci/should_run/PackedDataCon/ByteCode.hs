{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -fbyte-code #-}

module ByteCode where

import GHC.Exts
import GHC.Word
import GHC.Int
import Types

#include "Common.hs-incl"

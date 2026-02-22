{-# LANGUAGE CPP, UnboxedTuples, MagicHash #-}
{-# OPTIONS_GHC -fobject-code #-}

#include "MachDeps.h"

module Obj where

import GHC.Exts
import GHC.Word

#include "Common.hs-incl"

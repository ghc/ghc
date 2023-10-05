{-# LANGUAGE CPP #-}
#include "header1.h"
module CPPIncludes where

-- This module is only discovered by downsweep and hits a different code path
-- to the path which gets mod summaries for the targets
import CPPIncludes_Down

foo = A

qux = B

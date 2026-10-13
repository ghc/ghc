{-# language Strict #-}
{-# language CPP #-}
{-# options_ghc -fexpose-all-unfoldings #-}
module A where

#include "foo.h"
import {-# source #-} C

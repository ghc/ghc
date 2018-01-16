
{-# LANGUAGE CApiFFI #-}

module Capi_Ctype_A_001 where

#include "capi_ctype_001.h"

import Foreign
import Foreign.C

data FooA = FooA {
                ia :: CInt,
                ja :: CInt,
                ka :: CInt
            }

instance Storable FooA where
    sizeOf _ = #size Foo
    alignment = sizeOf
    peek p = do i <- (# peek Foo, i) p
                j <- (# peek Foo, j) p
                k <- (# peek Foo, k) p
                return $ FooA i j k
    poke p foo = do (# poke Foo, i) p (ia foo)
                    (# poke Foo, j) p (ja foo)
                    (# poke Foo, k) p (ka foo)


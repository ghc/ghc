
{-# LANGUAGE CApiFFI #-}

module Capi_Ctype_A_002 (Foo(..)) where

#include "capi_ctype_002_A.h"

import Foreign
import Foreign.C

data {-# CTYPE "capi_ctype_002_A.h" "Foo" #-}
     Foo = Foo {
               i :: CInt,
               j :: CInt,
               k :: CInt
           }

instance Storable Foo where
    sizeOf _ = #size Foo
    alignment = sizeOf
    peek p = do i <- (# peek Foo, i) p
                j <- (# peek Foo, j) p
                k <- (# peek Foo, k) p
                return $ Foo i j k
    poke p foo = do (# poke Foo, i) p (i foo)
                    (# poke Foo, j) p (j foo)
                    (# poke Foo, k) p (k foo)


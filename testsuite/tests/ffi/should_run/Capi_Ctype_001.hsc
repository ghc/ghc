
{-# LANGUAGE CApiFFI #-}

module Main (main) where

#include "capi_ctype_001.h"

import Capi_Ctype_A_001

import Foreign
import Foreign.C

main :: IO ()
main = do alloca $ \p ->
              do poke p (Foo 5 6 7)
                 r1 <- f p
                 print r1
          alloca $ \p ->
              do poke p (Foo 15 16 17)
                 r2 <- g p
                 print r2

data {-# CTYPE "Foo" #-}
     Foo = Foo {
               i :: CInt,
               j :: CInt,
               k :: CInt
           }

foreign import capi unsafe "capi_ctype_001.h f"
    f :: Ptr Foo -> IO CInt

foreign import capi unsafe "capi_ctype_001.h g"
    g :: Ptr Foo -> IO CInt

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


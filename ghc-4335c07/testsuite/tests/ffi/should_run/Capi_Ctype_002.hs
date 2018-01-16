
{-# LANGUAGE CApiFFI #-}

module Main (main) where

import Capi_Ctype_A_002

import Foreign
import Foreign.C

main :: IO ()
main = alloca $ \p ->
           do poke p (Foo 5 6 7)
              r1 <- f p
              print r1

foreign import capi unsafe "capi_ctype_002_B.h f"
    f :: Ptr Foo -> IO CInt


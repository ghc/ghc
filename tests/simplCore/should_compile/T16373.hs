{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -dno-typeable-binds -O2 #-}

module Test2 where

import GHC.TypeLits
import Data.Proxy

testAA :: Bool
testAA = symbolVal (Proxy :: Proxy "A") == symbolVal (Proxy :: Proxy "A")

testAB :: Bool
testAB = symbolVal (Proxy :: Proxy "A") == symbolVal (Proxy :: Proxy "B")

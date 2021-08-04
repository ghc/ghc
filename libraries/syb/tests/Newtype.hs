{-# LANGUAGE CPP #-}
{-# OPTIONS -fglasgow-exts #-}

module Newtype (tests) where

-- The type of a newtype should treat the newtype as opaque

import Test.Tasty.HUnit

import Data.Generics

newtype T = MkT Int deriving( Typeable )

tests = show (typeOf (undefined :: T)) @?= output

#if __GLASGOW_HASKELL__ >= 701
output = "T"
#else
output = "Newtype.T"
#endif

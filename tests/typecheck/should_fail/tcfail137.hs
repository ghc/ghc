{-# LANGUAGE DatatypeContexts #-}
-- Test the stupid context on newtypes
-- (GHC 6.4 dropped it on the floor by mistake)
module ShouldFail where

newtype Floating a => Test a = Test [a] 

x = Test [False, True]

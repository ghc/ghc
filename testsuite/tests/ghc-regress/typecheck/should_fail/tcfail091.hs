{-# LANGUAGE ImplicitParams #-}

-- !!! Illegal superclass constraint
-- These examples actually crashed GHC 4.08.2

module ShouldFail where

class (?imp :: Int) => C t where


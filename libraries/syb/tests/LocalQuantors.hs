{-# OPTIONS -fglasgow-exts #-}

module LocalQuantors () where

-- A datatype with a locally quantified component
-- Seems to be too polymorphic to descend into structure!
-- Largely irrelevant?!

import Data.Generics

data Test = Test (GenericT) deriving Typeable

instance Data Test
  where
    gfoldl _ z x = z x -- folding without descent 
    toConstr (Test _) = testConstr
    gunfold _ _ = error "gunfold"
    dataTypeOf _ = testDataType

testConstr   = mkConstr testDataType "Test" [] Prefix
testDataType = mkDataType "Main.Test" [testConstr]

{-# OPTIONS -fglasgow-exts #-}

-- A datatype with a locally quantified component
-- Seems to be too polymorphic to descend into structure!
-- Largely irrelevant?!

module Main where
import Data.Generics

data Test = Test (GenericT) deriving Typeable

instance Data Test
  where
    gfoldl _ z x = z x -- folding without descent 
    toConstr (Test _) = testConstr
    fromConstr c = case constrIndex c of
                     1 -> Test undefined
    dataTypeOf _ = testDataType

testConstr   = mkConstr testDataType "Test" [] Prefix
testDataType = mkDataType "Main.Test" [testConstr]

-- Test for compilation only
main = undefined

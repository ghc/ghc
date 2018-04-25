module TestModule where

{-# ANN module "Module annotation" #-}

{-# ANN type TestType "Type annotation" #-}
{-# ANN TestType "Constructor annotation" #-}
data TestType = TestType

{-# ANN testValue "Value annotation" #-}
testValue :: Int
testValue = 42

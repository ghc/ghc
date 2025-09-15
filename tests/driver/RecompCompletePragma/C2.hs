{-# LANGUAGE PatternSynonyms #-}
module RecompCompletePragmaC where

-- Define a data type
data MyType = IntValue Int | BoolValue Bool | StringValue String

-- Define pattern synonyms for the data type
pattern P :: MyType
pattern P = IntValue 42

-- Q is changed to use a different BoolValue, but P remains the same
pattern Q :: MyType
pattern Q = BoolValue False

{-# COMPLETE P, Q, StringValue #-}
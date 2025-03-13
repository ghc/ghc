{-# LANGUAGE PatternSynonyms #-}
module RecompCompletePragmaC where

-- Define a data type
data MyType = IntValue Int | BoolValue Bool | StringValue String

-- Define pattern synonyms for the data type
pattern P :: MyType
pattern P = IntValue 42

pattern Q :: MyType
pattern Q = BoolValue True

{-# COMPLETE P, Q, StringValue #-}
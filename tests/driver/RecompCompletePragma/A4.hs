{-# LANGUAGE PatternSynonyms #-}
module RecompCompletePragmaA where

-- Define a pattern synonym
pattern P :: Int
pattern P = 42

pattern Q :: Bool
pattern Q = True

pattern Z :: String
pattern Z = ""

{-# COMPLETE Q, P #-}
{-# COMPLETE Z #-}

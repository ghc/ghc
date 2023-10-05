{-# LANGUAGE PatternSynonyms #-}
module WildcardInPatSynSig where

pattern Single :: () => (Show a) => _ -> [a]
pattern Single x = [x]

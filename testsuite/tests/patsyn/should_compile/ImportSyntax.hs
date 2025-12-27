{-# LANGUAGE PatternSynonyms #-}
module ImportSyntax (A(.., B)) where

data A = A

pattern B = A

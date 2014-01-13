-- Pattern synonyms
-- Generated code doesn't emit overlapping pattern warnings

{-# LANGUAGE PatternSynonyms #-}
module ShouldCompile where

pattern P <- Just True

test1 P = 2
test1 Nothing = 3
test1 (Just _) = 4

{-# LANGUAGE PatternSynonyms #-}

module InlinePatSyn_ExplicitBidiBuilder where

-- Explicit bidirectional pattern
pattern ExplicitPattern x <- x:xs where
  ExplicitPattern x = [x]
{-# INLINE ExplicitPattern #-}

testExplicitBuilder x = ExplicitPattern (x+1)

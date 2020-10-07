{-# LANGUAGE PatternSynonyms #-}

module InlinePatSyn_ExplicitBidiMatcher where

-- Explicit bidirectional pattern
pattern ExplicitPattern x <- x:xs where
  ExplicitPattern x = [x]
{-# INLINE ExplicitPattern #-}

testMatcherofExplicitBuilder (ExplicitPattern x) = 1
testMatcherofExplicitBuilder _  = 2

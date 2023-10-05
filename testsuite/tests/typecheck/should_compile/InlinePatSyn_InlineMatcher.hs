{-# LANGUAGE PatternSynonyms #-}

module InlinePatSyn_InlineMatcher where

-- Pattern with "INLINE" pragma, both builder and matcher should be inlined
pattern InlinePattern a = [[[[a]]]]
{-# INLINE InlinePattern #-}

testInMatcher (InlinePattern x) = 1
testInMatcher _ = 2

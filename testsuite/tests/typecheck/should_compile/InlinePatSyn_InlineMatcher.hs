{-# LANGUAGE PatternSynonyms #-}

module InlinePatSyn_InlineMatcher where

-- Pattern with "INLINE" pragma, both builder and matcher should be inlined
pattern InlineablePattern a = [[[[a]]]]
{-# INLINE InlineablePattern #-}

testInMatcher (InlineablePattern x) = 1
testInMatcher _ = 2

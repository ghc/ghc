{-# LANGUAGE PatternSynonyms #-}

module InlinePatSyn_InlinableMatcher where

-- Pattern with "INLINE" pragma, both builder and matcher should be inlined
pattern InlinablePattern a = [[[[a]]]]
{-# INLINEABLE InlinablePattern #-}

testInMatcher (InlinablePattern x) = 1
testInMatcher _ = 2

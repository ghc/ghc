{-# LANGUAGE PatternSynonyms #-}

module T12178 where

-- Pattern with "NOINLINE" pragma, neither builder nor matcher should be inlined
pattern NonInlineablePattern a = Left a
{-# NOINLINE NonInlineablePattern #-}

testNonMatcher (NonInlineablePattern x) = 1
testNonMatcher _ = 2

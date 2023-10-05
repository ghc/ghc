{-# LANGUAGE PatternSynonyms #-}

module T12178 where

-- Pattern with "NOINLINE" pragma, neither builder nor matcher should be inlined
pattern NonInlinablePattern a = Left a
{-# NOINLINE NonInlinablePattern #-}

testNonMatcher (NonInlinablePattern x) = 1
testNonMatcher _ = 2

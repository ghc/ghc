{-# LANGUAGE PatternSynonyms #-}

module T12178 where

-- Pattern with "INLINE" pragma, both builder and matcher should be inlined
pattern InlineablePattern a = [[[[a]]]]
{-# INLINE InlineablePattern #-}

testInBuilder x = InlineablePattern (x+1)

testInMatcher (InlineablePattern x) = 1
testInMatcher _ = 2

-- Pattern with "NOINLINE" pragma, neither builder nor matcher should be inlined
pattern NonInlineablePattern a = Left a
{-# NOINLINE NonInlineablePattern #-}

testNonBuilder x = NonInlineablePattern (x+1)

testNonMatcher (NonInlineablePattern x) = 1
testNonMatcher _ = 2

-- Pattern without a pragma, neither builder nor matcher should be inlined
pattern DefaultInliningPattern a =  [[[[a]]]]

testDefaultBuilder x = DefaultInliningPattern (x+1)

testDefaultMatcher (DefaultInliningPattern a) = 1
testDefaultMatcher _ = 2

-- Explicit bidirectional pattern
pattern ExplicitPattern x <- x:xs where
  ExplicitPattern x = [x]
{-# INLINE ExplicitPattern #-}

testExplicitBuilder x = ExplicitPattern (x+1)

testMatcherofExplicitBuilder (ExplicitPattern x) = 1
testMatcherofExplicitBuilder _  = 2

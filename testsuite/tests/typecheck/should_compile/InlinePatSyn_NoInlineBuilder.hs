{-# LANGUAGE PatternSynonyms #-}

module InlinePatSyn_NoInlineBuilder where

-- Pattern with "NOINLINE" pragma, neither builder nor matcher should be inlined
pattern NonInlineablePattern a = Left a
{-# NOINLINE NonInlineablePattern #-}

testNonBuilder x = NonInlineablePattern (x+1)

{-# LANGUAGE PatternSynonyms #-}

module InlinePatSyn_NoInlineBuilder where

-- Pattern with "NOINLINE" pragma, neither builder nor matcher should be inlined
pattern NonInlinablePattern a = Left a
{-# NOINLINE NonInlinablePattern #-}

testNonBuilder x = NonInlinablePattern (x+1)

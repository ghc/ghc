{-# LANGUAGE PatternSynonyms #-}

module InlinePatSyn_InlineBuilder where

-- Pattern with "INLINE" pragma, both builder and matcher should be inlined
pattern InlinePattern a = [[[[a]]]]
{-# INLINE InlinePattern #-}

testInBuilder x = InlinePattern (x+1)

{-# LANGUAGE PatternSynonyms #-}

module InlinePatSyn_InlineBuilder where

-- Pattern with "INLINE" pragma, both builder and matcher should be inlined
pattern InlineablePattern a = [[[[a]]]]
{-# INLINE InlineablePattern #-}

testInBuilder x = InlineablePattern (x+1)

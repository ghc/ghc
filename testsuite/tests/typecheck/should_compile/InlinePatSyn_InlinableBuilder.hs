{-# LANGUAGE PatternSynonyms #-}

module InlinePatSyn_InlinableBuilder where

-- Pattern with "INLINE" pragma, both builder and matcher should be inlined
pattern InlinablePattern a = [[[[a]]]]
{-# INLINABLE InlinablePattern #-}

testInBuilder x = InlinablePattern (x+1)

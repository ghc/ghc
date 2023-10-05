{-# LANGUAGE PatternSynonyms, LinearTypes #-}
module LinearPatSyn2 where

-- Should be rejected, #18806
pattern J :: x %1 -> Maybe x
pattern J a = Just a

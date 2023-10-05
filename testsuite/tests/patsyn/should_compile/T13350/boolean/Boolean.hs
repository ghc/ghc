{-# LANGUAGE PatternSynonyms #-}
module Boolean (Boolean, pattern F, pattern T) where

newtype Boolean = Boolean Int

pattern F, T :: Boolean
pattern F = Boolean 0
pattern T = Boolean 1
{-# COMPLETE F, T #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Boolean (Boolean, data F, data T) where

newtype Boolean = Boolean Int

pattern F, T :: Boolean
pattern F = Boolean 0
pattern T = Boolean 1
{-# COMPLETE F, T #-}

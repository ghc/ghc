{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Bug (pattern P) where

pattern P :: C a => a
pattern P <- (m -> True)

class C a where
  m :: a -> Bool

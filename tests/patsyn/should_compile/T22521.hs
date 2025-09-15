{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ViewPatterns #-}
module Bug (data P) where

pattern P :: C a => a
pattern P <- (m -> True)

class C a where
  m :: a -> Bool

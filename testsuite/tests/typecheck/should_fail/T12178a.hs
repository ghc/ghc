{-# LANGUAGE PatternSynonyms #-}

module T12178a where

-- Trying to inline a data constructor fails
data L a = C a (L a) | T
{-# INLINE C #-}

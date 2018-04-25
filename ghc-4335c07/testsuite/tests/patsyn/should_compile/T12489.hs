{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module T12489 where
pattern P :: a -> b
pattern P a <- (undefined -> a)

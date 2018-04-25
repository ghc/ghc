{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module T11367 where

pattern A :: Int -> String
pattern A n <- (read -> n) where
        A 0 = "hi"
        A 1 = "bye"

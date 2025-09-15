{-# language PatternSynonyms #-}

module PatSynUnboundVar where

pattern P :: Int -> (Int, Int)
pattern P a = (a, b)

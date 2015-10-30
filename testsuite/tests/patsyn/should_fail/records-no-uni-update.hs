{-# LANGUAGE PatternSynonyms #-}
module RecordPats where

-- No updates
pattern Uni{a,b} <- (a, b)

foo = ("a","b") { a = "b" }

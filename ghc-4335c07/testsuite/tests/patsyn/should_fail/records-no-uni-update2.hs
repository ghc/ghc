{-# LANGUAGE PatternSynonyms #-}
module RecordPats where

-- No updates
pattern Uni{a} <- Just a

qux = a (Just True)

qux2 (Uni b) = b

foo = Uni { a = "b" }

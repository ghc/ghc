{-# LANGUAGE NoFieldSelectors, GHC2021 #-}
module T18999_NoDisambiguateRecordFields where

data Foo = Foo { not :: Int }

foo = Foo { not = 1 } -- ambiguous without DisambiguateRecordFields
x = not -- unambiguous because of NoFieldSelectors
y = foo { not = 2 } -- ambiguous without DisambiguateRecordFields

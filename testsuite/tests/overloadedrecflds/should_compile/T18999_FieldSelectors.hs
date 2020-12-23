{-# LANGUAGE DisambiguateRecordFields #-}
module T18999_FieldSelectors where

data Foo = Foo { not :: Int }

foo = Foo { not = 1 }
y = foo { not = 2 }

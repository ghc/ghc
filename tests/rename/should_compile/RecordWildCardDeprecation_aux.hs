module RecordWildCardDeprecation_aux(Foo(Foo, x, z), {-# DEPRECATED "export depr" #-} Foo(y)) where

data Foo = Foo { x :: Int, y :: Bool, z :: Char }

{-# DEPRECATED x "name depr" #-}
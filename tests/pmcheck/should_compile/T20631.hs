{-# LANGUAGE UnliftedDatatypes  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lib where
import Data.Kind (Type)
import GHC.Exts (UnliftedType)

type Foo :: UnliftedType -> Type -> Type
data Foo a b =
      Bar a -- no need for strictness annotation
    | Baz b

type MyVoid :: UnliftedType
data MyVoid

v :: Foo MyVoid Char
v = Baz 'c'

main :: IO ()
main = case v of
    -- The Baz case is impossible
    -- MyVoid has no values, and it can't
    -- be undefined because it's unlifted.
    Baz c -> putChar c

